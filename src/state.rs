use std::{
    collections::{HashMap, HashSet},
    fs::read,
    io,
};

use image::{ColorType, ImageBuffer, Rgba, load_from_memory, save_buffer};
use itertools::Itertools;
use ratatui::crossterm::event::KeyCode;

use crate::bar::Input;

#[derive(PartialEq, Eq)]
pub(crate) enum Bar {
    Closed,
    Input(Input),
    Err(String),
    Ok(String),
}

pub(crate) enum CommandResult {
    None,
    Err(String),
    Ok(String),
}

#[derive(PartialEq, Eq)]
pub(crate) enum Pen {
    Up,
    Down,
}

pub(crate) enum Brush {
    Add,
    Subtract,
    Color(Rgba<u8>),
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct Buffer {
    pub(crate) buffer: ImageBuffer<Rgba<u8>, Vec<u8>>,
    pub(crate) select: HashSet<(u32, u32)>,
}

pub(crate) struct Clipboard {
    pub(crate) content: HashMap<(i32, i32), Rgba<u8>>,
    pub(crate) offsetx: u32,
    pub(crate) offsety: u32,
}

pub(crate) struct State {
    pub(crate) buffer: Buffer,
    pub(crate) argument: usize,
    pub(crate) bar: Bar,
    pub(crate) brush: Brush,
    pub(crate) clipboard: Option<Clipboard>,
    pub(crate) cursorx: u32,
    pub(crate) cursory: u32,
    pub(crate) exit: bool,
    pub(crate) last_saved: Option<ImageBuffer<Rgba<u8>, Vec<u8>>>,
    pub(crate) path: Option<String>,
    pub(crate) pen: Pen,
    redo_stack: Vec<Buffer>,
    undo_stack: Vec<Buffer>,
}

struct Command {
    name: &'static str,
    aliases: &'static [&'static str],
    argsmin: usize,
    argsmax: usize,
    function: fn(&mut State, &[&str]) -> CommandResult,
}

impl Command {
    const fn new(
        name: &'static str,
        aliases: &'static [&'static str],
        argsmin: usize,
        argsmax: usize,
        function: fn(&mut State, &[&str]) -> CommandResult,
    ) -> Self {
        Command {
            name,
            aliases,
            argsmin,
            argsmax,
            function,
        }
    }
}

enum Direction {
    Left,
    Down,
    Up,
    Right,
}

fn parse_direction(arg: &str) -> Result<Direction, String> {
    match arg.to_lowercase().as_str() {
        "left" | "h" => Ok(Direction::Left),
        "down" | "j" => Ok(Direction::Down),
        "up" | "k" => Ok(Direction::Up),
        "right" | "l" => Ok(Direction::Right),
        _ => Err(format!(
            "Parse error: {} is not a direction, options are Left, Down, Up, Right.",
            arg
        )),
    }
}

fn parse_u32(arg: &str) -> Result<u32, String> {
    arg.parse()
        .map_err(|_| format!("Parse error: {} is not an integer.", arg))
}

fn parse_color(color: &str) -> Result<Rgba<u8>, String> {
    Ok(match color.to_lowercase().as_str() {
        "white" => Rgba([255, 255, 255, 255]),
        "black" => Rgba([0, 0, 0, 255]),
        "red" => Rgba([255, 0, 0, 255]),
        "green" => Rgba([0, 255, 0, 255]),
        "blue" => Rgba([0, 0, 255, 255]),
        "yellow" => Rgba([255, 255, 0, 255]),
        "magenta" => Rgba([255, 0, 255, 255]),
        "cyan" => Rgba([0, 255, 255, 255]),
        color => {
            let err = format!(
                "Parse error: {} is not a valid color. Valid are color names, RRGGBB or RRGGBBAA.",
                color
            );
            if color.len() == 6 {
                Rgba([
                    u8::from_str_radix(&color[0..2], 16).map_err(|_| err.clone())?,
                    u8::from_str_radix(&color[2..4], 16).map_err(|_| err.clone())?,
                    u8::from_str_radix(&color[4..6], 16).map_err(|_| err.clone())?,
                    255,
                ])
            } else if color.len() == 8 {
                Rgba([
                    u8::from_str_radix(&color[0..2], 16).map_err(|_| err.clone())?,
                    u8::from_str_radix(&color[2..4], 16).map_err(|_| err.clone())?,
                    u8::from_str_radix(&color[4..6], 16).map_err(|_| err.clone())?,
                    u8::from_str_radix(&color[6..8], 16).map_err(|_| err.clone())?,
                ])
            } else {
                Err(err)?
            }
        }
    })
}

impl State {
    pub(crate) fn new() -> Result<State, io::Error> {
        Ok(State {
            buffer: Buffer {
                buffer: ImageBuffer::new(8, 8),
                select: HashSet::new(),
            },
            clipboard: None,
            last_saved: None,
            exit: false,
            path: None,
            pen: Pen::Up,
            cursorx: 0,
            cursory: 0,
            argument: 0,
            brush: Brush::Color(Rgba([255, 255, 255, 255])),
            bar: Bar::Closed,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
        })
    }

    pub(crate) fn modified(&self) -> bool {
        match &self.last_saved {
            None => false,
            Some(saved) => self.buffer.buffer != *saved,
        }
    }

    fn push_undo(&mut self, buffer_clone: Buffer) {
        self.undo_stack.push(buffer_clone);
        self.redo_stack.clear();
    }

    pub(crate) fn append_argument(&mut self, digit: u8) {
        self.argument = (if let Some(p) = self.argument.checked_mul(10) {
            p.checked_add(digit.into())
        } else {
            None
        })
        .unwrap_or(self.argument)
    }

    pub(crate) fn open(&mut self, args: &[&str]) -> CommandResult {
        if self.modified() {
            CommandResult::Err(
                "Unsaved changes (use :o! to discard them and open another file or :w to save them).".to_owned(),
            )
        } else {
            self.open_force(args)
        }
    }

    fn open_force(&mut self, args: &[&str]) -> CommandResult {
        let path = args[0];
        let buffer = match load_from_memory(match &read(path) {
            Ok(bytes) => bytes,
            Err(_) => return CommandResult::Err(format!("Could not open file {}.", path)),
        }) {
            Ok(buffer) => buffer,
            Err(_) => return CommandResult::Err("Could not parse map.".to_owned()),
        }
        .into_rgba8();
        self.buffer.buffer = buffer;
        self.path = Some(path.to_owned());
        CommandResult::Ok(format!("Opened {}.", path))
    }

    pub(crate) fn write(&mut self, args: &[&str]) -> CommandResult {
        if let Some(&path) = args.first() {
            self.path = Some(path.to_owned())
        }
        match &self.path {
            None => CommandResult::Err("No path set (use :w <path>).".to_owned()),
            Some(path) => {
                if let Err(err) = save_buffer(
                    path,
                    &self.buffer.buffer,
                    self.buffer.buffer.width(),
                    self.buffer.buffer.height(),
                    ColorType::Rgba8,
                ) {
                    CommandResult::Err(format!("Format Error: {}.", err))
                } else {
                    self.last_saved = Some(self.buffer.buffer.clone());
                    CommandResult::Ok(format!("Written to {}.", path))
                }
            }
        }
    }

    pub(crate) fn quit(&mut self, _: &[&str]) -> CommandResult {
        if self.modified() {
            CommandResult::Err(
                "Unsaved changes (use :q! to discard them and quit or :wq to save and quit)."
                    .to_owned(),
            )
        } else {
            self.exit = true;
            CommandResult::None
        }
    }

    pub(crate) fn quit_force(&mut self, _: &[&str]) -> CommandResult {
        self.exit = true;
        CommandResult::None
    }

    pub(crate) fn write_quit(&mut self, args: &[&str]) -> CommandResult {
        if let CommandResult::Err(err) = self.write(args) {
            CommandResult::Err(err)
        } else {
            self.quit_force(&[]);
            CommandResult::None
        }
    }
    pub(crate) fn bucket(&mut self, _: &[&str]) -> CommandResult {
        let buffer_clone = self.buffer.clone();
        if let Brush::Color(color) = self.brush {
            for (x, y) in self.buffer.select.clone() {
                self.buffer.buffer.put_pixel(x, y, color);
            }
            if self.buffer != buffer_clone {
                self.push_undo(buffer_clone);
            }
        }
        CommandResult::None
    }

    pub(crate) fn dot(&mut self, _: &[&str]) -> CommandResult {
        let buffer_clone = self.buffer.clone();
        match self.brush {
            Brush::Color(color) => {
                if *self.buffer.buffer.get_pixel(self.cursorx, self.cursory) != color {
                    self.buffer
                        .buffer
                        .put_pixel(self.cursorx, self.cursory, color);
                    self.push_undo(buffer_clone);
                }
            }
            Brush::Add => {
                if self.buffer.select.insert((self.cursory, self.cursorx)) {
                    self.push_undo(buffer_clone);
                }
            }
            Brush::Subtract => {
                if self.buffer.select.remove(&(self.cursory, self.cursorx)) {
                    self.push_undo(buffer_clone);
                }
            }
        }
        CommandResult::None
    }

    pub(crate) fn brush(&mut self, args: &[&str]) -> CommandResult {
        match args[0].to_lowercase().as_str() {
            "add" => {
                self.brush = Brush::Add;
            }
            "subtract" => {
                self.brush = Brush::Subtract;
            }
            color => {
                self.brush = Brush::Color(match parse_color(color) {
                    Ok(color) => color,
                    Err(err) => return CommandResult::Err(err),
                })
            }
        };
        CommandResult::None
    }

    pub(crate) fn pen(&mut self, args: &[&str]) -> CommandResult {
        match args[0].to_lowercase().as_str() {
            "up" => {
                self.pen = Pen::Up;
                CommandResult::None
            }
            "down" => {
                self.pen = Pen::Down;
                CommandResult::None
            }
            _ => CommandResult::Err(format!(
                "Pen mode {} not found, options are up, down.",
                args[0]
            )),
        }
    }

    pub(crate) fn r#move(&mut self, args: &[&str]) -> CommandResult {
        let distance = match args.get(1) {
            None => 1,
            Some(arg) => match parse_u32(arg) {
                Ok(distance) => distance,
                Err(err) => return CommandResult::Err(err),
            },
        };
        self.move_cursor(
            match parse_direction(args[0]) {
                Ok(direction) => direction,
                Err(err) => return CommandResult::Err(err),
            },
            distance,
        );
        CommandResult::None
    }

    fn move_cursor(&mut self, direction: Direction, distance: u32) {
        let (nx, ny, positions) = match direction {
            Direction::Left => {
                let nx = (self.cursorx as i32 - distance as i32).max(0) as u32;
                (
                    nx,
                    self.cursory,
                    (nx..self.cursorx)
                        .map(|x| (x, self.cursory))
                        .collect::<Vec<_>>(),
                )
            }
            Direction::Down => {
                let ny = (self.cursory + distance).min(self.buffer.buffer.height() - 1);
                (
                    self.cursorx,
                    ny,
                    (self.cursory + 1..=ny).map(|y| (self.cursorx, y)).collect(),
                )
            }
            Direction::Up => {
                let ny = (self.cursory as i32 - distance as i32).max(0) as u32;
                (
                    self.cursorx,
                    ny,
                    (ny..self.cursory).map(|y| (self.cursorx, y)).collect(),
                )
            }
            Direction::Right => {
                let nx = (self.cursorx + distance).min(self.buffer.buffer.width() - 1);
                (
                    nx,
                    self.cursory,
                    (self.cursorx + 1..=nx).map(|x| (x, self.cursory)).collect(),
                )
            }
        };
        self.cursorx = nx;
        self.cursory = ny;
        if self.pen == Pen::Down {
            let buffer_clone = self.buffer.clone();
            match self.brush {
                Brush::Color(color) => {
                    for &(i, j) in &positions {
                        self.buffer.buffer.put_pixel(i, j, color);
                    }
                }
                Brush::Add => self.buffer.select.extend(positions),
                Brush::Subtract => {
                    for p in positions {
                        self.buffer.select.remove(&p);
                    }
                }
            }
            if self.buffer != buffer_clone {
                self.push_undo(buffer_clone);
            }
        }
    }

    pub(crate) fn edge(&mut self, args: &[&str]) -> CommandResult {
        match match parse_direction(args[0]) {
            Ok(direction) => direction,
            Err(err) => return CommandResult::Err(err),
        } {
            Direction::Left => self.move_cursor(Direction::Left, self.cursorx),
            Direction::Down => {
                self.move_cursor(Direction::Down, self.buffer.buffer.height() - self.cursory)
            }
            Direction::Up => self.move_cursor(Direction::Up, self.cursory),
            Direction::Right => {
                self.move_cursor(Direction::Right, self.buffer.buffer.width() - self.cursorx)
            }
        };
        CommandResult::None
    }

    fn in_bounds(&self, x: u32, y: u32) -> bool {
        x < self.buffer.buffer.width() && y < self.buffer.buffer.height()
    }

    pub(crate) fn goto(&mut self, args: &[&str]) -> CommandResult {
        let i = match parse_u32(args[0]) {
            Ok(i) => i,
            Err(err) => return CommandResult::Err(err),
        };
        let j = match parse_u32(args[1]) {
            Ok(j) => j,
            Err(err) => return CommandResult::Err(err),
        };
        if self.in_bounds(i, j) {
            self.cursorx = i;
            self.cursory = j;
            CommandResult::None
        } else {
            CommandResult::Err("Out of bounds.".to_owned())
        }
    }

    pub(crate) fn pick(&mut self, _: &[&str]) -> CommandResult {
        self.brush = Brush::Color(*self.buffer.buffer.get_pixel(self.cursorx, self.cursory));
        CommandResult::None
    }

    pub(crate) fn select(&mut self, args: &[&str]) -> CommandResult {
        let buffer_clone = self.buffer.clone();
        match args[0].to_lowercase().as_str() {
            "all" => {
                self.buffer.select = ((0..self.buffer.buffer.width())
                    .cartesian_product(0..self.buffer.buffer.height()))
                .collect();
                CommandResult::None
            }
            "none" => {
                self.buffer.select.clear();
                CommandResult::None
            }
            "invert" => {
                self.buffer.select = ((0..self.buffer.buffer.width())
                    .cartesian_product(0..self.buffer.buffer.height()))
                .filter(|p| !self.buffer.select.contains(p))
                .collect();
                CommandResult::None
            }
            arg => match parse_color(arg) {
                Ok(tile) => {
                    let positions = (0..self.buffer.buffer.width())
                        .cartesian_product(0..self.buffer.buffer.height())
                        .filter(|&(i, j)| *self.buffer.buffer.get_pixel(i, j) == tile);
                    match self.brush {
                        Brush::Add => self.buffer.select.extend(positions),
                        Brush::Subtract => {
                            for p in positions {
                                self.buffer.select.remove(&p);
                            }
                        }
                        _ => self.buffer.select = positions.collect(),
                    }
                    CommandResult::None
                }
                _ => {
                    return CommandResult::Err(
                        "Invalid selection argument, options are all, none, invert and <tile>."
                            .to_owned(),
                    );
                }
            },
        };
        if self.buffer.select != buffer_clone.select {
            self.push_undo(buffer_clone);
        };
        CommandResult::None
    }

    pub(crate) fn draw_shape<F, I>(&mut self, args: &[&str], shape: F) -> CommandResult
    where
        F: FnOnce(&[&str]) -> Result<I, String>,
        I: IntoIterator<Item = (u32, u32)>,
    {
        let positions = match shape(args) {
            Ok(i) => i,
            Err(err) => return CommandResult::Err(err),
        }
        .into_iter()
        .filter(|(x, y)| self.in_bounds(*x, *y))
        .collect::<Vec<_>>();
        let buffer_clone = self.buffer.clone();
        match &self.brush {
            Brush::Add => {
                self.buffer.select.extend(positions);
                if self.buffer.select != buffer_clone.select {
                    self.push_undo(buffer_clone);
                }
            }
            Brush::Subtract => {
                for p in positions {
                    self.buffer.select.remove(&p);
                }
                if self.buffer.select != buffer_clone.select {
                    self.push_undo(buffer_clone);
                }
            }
            Brush::Color(color) => {
                for (x, y) in positions {
                    self.buffer.buffer.put_pixel(x, y, *color);
                }
                if self.buffer != buffer_clone {
                    self.push_undo(buffer_clone);
                }
            }
        }
        CommandResult::None
    }

    pub(crate) fn fuzzy(&mut self, args: &[&str]) -> CommandResult {
        let buffer = self.buffer.buffer.clone();
        let cursorx = self.cursorx;
        let cursory = self.cursory;
        self.draw_shape(args, |args| {
            let mut reached = HashSet::new();
            let mut frontier = HashSet::new();
            let mut new_frontier = HashSet::new();
            frontier.insert((cursorx, cursory));
            let color = buffer.get_pixel(cursorx, cursory);
            let mut i = match args.first() {
                Some(arg) => parse_u32(arg)? as i32,
                None => -1,
            };
            while !frontier.is_empty() && i != 0 {
                i -= 1;
                reached.extend(&frontier);
                new_frontier.clear();
                for (i, j) in frontier.clone() {
                    for (di, dj) in [(-1, 0), (0, -1), (1, 0), (0, 1)] {
                        let ni = i as i32 + di;
                        let nj = j as i32 + dj;
                        if ni >= 0
                            && ni < buffer.width() as i32
                            && nj >= 0
                            && nj < buffer.height() as i32
                            && !reached.contains(&(ni as u32, nj as u32))
                            && buffer.get_pixel(ni as u32, nj as u32) == color
                        {
                            new_frontier.insert((ni as u32, nj as u32));
                        }
                    }
                    frontier.clear();
                    frontier.extend(new_frontier.clone());
                }
            }
            Ok(reached)
        })
    }

    pub(crate) fn r#box(&mut self, args: &[&str]) -> CommandResult {
        self.draw_shape::<_, Vec<_>>(args, |args| {
            let (x0, y0, x1, y1) = (
                parse_u32(args[0])?,
                parse_u32(args[1])?,
                parse_u32(args[2])?,
                parse_u32(args[3])?,
            );
            let fill = if let Some(arg) = args.get(4) {
                if *arg == "fill" || *arg == "true" {
                    true
                } else {
                    return Err("Invalid argument, the only option is fill (optional).".to_owned());
                }
            } else {
                false
            };
            Ok(if fill {
                (y0..=y1).cartesian_product(x0..=x1).collect()
            } else {
                (y0..=y1)
                    .map(|x| (x, x0))
                    .chain((y0..=y1).map(|x| (x, x1)))
                    .chain((x0 + 1..x1).map(|y| (y0, y)))
                    .chain((x0 + 1..x1).map(|y| (y1, y)))
                    .collect()
            })
        })
    }

    //adapted from http://members.chello.at/~easyfilter/bresenham.html
    pub(crate) fn ellipse(&mut self, args: &[&str]) -> CommandResult {
        self.draw_shape(args, |args| {
            let (x0, y0, x1, y1) = (
                parse_u32(args[0])? as i32,
                parse_u32(args[1])? as i32,
                parse_u32(args[2])? as i32,
                parse_u32(args[3])? as i32,
            );
            let fill = if let Some(arg) = args.get(4) {
                if *arg == "fill" || *arg == "true" {
                    true
                } else {
                    return Err("Invalid argument, the only option is fill (optional)".to_owned());
                }
            } else {
                false
            };
            let mut positions = Vec::new();
            let a = (x1 - x0).abs();
            let b = (y1 - y0).abs();
            let bp = b & 1;
            let mut dx = 4 * (1 - a) * b * b;
            let mut dy = 4 * (bp + 1) * a * a;
            let mut e = dx + dy + bp * a * a;
            let (mut x0, mut x1) = if x0 > x1 { (x1, x0) } else { (x0, x1) };
            let mut y0 = if y1 > y0 { y0 } else { y1 };
            y0 += (b + 1) / 2;
            let mut y1 = y0 - bp;
            let a = 8 * a * a;
            let mut e2;
            while x0 <= x1 {
                let x0u = x0 as u32;
                let x1u = x1 as u32;
                let y0u = y0 as u32;
                let y1u = y1 as u32;
                if fill {
                    positions.extend(
                        (x0u..=x1u)
                            .map(|x| (x, y0u))
                            .chain((x0u..=x1u).map(|x| (x, y1u))),
                    )
                } else {
                    positions.extend_from_slice(&[(x1u, y0u), (x0u, y0u), (x0u, y1u), (x1u, y1u)])
                };
                e2 = 2 * e;
                if e2 <= dy {
                    y0 += 1;
                    y1 -= 1;
                    dy += a;
                    e += dy;
                }
                if e2 >= dx || 2 * e > dy {
                    x0 += 1;
                    x1 -= 1;
                    dx += 8 * b * b;
                    e += dx;
                }
            }
            Ok(positions)
        })
    }

    pub(crate) fn create(&mut self, args: &[&str]) -> CommandResult {
        let y = match parse_u32(args[1]) {
            Ok(y) => y,
            Err(err) => return CommandResult::Err(err),
        };
        let x = match parse_u32(args[0]) {
            Ok(x) => x,
            Err(err) => return CommandResult::Err(err),
        };
        let new_map = Buffer {
            buffer: ImageBuffer::new(x, y),
            select: HashSet::new(),
        };
        if self.buffer != new_map {
            self.push_undo(self.buffer.clone());
            self.buffer = new_map;
        }
        self.reset_cursor();
        CommandResult::Ok(format!("Created empty {}x{} map.", x, y))
    }

    pub(crate) fn reset_cursor(&mut self) {
        if !self.in_bounds(self.cursorx, self.cursory) {
            self.cursorx = 0;
            self.cursory = 0;
        }
    }

    pub(crate) fn undo(&mut self, _: &[&str]) -> CommandResult {
        match self.undo_stack.pop() {
            None => CommandResult::Err("Undo stack is empty.".to_owned()),
            Some(map) => {
                self.redo_stack.push(self.buffer.clone());
                self.buffer = map;
                self.reset_cursor();
                CommandResult::None
            }
        }
    }

    pub(crate) fn redo(&mut self, _: &[&str]) -> CommandResult {
        match self.redo_stack.pop() {
            None => CommandResult::Err("Redo stack is empty".to_owned()),
            Some(map) => {
                self.undo_stack.push(self.buffer.clone());
                self.buffer = map;
                self.reset_cursor();
                CommandResult::None
            }
        }
    }

    pub(crate) fn parse_command(&mut self, text: &str) -> CommandResult {
        if let Some((name, args)) = text
            .split(" ")
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .split_first()
        {
            match COMMANDS
                .iter()
                .find(|c| c.name == *name || c.aliases.contains(name))
            {
                None => CommandResult::Err(format!("Command {} not found.", name)),
                Some(command) => {
                    if args.len() >= command.argsmin && args.len() <= command.argsmax {
                        (command.function)(self, args)
                    } else {
                        CommandResult::Err(format!(
                            "Incorrect number of arguments for {}: expected {}, found {}.",
                            command.name,
                            if command.argsmin == command.argsmax {
                                command.argsmin.to_string()
                            } else {
                                format!("{}-{}", command.argsmin, command.argsmax)
                            },
                            args.len()
                        ))
                    }
                }
            }
        } else {
            CommandResult::None
        }
    }

    pub(crate) fn info_bar(&self) -> String {
        format!(
            "Path: {}{}, Pen: {}, Brush: {}, Cursor: ({},{}), Argument: {}",
            self.path.clone().unwrap_or("[-]".to_owned()),
            if self.modified() { "(*)" } else { "" },
            match self.pen {
                Pen::Up => "Up",
                Pen::Down => "Down",
            },
            match self.brush {
                Brush::Add => "add".to_owned(),
                Brush::Subtract => "subtract".to_owned(),
                Brush::Color(color) => format!(
                    "{:02x}{:02x}{:02x}{:02x}",
                    color.0[0], color.0[1], color.0[2], color.0[3]
                ),
            },
            self.cursorx,
            self.cursory,
            if self.argument > 0 {
                self.argument.to_string()
            } else {
                "".to_owned()
            }
        )
    }

    pub(crate) fn copy(&mut self, _: &[&str]) -> CommandResult {
        self.clipboard = Some(Clipboard {
            content: self
                .buffer
                .select
                .iter()
                .map(|(i, j)| {
                    (
                        (*i as i32, *j as i32),
                        *self.buffer.buffer.get_pixel(*i, *j),
                    )
                })
                .collect(),
            offsetx: self.cursorx,
            offsety: self.cursory,
        });
        CommandResult::Ok(format!(
            "Copied {} tiles to clipboard.",
            self.buffer.select.len()
        ))
    }

    pub(crate) fn paste(&mut self, _: &[&str]) -> CommandResult {
        let buffer_clone = self.buffer.clone();
        if let Some(clipboard) = &self.clipboard {
            for (i, j, color) in clipboard
                .content
                .iter()
                .map(|((i, j), color)| {
                    (
                        (*i + self.cursory as i32 - (clipboard.offsety as i32)) as u32,
                        (*j + self.cursorx as i32 - (clipboard.offsetx as i32)) as u32,
                        color,
                    )
                })
                .filter(|(i, j, _)| self.in_bounds(*i, *j))
                .collect_vec()
            {
                self.buffer.buffer.put_pixel(i, j, *color);
            }
            if self.buffer.buffer != buffer_clone.buffer {
                self.push_undo(buffer_clone);
            }
            CommandResult::None
        } else {
            CommandResult::Err("Clipboard is empty".to_owned())
        }
    }

    pub(crate) fn clipboard(&mut self, args: &[&str]) -> CommandResult {
        if let Some(clipboard) = &mut self.clipboard {
            match args[0].to_lowercase().as_str() {
                "rotate anticlockwise" | "a" => {
                    clipboard.content = clipboard
                        .content
                        .iter()
                        .map(|((i, j), tile)| {
                            (
                                (
                                    *j - clipboard.offsetx as i32 + clipboard.offsety as i32,
                                    -i + clipboard.offsetx as i32 + clipboard.offsety as i32,
                                ),
                                *tile,
                            )
                        })
                        .collect();
                    CommandResult::Ok("Rotated the clipboard anticlockwise".to_owned())
                }
                "rotate clockwise" | "c" => {
                    clipboard.content = clipboard
                        .content
                        .iter()
                        .map(|((i, j), tile)| {
                            (
                                (
                                    -j + clipboard.offsetx as i32 + clipboard.offsety as i32,
                                    *i - clipboard.offsetx as i32 + clipboard.offsety as i32,
                                ),
                                *tile,
                            )
                        })
                        .collect();
                    CommandResult::Ok("Rotated the clipboard clockwise".to_owned())
                }
                "reflect horizontal" | "h" => {
                    clipboard.content = clipboard
                        .content
                        .iter()
                        .map(|((i, j), tile)| ((*i, -j + 2 * clipboard.offsetx as i32), *tile))
                        .collect();
                    CommandResult::Ok("Reflected the clipboard horizontally".to_owned())
                }
                "reflect vertical" | "v" => {
                    clipboard.content = clipboard
                        .content
                        .iter()
                        .map(|((i, j), tile)| ((-i + 2 * clipboard.offsety as i32, *j), *tile))
                        .collect();
                    CommandResult::Ok("Reflected the clipboard vertically.".to_owned())
                }
                _ => CommandResult::Err(
                    "Invalid options, the only options are rotate|reflect horizontal|vertical."
                        .to_owned(),
                ),
            }
        } else {
            CommandResult::None
        }
    }

    fn move_with(&mut self, direction: Direction) -> CommandResult {
        self.move_cursor(direction, self.argument.max(1) as u32);
        self.argument = 0;
        CommandResult::None
    }
    pub(crate) fn receive_key_closed(&mut self, code: KeyCode) {
        match &code {
            KeyCode::Char(':') => {
                self.bar = Bar::Input(Input::empty());
            }
            KeyCode::Char('h') | KeyCode::Left => {
                self.move_with(Direction::Left);
            }
            KeyCode::Char('j') | KeyCode::Down => {
                self.move_with(Direction::Down);
            }
            KeyCode::Char('k') | KeyCode::Up => {
                self.move_with(Direction::Up);
            }
            KeyCode::Char('l') | KeyCode::Right => {
                self.move_with(Direction::Right);
            }
            KeyCode::Char('H') => {
                self.edge(&["left"]);
            }
            KeyCode::Char('J') => {
                self.edge(&["down"]);
            }
            KeyCode::Char('K') => {
                self.edge(&["up"]);
            }
            KeyCode::Char('L') => {
                self.edge(&["right"]);
            }
            KeyCode::Char('d') => {
                self.dot(&[]);
            }
            KeyCode::Char('a') => {
                self.brush(&["add"]);
            }
            KeyCode::Char('s') => {
                self.brush(&["subtract"]);
            }
            KeyCode::Char('i') => {
                self.pen(&["down"]);
            }
            KeyCode::Char('I') => {
                self.pen(&["up"]);
            }
            KeyCode::Char('A') => {
                self.select(&["all"]);
            }
            KeyCode::Char('S') => {
                self.select(&["none"]);
            }
            KeyCode::Char('F') => {
                self.select(&["invert"]);
            }
            KeyCode::Esc => {
                self.argument = 0;
            }
            KeyCode::Char('f') => {
                self.bucket(&[]);
            }
            KeyCode::Char('e') => {
                self.pick(&[]);
            }
            KeyCode::Char('E') => {
                self.brush(&["00000000"]);
            }
            KeyCode::Char('u') => {
                self.undo(&[]);
            }
            KeyCode::Char('U') => {
                self.redo(&[]);
            }
            KeyCode::Char('o') => {
                self.copy(&[]);
            }
            KeyCode::Char('O') => {
                self.paste(&[]);
            }
            KeyCode::Char(c) => {
                if let Some(i) = c.to_digit(10) {
                    self.append_argument(i as u8)
                };
            }
            _ => {}
        };
    }
}

const COMMANDS: [Command; 24] = [
    Command::new("open", &["o"], 1, 1, State::open),
    Command::new("open!", &["o!"], 1, 1, State::open_force),
    Command::new("write", &["w"], 0, 1, State::write),
    Command::new("quit", &["q"], 0, 0, State::quit),
    Command::new("quit!", &["q!"], 0, 0, State::quit_force),
    Command::new("write-quit", &["wq"], 0, 1, State::write_quit),
    Command::new("brush", &["tile", "t"], 1, 1, State::brush),
    Command::new("dot", &[], 0, 0, State::dot),
    Command::new("bucket", &[], 0, 0, State::bucket),
    Command::new("move", &[], 1, 2, State::r#move),
    Command::new("pick", &[], 0, 0, State::pick),
    Command::new("pen", &[], 1, 1, State::pen),
    Command::new("edge", &[], 1, 1, State::edge),
    Command::new("goto", &["g"], 2, 2, State::goto),
    Command::new("select", &["s"], 1, 1, State::select),
    Command::new("undo", &[], 0, 0, State::undo),
    Command::new("redo", &[], 0, 0, State::redo),
    Command::new("create", &["n"], 2, 2, State::create),
    Command::new("box", &["b"], 4, 5, State::r#box),
    Command::new("ellipse", &["e"], 4, 5, State::ellipse),
    Command::new("fuzzy", &["f"], 0, 1, State::fuzzy),
    Command::new("copy", &[], 0, 0, State::copy),
    Command::new("paste", &[], 0, 0, State::paste),
    Command::new("clipboard", &["c"], 1, 1, State::clipboard),
];
