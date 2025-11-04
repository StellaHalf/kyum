use std::io;

use ratatui::{
    Frame,
    buffer::Buffer,
    crossterm::event::{self, Event, KeyCode, KeyEventKind},
    layout::Rect,
    prelude::Color,
    style::Stylize,
    widgets::{Paragraph, Widget},
};

use crate::state::{Bar, CommandResult, State};

const SELECT_COLOR: Color = Color::Rgb(75, 0, 255);
const CURSOR_COLOR: Color = Color::Rgb(255, 0, 75);

impl State {
    fn pixel(&'_ self, x: u32, y: u32) -> Option<(Paragraph<'_>, Paragraph<'_>)> {
        if x < self.buffer.buffer.width() + 2 && y < self.buffer.buffer.width() + 2 {
            Some(
                match (
                    x == 0,
                    y == 0,
                    x == self.buffer.buffer.width() + 1,
                    y == self.buffer.buffer.height() + 1,
                ) {
                    (true, true, _, _) | (true, _, _, true) => {
                        (Paragraph::new("|"), Paragraph::new("-"))
                    }
                    (_, true, true, _) | (_, _, true, true) => {
                        (Paragraph::new("-"), Paragraph::new("|"))
                    }
                    (false, true, false, _) | (false, _, false, true) => {
                        (Paragraph::new("-"), Paragraph::new("-"))
                    }
                    (true, false, _, false) => (Paragraph::new("|"), Paragraph::new(" ")),
                    (_, false, true, false) => (Paragraph::new(" "), Paragraph::new("|")),
                    _ => {
                        let i = x - 1;
                        let j = y - 1;
                        let select = self.buffer.select.contains(&(i, j));
                        let cursor = self.cursorx == i && self.cursory == j;
                        let [r, g, b, a] = self.buffer.buffer.get_pixel(i, j).0;
                        let (rf, gf, bf, af) = (r as f64, g as f64, b as f64, a as f64 / 255.0);
                        let maf = 1.0 - af;
                        let text = if cursor {
                            "<>"
                        } else if select {
                            "\\\\"
                        } else {
                            "  "
                        };
                        let fg = if select {
                            SELECT_COLOR
                        } else if cursor {
                            CURSOR_COLOR
                        } else {
                            Color::Rgb(0, 0, 0)
                        };
                        let (bg0, bg1) = (
                            Color::Rgb((rf * af) as u8, (bf * af) as u8, (gf * af) as u8),
                            Color::Rgb(
                                (rf * af + 20.0 * maf) as u8,
                                (bf * af + 20.0 * maf) as u8,
                                (gf * af + 20.0 * maf) as u8,
                            ),
                        );
                        let (bgl, bgr) = if (j % 2) == 0 { (bg0, bg1) } else { (bg1, bg0) };
                        (
                            Paragraph::new(&text[0..1]).fg(fg).bg(bgl),
                            Paragraph::new(&text[1..2]).fg(fg).bg(bgr),
                        )
                    }
                },
            )
        } else {
            None
        }
    }

    fn render_map(&self, area: Rect, buf: &mut Buffer) {
        let width = area.width / 2;
        for x in 0..width.min(self.buffer.buffer.width() as u16 + 2) {
            for y in 0..area.height.min(self.buffer.buffer.height() as u16 + 2) {
                if let Some(pixel) = self.pixel(
                    self.cursorx.saturating_sub((width - 3) as u32) + x as u32,
                    self.cursory.saturating_sub(area.height as u32 - 3) + y as u32,
                ) {
                    pixel
                        .0
                        .render(Rect::new(area.x + 2 * x, area.y + y, 1, 1), buf);
                    pixel
                        .1
                        .render(Rect::new(area.x + 2 * x + 1, area.y + y, 1, 1), buf);
                }
            }
        }
    }

    pub(crate) fn draw(&self, frame: &mut Frame) {
        let area = frame.area();
        let buf = frame.buffer_mut();
        let map_area = Rect::new(0, 0, area.width, area.height - 2);
        self.render_map(map_area, buf);

        let bar_area = Rect::new(0, area.height.max(1) - 1, area.width, 1);
        let info_area = Rect::new(0, area.height.max(2) - 2, area.width, 1);
        Paragraph::new(self.info_bar()).render(info_area, buf);
        match &self.bar {
            Bar::Input(input) => {
                Paragraph::new(":".to_owned() + input.text().as_ref()).render(bar_area, buf);
                frame.set_cursor_position((input.cursor() as u16 + 1, bar_area.y));
            }
            Bar::Err(err) => {
                Paragraph::new(err.as_str())
                    .fg(Color::Red)
                    .render(bar_area, buf);
            }
            Bar::Ok(message) => {
                Paragraph::new(message.as_str()).render(bar_area, buf);
            }
            Bar::Closed => (),
        }
    }

    pub(crate) fn handle_events(&mut self) -> Result<(), io::Error> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.receive_key(key_event.code)
            }
            _ => (),
        };
        Ok(())
    }

    fn receive_key(&mut self, code: KeyCode) {
        match &mut self.bar {
            Bar::Input(input) => match &code {
                KeyCode::Right => input.move_right(),
                KeyCode::Left => input.move_left(),
                KeyCode::Char(c) => input.write(*c),
                KeyCode::Backspace => input.backspace(),
                KeyCode::Delete => input.delete(),
                KeyCode::Esc => self.bar = Bar::Closed,
                KeyCode::Enter => {
                    let text = input.text();
                    match self.parse_command(&text) {
                        CommandResult::Err(err) => self.bar = Bar::Err(err),
                        CommandResult::Ok(message) => self.bar = Bar::Ok(message),
                        CommandResult::None => self.bar = Bar::Closed,
                    }
                }
                _ => (),
            },
            Bar::Closed => self.receive_key_closed(code),
            Bar::Err(_) | Bar::Ok(_) => {
                self.bar = Bar::Closed;
                self.receive_key_closed(code);
            }
        }
    }
}
