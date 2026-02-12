use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::input::keymap;

pub fn render_tutorial(f: &mut Frame, area: Rect) {
    let panel_area = super::centered_rect(area, 50, 80, 44, 20);

    f.render_widget(Clear, panel_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            " Welcome to Kando! ",
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ))
        .padding(Padding::new(2, 2, 1, 1));

    let inner = block.inner(panel_area);
    f.render_widget(block, panel_area);

    if inner.height == 0 {
        return;
    }

    let key_style = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();
    let heading = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);

    let mut lines = Vec::new();
    for group in keymap::TUTORIAL_GROUPS {
        lines.push(Line::from(Span::styled(group.name, heading)));
        for b in group.bindings.iter().filter(|b| b.tutorial) {
            lines.push(Line::from(vec![
                Span::styled(format!("  {:<14}", b.key), key_style),
                Span::styled(b.description, dim),
            ]));
        }
        lines.push(Line::from(""));
    }
    lines.push(Line::from(Span::styled(
        "Press any key to start",
        Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
    )));

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });
    f.render_widget(paragraph, inner);
}
