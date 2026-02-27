use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::input::keymap;

pub fn render_help(f: &mut Frame, area: Rect, scroll: u16) {
    let panel_area = super::centered_rect(area, 70, 85, 60, 24);

    f.render_widget(Clear, panel_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            " Kando Help ",
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

    let key = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();
    let heading = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED);

    let mut lines = Vec::new();
    for group in keymap::HELP_GROUPS {
        lines.push(Line::from(Span::styled(group.name, heading)));
        for b in group.bindings {
            lines.push(Line::from(vec![
                Span::styled(format!("  {:<14}", b.key), key),
                Span::styled(b.description, dim),
            ]));
        }
        lines.push(Line::from(""));
    }
    lines.push(Line::from(Span::styled(
        "J/K scroll  |  Esc close",
        Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
    )));

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));
    f.render_widget(paragraph, inner);
}
