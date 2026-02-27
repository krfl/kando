use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::input::keymap;

pub fn render_tutorial(f: &mut Frame, area: Rect, scroll: &mut u16) {
    let panel_area = super::overlay_rect(area);

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

    let mut lines = vec![
        Line::from(Span::styled(
            "j/k scroll  |  Esc close",
            Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
    ];
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

    let max_scroll = (lines.len() as u16).saturating_sub(inner.height);
    *scroll = (*scroll).min(max_scroll);

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((*scroll, 0));
    f.render_widget(paragraph, inner);
}
