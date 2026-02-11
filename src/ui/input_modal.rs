use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

use super::theme::Theme;
use crate::app::Mode;
use crate::input::keymap::mode_hints;

/// Render the minor-mode hint popup (shown for g, space, z modes).
pub fn render_hint_popup(f: &mut Frame, area: Rect, mode: &Mode) {
    let hints = mode_hints(mode);
    if hints.is_empty() {
        return;
    }

    // Calculate popup dimensions
    let max_key_len = hints.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
    let max_desc_len = hints.iter().map(|(_, d)| d.len()).max().unwrap_or(0);
    let popup_width = (max_key_len + max_desc_len + 7).min(area.width as usize) as u16;
    let popup_height = (hints.len() as u16 + 2).min(area.height);

    let x = area.x + area.width.saturating_sub(popup_width);
    let y = area.y + area.height.saturating_sub(popup_height);
    let popup_area = Rect::new(x, y, popup_width, popup_height);

    f.render_widget(Clear, popup_area);

    let mode_name = match mode {
        Mode::Goto => "goto",
        Mode::Space => "commands",
        Mode::View => "view",
        _ => "",
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            format!(" {mode_name} "),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ));

    let inner = block.inner(popup_area);
    f.render_widget(block, popup_area);

    for (i, (key, desc)) in hints.iter().enumerate() {
        if i >= inner.height as usize {
            break;
        }
        let line = Line::from(vec![
            Span::raw(" "),
            Span::styled(
                format!("{key:>width$}", width = max_key_len),
                Style::default()
                    .fg(Theme::HINT_KEY)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw("  "),
            Span::styled(*desc, Style::default().fg(Theme::HINT_DESC)),
        ]);
        f.render_widget(
            Paragraph::new(line),
            Rect::new(inner.x, inner.y + i as u16, inner.width, 1),
        );
    }
}

/// Render a generic picker popup.
pub fn render_picker(
    f: &mut Frame,
    area: Rect,
    title: &str,
    items: &[(String, bool)],
    selected: usize,
) {
    let max_label_len = items.iter().map(|(l, _)| l.len()).max().unwrap_or(0);
    let popup_width = ((max_label_len + 6) as u16).max(20).min(area.width - 4);
    let popup_height = (items.len() as u16 + 2).min(area.height - 4).max(3);
    let x = area.x + area.width.saturating_sub(popup_width);
    let y = area.y + area.height.saturating_sub(popup_height);
    let popup_area = Rect::new(x, y, popup_width, popup_height);

    f.render_widget(Clear, popup_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            format!(" {title} "),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ));

    let inner = block.inner(popup_area);
    f.render_widget(block, popup_area);

    // Check if any item has is_active=true (used for toggle markers)
    let has_toggles = items.iter().any(|(_, active)| *active);

    for (i, (label, is_active)) in items.iter().enumerate() {
        if i >= inner.height as usize {
            break;
        }
        let is_selected = i == selected;
        let sel_mod = if is_selected {
            Modifier::BOLD | Modifier::REVERSED
        } else {
            Modifier::empty()
        };

        let mut spans = Vec::new();

        // Show toggle marker if this picker uses toggles
        if has_toggles {
            let marker = if *is_active { "x " } else { "  " };
            spans.push(Span::styled(
                marker,
                Style::default().fg(if *is_active { Theme::FG } else { Theme::DIM }),
            ));
        } else {
            spans.push(Span::raw("  "));
        }

        spans.push(Span::styled(
            label.clone(),
            Style::default().fg(Theme::FG).add_modifier(sel_mod),
        ));

        let line = Line::from(spans);
        f.render_widget(
            Paragraph::new(line),
            Rect::new(inner.x, inner.y + i as u16, inner.width, 1),
        );
    }
}
