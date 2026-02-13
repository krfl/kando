use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

use super::theme::Theme;
use crate::app::Mode;
use crate::input::keymap;

/// Render the minor-mode hint popup (shown for g, space, z modes).
pub fn render_hint_popup(f: &mut Frame, area: Rect, mode: &Mode) {
    let bindings = keymap::mode_bindings(mode);
    if bindings.is_empty() {
        return;
    }

    // Calculate popup dimensions
    let max_key_len = bindings.iter().map(|b| b.key.len()).max().unwrap_or(0);
    let max_desc_len = bindings.iter().map(|b| b.description.len()).max().unwrap_or(0);
    let popup_width = (max_key_len + max_desc_len + 7).min(area.width as usize) as u16;
    let popup_height = (bindings.len() as u16 + 2).min(area.height);

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

    for (i, binding) in bindings.iter().enumerate() {
        if i >= inner.height as usize {
            break;
        }
        let line = Line::from(vec![
            Span::raw(" "),
            Span::styled(
                format!("{:>width$}", binding.key, width = max_key_len),
                Style::default()
                    .fg(Theme::HINT_KEY)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw("  "),
            Span::styled(binding.description, Style::default().fg(Theme::HINT_DESC)),
        ]);
        f.render_widget(
            Paragraph::new(line),
            Rect::new(inner.x, inner.y + i as u16, inner.width, 1),
        );
    }
}

/// Render the command palette popup (shown during command mode).
///
/// Displays a filtered list of `(name, description)` pairs, with the row whose
/// name matches `selected_name` highlighted. The `query` is the current token
/// used for match highlighting.
pub fn render_command_palette(
    f: &mut Frame,
    area: Rect,
    title: &str,
    items: &[(String, String)],
    selected_name: Option<&str>,
    query: &str,
) {
    if items.is_empty() {
        return;
    }

    let max_name_len = items.iter().map(|(n, _)| n.len()).max().unwrap_or(0);
    let max_desc_len = items.iter().map(|(_, d)| d.len()).max().unwrap_or(0);
    let has_descs = max_desc_len > 0;

    // Width: " name  description " + borders + padding
    let content_width = if has_descs {
        max_name_len + max_desc_len + 5 // "  name  desc  "
    } else {
        max_name_len + 4 // "  name  "
    };
    let popup_width = (content_width as u16 + 2).max(title.len() as u16 + 4).min(area.width.saturating_sub(4));
    let max_visible = 10u16;
    let popup_height = (items.len() as u16 + 2).min(max_visible + 2).min(area.height.saturating_sub(2));

    let x = area.x + area.width.saturating_sub(popup_width);
    let y = area.y + area.height.saturating_sub(popup_height);
    let popup_area = Rect::new(x, y, popup_width, popup_height);

    f.render_widget(Clear, popup_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::DIM))
        .title(Span::styled(
            format!(" {title} "),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ));

    let inner = block.inner(popup_area);
    f.render_widget(block, popup_area);

    let query_lower = query.to_lowercase();

    for (i, (name, desc)) in items.iter().enumerate() {
        if i >= inner.height as usize {
            break;
        }
        let is_selected = selected_name == Some(name.as_str());

        // Build name spans with match highlighting
        let name_spans = if !query_lower.is_empty() {
            highlight_match(name, &query_lower, is_selected)
        } else {
            let style = if is_selected {
                Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD | Modifier::REVERSED)
            } else {
                Style::default().fg(Theme::FG)
            };
            vec![Span::styled(
                format!("{:<width$}", name, width = max_name_len),
                style,
            )]
        };

        let mut spans = vec![Span::raw(if is_selected { "▶ " } else { "  " })];
        spans.extend(name_spans);

        // Pad name to alignment
        if !query_lower.is_empty() {
            let name_display_len = name.len();
            if name_display_len < max_name_len {
                let pad_style = if is_selected {
                    Style::default().add_modifier(Modifier::REVERSED)
                } else {
                    Style::default()
                };
                spans.push(Span::styled(
                    " ".repeat(max_name_len - name_display_len),
                    pad_style,
                ));
            }
        }

        if has_descs && !desc.is_empty() {
            spans.push(Span::raw("  "));
            spans.push(Span::styled(desc.clone(), Style::default().fg(Theme::DIM)));
        }

        let line = Line::from(spans);
        f.render_widget(
            Paragraph::new(line),
            Rect::new(inner.x, inner.y + i as u16, inner.width, 1),
        );
    }
}

/// Highlight substring matches in `text` for the given `query` (lowercased).
fn highlight_match(text: &str, query_lower: &str, is_selected: bool) -> Vec<Span<'static>> {
    let text_lower = text.to_lowercase();
    let base_mod = if is_selected {
        Modifier::REVERSED
    } else {
        Modifier::empty()
    };

    if let Some(start) = text_lower.find(query_lower) {
        let end = start + query_lower.len();
        let mut spans = Vec::new();
        if start > 0 {
            spans.push(Span::styled(
                text[..start].to_string(),
                Style::default().fg(Theme::FG).add_modifier(base_mod),
            ));
        }
        spans.push(Span::styled(
            text[start..end].to_string(),
            Style::default()
                .fg(Theme::HINT_KEY)
                .add_modifier(Modifier::BOLD | base_mod),
        ));
        if end < text.len() {
            spans.push(Span::styled(
                text[end..].to_string(),
                Style::default().fg(Theme::FG).add_modifier(base_mod),
            ));
        }
        spans
    } else {
        vec![Span::styled(
            text.to_string(),
            Style::default().fg(Theme::FG).add_modifier(base_mod),
        )]
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
