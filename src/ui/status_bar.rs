use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;
use unicode_width::UnicodeWidthStr;

use super::theme::{self, Theme};
use crate::app::{AppState, Mode, NotificationLevel};
use crate::board::Board;

pub fn render_status_bar(f: &mut Frame, area: Rect, state: &AppState, board: &Board) {
    // Full-line modes: Filter, Input, Confirm, Command — take over entire bar
    if let Some(line) = render_full_line_mode(state, board) {
        let paragraph = Paragraph::new(line).style(Theme::status_style());
        f.render_widget(paragraph, area);
        return;
    }

    // Three-zone layout for all other modes
    let left = build_left_zone(state, &board.name);
    let right = build_right_zone(state, board);

    let left_width: usize = left.iter().map(|s| s.content.width()).sum();
    let right_width: usize = right.iter().map(|s| s.content.width()).sum();
    let total_width = area.width as usize;

    // Center zone: notification (fills remaining space)
    let center_avail = total_width.saturating_sub(left_width + right_width);
    let center = build_center_zone(state, center_avail);

    let mut spans = left;
    spans.extend(center);
    spans.extend(right);

    let line = Line::from(spans);
    let paragraph = Paragraph::new(line).style(Theme::status_style());
    f.render_widget(paragraph, area);
}

/// Build the left zone: mode badge + board name + active filters/tags.
fn build_left_zone<'a>(state: &'a AppState, board_name: &'a str) -> Vec<Span<'a>> {
    let mode_str = match &state.mode {
        Mode::Normal => "NORMAL",
        Mode::Goto => "GOTO",
        Mode::Space => "SPACE",
        Mode::View => "VIEW",
        Mode::FilterMenu => "FILTER",
        Mode::CardDetail { .. } => "DETAIL",
        Mode::Metrics { .. } => "METRICS",
        Mode::Tutorial => "TUTORIAL",
        Mode::Help => "HELP",
        Mode::Picker { .. } => "PICKER",
        // Full-line modes handled separately
        Mode::Input { .. } | Mode::Confirm { .. } | Mode::Filter { .. } | Mode::Command { .. } => "",
    };

    let mut spans = vec![
        Span::styled(
            format!(" {mode_str} "),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED),
        ),
        Span::raw(" "),
        Span::styled(format!("{board_name} "), Style::default().fg(Theme::DIM)),
    ];

    // Show text filter if active
    if let Some(ref filter) = state.active_filter {
        spans.push(Span::styled(
            format!("/{filter} "),
            Style::default().fg(Theme::FG),
        ));
    }

    // Show active tag filters
    if !state.active_tag_filters.is_empty() {
        for tag in &state.active_tag_filters {
            spans.push(Span::styled(
                format!("@{tag} "),
                Style::default().fg(Theme::tag_color(tag)),
            ));
        }
    }

    spans
}

/// Build the right zone: column position + sync status.
fn build_right_zone<'a>(state: &'a AppState, board: &'a Board) -> Vec<Span<'a>> {
    let mut spans = Vec::new();

    // Column name + card count + position
    if let Some(col) = board.columns.get(state.focused_column) {
        let card_count = col.cards.len();
        let pos = if card_count > 0 {
            format!(" {}/{}",  state.selected_card + 1, card_count)
        } else {
            " 0".to_string()
        };
        spans.push(Span::styled(
            format!("{}[{}]", col.name, card_count),
            Style::default().fg(Theme::DIM),
        ));
        spans.push(Span::styled(
            pos,
            Style::default().fg(Theme::FG),
        ));
    }

    // Sync status
    if let Some(ref sync_state) = state.sync_state {
        spans.push(Span::raw(" "));
        let icons = theme::icons(state.nerd_font);
        let (sync_icon, sync_color) = if sync_state.online {
            (icons.sync_online, Theme::WIP_OK)
        } else {
            (icons.sync_offline, Theme::WIP_OVER)
        };
        spans.push(Span::styled(sync_icon, Style::default().fg(sync_color)));
    }

    spans.push(Span::raw(" "));
    spans
}

/// Build the center zone: notification text padded to fill available width.
fn build_center_zone<'a>(state: &'a AppState, avail_width: usize) -> Vec<Span<'a>> {
    if let Some(ref notif) = state.notification {
        let notif_width = notif.width();
        let color = match state.notification_level {
            NotificationLevel::Info => Theme::FG,
            NotificationLevel::Error => Theme::STATUS_ERROR,
        };

        if notif_width >= avail_width {
            // Notification wider than available — just show it truncated
            let truncated: String = notif.chars().take(avail_width).collect();
            return vec![Span::styled(truncated, Style::default().fg(color))];
        }

        // Center the notification in the available space
        let pad_total = avail_width - notif_width;
        let pad_left = pad_total / 2;
        let pad_right = pad_total - pad_left;

        vec![
            Span::raw(" ".repeat(pad_left)),
            Span::styled(notif.as_str(), Style::default().fg(color)),
            Span::raw(" ".repeat(pad_right)),
        ]
    } else {
        // No notification — just fill with spaces
        vec![Span::raw(" ".repeat(avail_width))]
    }
}

/// Render full-line modes (Filter, Input, Confirm, Command).
fn render_full_line_mode<'a>(state: &'a AppState, board: &Board) -> Option<Line<'a>> {
    match &state.mode {
        Mode::Filter { buf } => {
            let spans = vec![
                Span::styled(
                    " / ",
                    Style::default()
                        .fg(Theme::FG)
                        .add_modifier(Modifier::BOLD | Modifier::REVERSED),
                ),
                Span::raw(format!(" {}", buf.input)),
                Span::raw("_"),
            ];
            Some(Line::from(spans))
        }
        Mode::Input { prompt, buf, .. } => {
            let spans = vec![
                Span::styled(
                    format!(" {prompt} "),
                    Style::default()
                        .fg(Theme::FG)
                        .add_modifier(Modifier::BOLD | Modifier::REVERSED),
                ),
                Span::raw(format!(" {}", buf.input)),
                Span::raw("_"),
            ];
            Some(Line::from(spans))
        }
        Mode::Confirm { prompt, .. } => {
            let spans = vec![Span::styled(
                format!(" {prompt} (y/n) "),
                Style::default()
                    .fg(Theme::FG)
                    .add_modifier(Modifier::BOLD | Modifier::REVERSED),
            )];
            Some(Line::from(spans))
        }
        Mode::Command { cmd } => {
            let (card_tags, card_assignees) = state.selected_card_metadata(board);
            let mut spans = vec![
                Span::styled(
                    " : ",
                    Style::default()
                        .fg(Theme::FG)
                        .add_modifier(Modifier::BOLD | Modifier::REVERSED),
                ),
                Span::raw(format!(" {}", cmd.buf.input)),
                Span::raw("_"),
            ];
            // Ghost completion text (dimmed, after cursor) — only when NOT actively cycling
            if cmd.completion.is_none() {
                if let Some(ghost) = crate::command::compute_ghost(&cmd.buf.input, board, &card_tags, &card_assignees, &state.cached_trash_ids) {
                    spans.push(Span::styled(ghost, Style::default().fg(Theme::DIM)));
                }
            }
            Some(Line::from(spans))
        }
        _ => None,
    }
}
