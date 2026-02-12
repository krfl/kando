use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::theme::Theme;
use crate::app::{AppState, Mode};

pub fn render_status_bar(f: &mut Frame, area: Rect, state: &AppState, board_name: &str) {
    let mode_str = match &state.mode {
        Mode::Normal => "NORMAL",
        Mode::Goto => "GOTO",
        Mode::Space => "SPACE",
        Mode::View => "VIEW",
        Mode::Input { prompt, .. } => prompt,
        Mode::Confirm { prompt, .. } => prompt,
        Mode::Filter { .. } => "FILTER",
        Mode::Picker { .. } => "PICKER",
        Mode::CardDetail { .. } => "DETAIL",
        Mode::Tutorial => "TUTORIAL",
        Mode::Help => "HELP",
    };

    let hints = match &state.mode {
        Mode::Normal => "h/l: column  j/k: card  H/L: move  space: commands  q: quit",
        Mode::Goto => "1-9: column  g: first  e: last  b: backlog  d: done",
        Mode::Space => "n: new  d: del  e: edit  t: tags  f: filter  p: priority  ?: help",
        Mode::View => "c: collapse  a: all  w: wip  z: center  h: hidden",
        Mode::Input { .. } => "Enter: confirm  Esc: cancel",
        Mode::Confirm { .. } => "y: yes  n: no",
        Mode::Filter { .. } => "type to filter  Enter: confirm  Esc: clear",
        Mode::Picker { .. } => "j/k: select  Enter: confirm  Esc: cancel",
        Mode::CardDetail { .. } => "Esc: close  j/k: card  J/K: scroll  e: edit  t: tags  p: priority  b: blocker",
        Mode::Tutorial => "Press any key to start",
        Mode::Help => "Esc: close",
    };

    let mut spans = vec![
        Span::styled(
            format!(" {mode_str} "),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD | Modifier::REVERSED),
        ),
        Span::raw(" "),
    ];

    // Show board name
    spans.push(Span::styled(
        format!("{board_name} "),
        Style::default().fg(Theme::DIM),
    ));
    spans.push(Span::raw("| "));

    // Show text filter if active
    if let Some(ref filter) = state.active_filter {
        spans.push(Span::raw(format!("filter: {filter} ")));
        spans.push(Span::raw("| "));
    }

    // Show active tag filters (item 2)
    if !state.active_tag_filters.is_empty() {
        spans.push(Span::styled("tags: ", Theme::dim_style()));
        for (i, tag) in state.active_tag_filters.iter().enumerate() {
            if i > 0 {
                spans.push(Span::raw(", "));
            }
            spans.push(Span::styled(
                tag.clone(),
                Style::default().fg(Theme::tag_color(tag)),
            ));
        }
        spans.push(Span::raw(" | "));
    }

    // Show sync status
    if let Some(ref sync_state) = state.sync_state {
        let (sync_icon, sync_color) = if sync_state.online {
            ("\u{f021} ", Theme::WIP_OK) // nf-fa-refresh, green
        } else {
            ("\u{f071} offline ", Theme::WIP_OVER) // nf-fa-warning, red
        };
        spans.push(Span::styled(sync_icon, Style::default().fg(sync_color)));
        if let Some(ref err) = sync_state.last_error {
            spans.push(Span::styled(
                format!("{err} "),
                Style::default().fg(Theme::WIP_OVER),
            ));
        }
        spans.push(Span::raw("| "));
    }

    // Show notification if present
    if let Some(ref notif) = state.notification {
        spans.push(Span::raw(format!("{notif} ")));
        spans.push(Span::raw("| "));
    }

    spans.push(Span::styled(
        hints.to_string(),
        Style::default().fg(Theme::STATUS_HINT),
    ));

    // Show input text if in filter/input mode
    match &state.mode {
        Mode::Filter { buf } => {
            spans.clear();
            spans.push(Span::styled(
                " / ",
                Style::default()
                    .fg(Theme::FG)
                    .add_modifier(Modifier::BOLD | Modifier::REVERSED),
            ));
            spans.push(Span::raw(format!(" {}", buf.input)));
            spans.push(Span::raw("_"));
        }
        Mode::Input { prompt, buf, .. } => {
            spans.clear();
            spans.push(Span::styled(
                format!(" {prompt} "),
                Style::default()
                    .fg(Theme::FG)
                    .add_modifier(Modifier::BOLD | Modifier::REVERSED),
            ));
            spans.push(Span::raw(format!(" {}", buf.input)));
            spans.push(Span::raw("_"));
        }
        Mode::Confirm { prompt, .. } => {
            spans.clear();
            spans.push(Span::styled(
                format!(" {prompt} (y/n) "),
                Style::default()
                    .fg(Theme::FG)
                    .add_modifier(Modifier::BOLD | Modifier::REVERSED),
            ));
        }
        _ => {}
    }

    let line = Line::from(spans);
    let paragraph = Paragraph::new(line).style(Theme::status_style());
    f.render_widget(paragraph, area);
}
