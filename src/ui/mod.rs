pub mod board_view;
pub mod card_detail;
pub mod help;
pub mod input_modal;
pub mod status_bar;
pub mod theme;
pub mod tutorial;

use chrono::{DateTime, Utc};
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::Frame;

use crate::app::AppState;
use crate::board::Board;

/// Create a centered rect within `area` using percentage-based sizing with minimums.
pub fn centered_rect(area: Rect, w_pct: u16, h_pct: u16, min_w: u16, min_h: u16) -> Rect {
    let width = (area.width * w_pct / 100).max(min_w).min(area.width);
    let height = (area.height * h_pct / 100).max(min_h).min(area.height);
    let x = area.x + (area.width - width) / 2;
    let y = area.y + (area.height - height) / 2;
    Rect::new(x, y, width, height)
}

pub fn render(f: &mut Frame, board: &Board, state: &AppState, now: DateTime<Utc>) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(1), Constraint::Length(1)])
        .split(f.area());

    // Main board area
    board_view::render_board(f, chunks[0], board, state, now);

    // Status bar
    status_bar::render_status_bar(f, chunks[1], state, board);

    // Overlays
    match &state.mode {
        crate::app::Mode::Goto | crate::app::Mode::Space | crate::app::Mode::View | crate::app::Mode::FilterMenu => {
            input_modal::render_hint_popup(f, chunks[0], &state.mode);
        }
        crate::app::Mode::Picker { title, items, selected, .. } => {
            input_modal::render_picker(f, chunks[0], title, items, *selected);
        }
        crate::app::Mode::CardDetail { scroll } => {
            // Show card detail overlay
            if let Some(card) = state.selected_card_ref(board) {
                card_detail::render_card_detail(f, f.area(), card, &board.policies, *scroll, now);
            }
        }
        crate::app::Mode::Tutorial => {
            tutorial::render_tutorial(f, f.area());
        }
        crate::app::Mode::Help => {
            help::render_help(f, f.area());
        }
        crate::app::Mode::Command { cmd } => {
            let (card_tags, card_assignees) = state
                .selected_card_ref(board)
                .map(|c| (c.tags.clone(), c.assignees.clone()))
                .unwrap_or_default();
            let (title, items) = crate::command::palette_items(&cmd.buf.input, board, &card_tags, &card_assignees);
            if !items.is_empty() {
                // Determine which name is selected (from active completion state)
                let selected_name = cmd.completion.as_ref().and_then(|c| {
                    c.candidates.get(c.index).map(|s| s.as_str())
                });
                let query = crate::command::current_token(&cmd.buf.input);
                input_modal::render_command_palette(f, chunks[0], title, &items, selected_name, &query);
            }
        }
        _ => {}
    }
}
