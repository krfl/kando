pub mod board_view;
pub mod card_detail;
pub mod help;
pub mod input_modal;
pub mod metrics;
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
        crate::app::Mode::Goto | crate::app::Mode::Space | crate::app::Mode::View | crate::app::Mode::ColMove | crate::app::Mode::FilterMenu => {
            input_modal::render_hint_popup(f, chunks[0], &state.mode);
        }
        crate::app::Mode::Picker { title, items, selected, .. } => {
            input_modal::render_picker(f, chunks[0], title, items, *selected);
        }
        crate::app::Mode::CardDetail { scroll } => {
            // Show card detail overlay
            if let Some(card) = state.selected_card_ref(board) {
                let icons = theme::icons(state.nerd_font);
                card_detail::render_card_detail(f, f.area(), card, &board.policies, *scroll, now, icons);
            }
        }
        crate::app::Mode::Tutorial => {
            tutorial::render_tutorial(f, f.area());
        }
        crate::app::Mode::Help => {
            help::render_help(f, f.area());
        }
        crate::app::Mode::Metrics { scroll } => {
            metrics::render_metrics(f, f.area(), board, *scroll, now);
        }
        crate::app::Mode::Command { cmd } => {
            let (card_tags, card_assignees) = state.selected_card_metadata(board);
            let (title, items) = crate::command::palette_items(&cmd.buf.input, board, &card_tags, &card_assignees, &state.cached_trash_ids);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn centered_rect_basic() {
        let area = Rect::new(0, 0, 100, 100);
        let r = centered_rect(area, 80, 60, 0, 0);
        assert_eq!(r.width, 80);
        assert_eq!(r.height, 60);
        assert_eq!(r.x, 10);
        assert_eq!(r.y, 20);
    }

    #[test]
    fn centered_rect_min_width_enforced() {
        let area = Rect::new(0, 0, 100, 100);
        // 10% of 100 = 10, but min_w = 50
        let r = centered_rect(area, 10, 50, 50, 0);
        assert_eq!(r.width, 50);
    }

    #[test]
    fn centered_rect_min_height_enforced() {
        let area = Rect::new(0, 0, 100, 100);
        let r = centered_rect(area, 50, 10, 0, 40);
        assert_eq!(r.height, 40);
    }

    #[test]
    fn centered_rect_capped_at_area() {
        let area = Rect::new(0, 0, 50, 50);
        // min_w=200 exceeds area width=50, should be clamped
        let r = centered_rect(area, 100, 100, 200, 200);
        assert_eq!(r.width, 50);
        assert_eq!(r.height, 50);
    }

    #[test]
    fn centered_rect_zero_area() {
        let area = Rect::new(0, 0, 0, 0);
        let r = centered_rect(area, 80, 60, 0, 0);
        assert_eq!(r.width, 0);
        assert_eq!(r.height, 0);
    }

    #[test]
    fn centered_rect_with_offset() {
        let area = Rect::new(10, 20, 100, 100);
        let r = centered_rect(area, 50, 50, 0, 0);
        assert_eq!(r.width, 50);
        assert_eq!(r.height, 50);
        assert_eq!(r.x, 35); // 10 + (100-50)/2
        assert_eq!(r.y, 45); // 20 + (100-50)/2
    }
}
