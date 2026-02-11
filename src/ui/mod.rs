pub mod board_view;
pub mod card_detail;
pub mod help;
pub mod input_modal;
pub mod status_bar;
pub mod theme;
pub mod tutorial;

use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::Frame;

use crate::app::AppState;
use crate::board::Board;

pub fn render(f: &mut Frame, board: &Board, state: &AppState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(1), Constraint::Length(1)])
        .split(f.area());

    // Main board area
    board_view::render_board(f, chunks[0], board, state);

    // Status bar
    status_bar::render_status_bar(f, chunks[1], state, &board.name);

    // Overlays
    match &state.mode {
        crate::app::Mode::Goto | crate::app::Mode::Space | crate::app::Mode::View => {
            input_modal::render_hint_popup(f, chunks[0], &state.mode);
        }
        crate::app::Mode::Picker { title, items, selected, .. } => {
            input_modal::render_picker(f, chunks[0], title, items, *selected);
        }
        crate::app::Mode::CardDetail { scroll } => {
            // Show card detail overlay
            if let Some(card) = state.selected_card_ref(board) {
                card_detail::render_card_detail(f, f.area(), card, &board.policies, *scroll);
            }
        }
        crate::app::Mode::Tutorial => {
            tutorial::render_tutorial(f, f.area());
        }
        crate::app::Mode::Help => {
            help::render_help(f, f.area());
        }
        _ => {}
    }
}
