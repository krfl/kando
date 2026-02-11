use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use super::action::Action;
use crate::app::Mode;

/// Map a key event to a semantic action based on current mode.
pub fn map_key(key: KeyEvent, mode: &Mode) -> Action {
    match mode {
        Mode::Normal => map_normal(key),
        Mode::Goto => map_goto(key),
        Mode::Space => map_space(key),
        Mode::View => map_view(key),
        Mode::Input { .. } => map_input(key),
        Mode::Confirm { .. } => map_confirm(key),
        Mode::Filter { .. } => map_input(key),
        Mode::Picker { .. } => map_picker(key),
        Mode::Tutorial => Action::DismissTutorial,
        Mode::Help => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::Quit,
            _ => Action::None,
        },
        Mode::CardDetail { .. } => match key.code {
            KeyCode::Esc => Action::ClosePanel,
            KeyCode::Char('q') => Action::ClosePanel,
            KeyCode::Char('e') => Action::EditCardExternal,
            KeyCode::Char('t') => Action::EditTags,
            KeyCode::Char('p') => Action::CyclePriority,
            KeyCode::Char('b') => Action::ToggleBlocker,
            KeyCode::Char('j') | KeyCode::Down => Action::DetailNextCard,
            KeyCode::Char('k') | KeyCode::Up => Action::DetailPrevCard,
            KeyCode::Char('J') => Action::DetailScrollDown,
            KeyCode::Char('K') => Action::DetailScrollUp,
            _ => Action::None,
        },
    }
}

fn map_normal(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('h') | KeyCode::Left => Action::FocusPrevColumn,
        KeyCode::Char('l') | KeyCode::Right => Action::FocusNextColumn,
        KeyCode::Char('j') | KeyCode::Down => Action::SelectNextCard,
        KeyCode::Char('k') | KeyCode::Up => Action::SelectPrevCard,
        KeyCode::Char('H') => Action::MoveCardPrevColumn,
        KeyCode::Char('L') => Action::MoveCardNextColumn,
        KeyCode::Enter => Action::OpenCardDetail,
        KeyCode::Char('q') => Action::Quit,
        KeyCode::Char('/') => Action::StartFilter,
        KeyCode::Char('g') => Action::EnterGotoMode,
        KeyCode::Char(' ') => Action::EnterSpaceMode,
        KeyCode::Char('z') => Action::EnterViewMode,
        KeyCode::Tab => Action::CycleNextCard,
        KeyCode::BackTab => Action::CyclePrevCard,
        KeyCode::Esc => Action::ClearFilters,
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::Quit,
        _ => Action::None,
    }
}

fn map_goto(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char(c @ '1'..='9') => Action::JumpToColumn(c as usize - '1' as usize),
        KeyCode::Char('g') => Action::JumpToFirstCard,
        KeyCode::Char('e') => Action::JumpToLastCard,
        KeyCode::Char('b') => Action::JumpToBacklog,
        KeyCode::Char('d') => Action::JumpToDone,
        KeyCode::Esc => Action::None, // cancel goto mode
        _ => Action::None,
    }
}

fn map_space(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('n') => Action::NewCard,
        KeyCode::Char('d') => Action::DeleteCard,
        KeyCode::Char('e') => Action::EditCardExternal,
        KeyCode::Char('t') => Action::EditTags,
        KeyCode::Char('f') => Action::StartTagFilter,
        KeyCode::Char('p') => Action::PickPriority,
        KeyCode::Char('m') => Action::MoveToColumn,
        KeyCode::Char('b') => Action::ToggleBlocker,
        KeyCode::Char('/') => Action::StartSearch,
        KeyCode::Char('?') => Action::ShowHelp,
        KeyCode::Char('r') => Action::ReloadBoard,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

fn map_view(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('c') => Action::ToggleCollapseColumn,
        KeyCode::Char('a') => Action::ToggleCollapseAll,
        KeyCode::Char('w') => Action::ToggleWipDisplay,
        KeyCode::Char('z') => Action::CenterOnCard,
        KeyCode::Char('h') => Action::ToggleHiddenColumns,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

fn map_input(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Enter => Action::InputConfirm,
        KeyCode::Esc => Action::InputCancel,
        KeyCode::Char('a') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::InputHome,
        KeyCode::Char('e') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::InputEnd,
        KeyCode::Char('w') if key.modifiers.contains(KeyModifiers::CONTROL) => {
            Action::InputDeleteWord
        }
        KeyCode::Char(c) => Action::InputChar(c),
        KeyCode::Backspace => Action::InputBackspace,
        KeyCode::Left => Action::InputLeft,
        KeyCode::Right => Action::InputRight,
        KeyCode::Home => Action::InputHome,
        KeyCode::End => Action::InputEnd,
        _ => Action::None,
    }
}

fn map_confirm(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('y') | KeyCode::Enter => Action::Confirm,
        KeyCode::Char('n') | KeyCode::Esc => Action::Deny,
        _ => Action::None,
    }
}

fn map_picker(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('j') | KeyCode::Down => Action::SelectNextCard,
        KeyCode::Char('k') | KeyCode::Up => Action::SelectPrevCard,
        KeyCode::Enter | KeyCode::Char(' ') => Action::InputConfirm,
        KeyCode::Esc => Action::InputCancel,
        _ => Action::None,
    }
}

/// Get hints for the current mode (for the popup panel).
pub fn mode_hints(mode: &Mode) -> Vec<(&'static str, &'static str)> {
    match mode {
        Mode::Goto => vec![
            ("1-9", "jump to column"),
            ("g", "first card"),
            ("e", "last card"),
            ("b", "backlog"),
            ("d", "done"),
        ],
        Mode::Space => vec![
            ("n", "new card"),
            ("d", "delete card"),
            ("e", "edit in $EDITOR"),
            ("t", "edit tags"),
            ("f", "filter by tag"),
            ("p", "priority"),
            ("m", "move to column"),
            ("b", "toggle blocker"),
            ("r", "reload board"),
            ("/", "search all"),
            ("?", "help"),
        ],
        Mode::View => vec![
            ("c", "collapse column"),
            ("a", "collapse all"),
            ("w", "toggle WIP"),
            ("z", "center on card"),
            ("h", "toggle hidden"),
        ],
        _ => vec![],
    }
}
