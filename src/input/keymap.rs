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
        Mode::FilterMenu => map_filter_menu(key),
        Mode::Input { .. } => map_input(key),
        Mode::Confirm { .. } => map_confirm(key),
        Mode::Filter { .. } => map_input(key),
        Mode::Command { .. } => map_command(key),
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
        KeyCode::Char('f') => Action::EnterFilterMode,
        KeyCode::Char('g') => Action::EnterGotoMode,
        KeyCode::Char(' ') => Action::EnterSpaceMode,
        KeyCode::Char('z') => Action::EnterViewMode,
        KeyCode::Char('u') => Action::Undo,
        KeyCode::Char(':') => Action::EnterCommandMode,
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
        KeyCode::Char('a') => Action::EditAssignees,
        KeyCode::Char('p') => Action::PickPriority,
        KeyCode::Char('m') => Action::MoveToColumn,
        KeyCode::Char('b') => Action::ToggleBlocker,
        KeyCode::Char('u') => Action::Undo,
        KeyCode::Char('/') => Action::StartFilter,
        KeyCode::Char('?') => Action::ShowHelp,
        KeyCode::Char('r') => Action::ReloadBoard,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

fn map_view(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('h') => Action::ToggleHiddenColumns,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

fn map_filter_menu(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('t') => Action::StartTagFilter,
        KeyCode::Char('a') => Action::StartAssigneeFilter,
        KeyCode::Char('/') => Action::StartFilter,
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

fn map_command(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Tab => Action::InputComplete,
        KeyCode::BackTab => Action::InputCompleteBack,
        // Everything else same as map_input
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

// ---------------------------------------------------------------------------
// Binding registry — single source of truth for keybinding documentation.
// Used by help overlay, tutorial, status bar hints, and minor-mode popup.
// ---------------------------------------------------------------------------

/// A documented keybinding for display in help/tutorial/hints.
pub struct Binding {
    pub key: &'static str,
    pub description: &'static str,
    /// Include in the first-launch tutorial overlay?
    pub tutorial: bool,
}

/// A group of related bindings (one section in help/tutorial).
pub struct BindingGroup {
    pub name: &'static str,
    pub bindings: &'static [Binding],
}

pub const NORMAL_BINDINGS: &[Binding] = &[
    Binding { key: "h / l", description: "Switch columns", tutorial: true },
    Binding { key: "j / k", description: "Move between cards", tutorial: true },
    Binding { key: "H / L", description: "Move card left/right", tutorial: true },
    Binding { key: "Enter", description: "Open card detail", tutorial: true },
    Binding { key: "u", description: "Undo last delete", tutorial: true },
    Binding { key: "/", description: "Search cards", tutorial: false },
    Binding { key: ":", description: "Command mode", tutorial: false },
    Binding { key: "Esc", description: "Clear filters", tutorial: false },
    Binding { key: "q", description: "Quit", tutorial: false },
];

pub const SPACE_BINDINGS: &[Binding] = &[
    Binding { key: "n", description: "New card", tutorial: true },
    Binding { key: "d", description: "Delete card", tutorial: true },
    Binding { key: "e", description: "Edit in $EDITOR", tutorial: true },
    Binding { key: "t", description: "Edit tags", tutorial: false },
    Binding { key: "a", description: "Edit assignees", tutorial: false },
    Binding { key: "p", description: "Set priority", tutorial: true },
    Binding { key: "m", description: "Move to column", tutorial: true },
    Binding { key: "b", description: "Toggle blocker", tutorial: false },
    Binding { key: "u", description: "Undo last delete", tutorial: false },
    Binding { key: "r", description: "Reload board", tutorial: false },
    Binding { key: "/", description: "Search all", tutorial: false },
    Binding { key: "?", description: "This help", tutorial: false },
];

pub const FILTER_BINDINGS: &[Binding] = &[
    Binding { key: "t", description: "By tag", tutorial: false },
    Binding { key: "a", description: "By assignee", tutorial: false },
    Binding { key: "/", description: "Text search", tutorial: false },
];

pub const GOTO_BINDINGS: &[Binding] = &[
    Binding { key: "1-9", description: "Jump to column", tutorial: false },
    Binding { key: "g", description: "First card", tutorial: false },
    Binding { key: "e", description: "Last card", tutorial: false },
    Binding { key: "b", description: "Backlog", tutorial: false },
    Binding { key: "d", description: "Done", tutorial: false },
];

pub const VIEW_BINDINGS: &[Binding] = &[
    Binding { key: "h", description: "Toggle hidden columns", tutorial: false },
];

pub const DETAIL_BINDINGS: &[Binding] = &[
    Binding { key: "j / k", description: "Next/prev card", tutorial: false },
    Binding { key: "J / K", description: "Scroll content", tutorial: false },
    Binding { key: "e", description: "Edit in $EDITOR", tutorial: false },
    Binding { key: "t", description: "Edit tags", tutorial: false },
    Binding { key: "p", description: "Cycle priority", tutorial: false },
    Binding { key: "b", description: "Toggle blocker", tutorial: false },
    Binding { key: "Esc", description: "Close", tutorial: false },
];

/// Extra bindings shown only in the tutorial "More" section.
const TUTORIAL_EXTRA: &[Binding] = &[
    Binding { key: "g", description: "Goto mode (jump to columns)", tutorial: true },
    Binding { key: "z", description: "View mode (hidden columns)", tutorial: true },
    Binding { key: "/", description: "Search cards", tutorial: true },
    Binding { key: "Space ?", description: "Full help", tutorial: true },
    Binding { key: "q", description: "Quit", tutorial: true },
];

/// All binding groups for the help overlay.
pub const HELP_GROUPS: &[BindingGroup] = &[
    BindingGroup { name: "Normal Mode", bindings: NORMAL_BINDINGS },
    BindingGroup { name: "Commands (Space)", bindings: SPACE_BINDINGS },
    BindingGroup { name: "Goto (g)", bindings: GOTO_BINDINGS },
    BindingGroup { name: "View (z)", bindings: VIEW_BINDINGS },
    BindingGroup { name: "Card Detail", bindings: DETAIL_BINDINGS },
];

/// Binding groups for the first-launch tutorial (different organization).
pub const TUTORIAL_GROUPS: &[BindingGroup] = &[
    BindingGroup { name: "Navigation", bindings: NORMAL_BINDINGS },
    BindingGroup { name: "Commands (Space)", bindings: SPACE_BINDINGS },
    BindingGroup { name: "More", bindings: TUTORIAL_EXTRA },
];

/// Get bindings for a minor mode (for popup and status display).
pub fn mode_bindings(mode: &Mode) -> &'static [Binding] {
    match mode {
        Mode::Goto => GOTO_BINDINGS,
        Mode::Space => SPACE_BINDINGS,
        Mode::View => VIEW_BINDINGS,
        Mode::FilterMenu => FILTER_BINDINGS,
        _ => &[],
    }
}
