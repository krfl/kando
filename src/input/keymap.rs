use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use super::action::Action;
use crate::app::Mode;

/// Map a key event to a semantic action based on current mode.
pub fn map_key(key: KeyEvent, mode: &Mode) -> Action {
    match mode {
        Mode::Normal => map_normal(key),
        Mode::Goto => map_goto(key),
        Mode::Space => map_space(key),
        Mode::Column => map_column(key),
        Mode::ColMove => map_col_move(key),
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
        Mode::Metrics { .. } => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::ClosePanel,
            KeyCode::Char('J') | KeyCode::Down => Action::DetailScrollDown,
            KeyCode::Char('K') | KeyCode::Up => Action::DetailScrollUp,
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
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::Quit,
        KeyCode::Char('c') => Action::EnterColumnMode,
        KeyCode::Char('u') => Action::Undo,
        KeyCode::Char('?') => Action::ShowHelp,
        KeyCode::Char(':') => Action::EnterCommandMode,
        KeyCode::Tab => Action::CycleNextCard,
        KeyCode::BackTab => Action::CyclePrevCard,
        KeyCode::Esc => Action::ClearFilters,
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
        KeyCode::Char('s') => Action::ShowMetrics,
        KeyCode::Char('u') => Action::Undo,
        KeyCode::Char('/') => Action::StartFilter,
        KeyCode::Char('?') => Action::ShowHelp,
        KeyCode::Char('r') => Action::ReloadBoard,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

/// Map keys for column-management mode, entered by pressing `c` in Normal mode.
/// Displayed as "Column (c)" in the help panel; uses [`Mode::Column`] internally.
fn map_column(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('h') => Action::ToggleFocusedColumnHidden,
        KeyCode::Char('s') => Action::ToggleHiddenColumns,
        KeyCode::Char('r') => Action::ColRenameSelected,
        KeyCode::Char('a') => Action::ColAddBefore,
        KeyCode::Char('d') => Action::ColRemoveSelected,
        KeyCode::Char('m') => Action::EnterColMoveMode,
        KeyCode::Esc => Action::None,
        _ => Action::None,
    }
}

/// Map keys for column-move mode, entered by pressing `m` in Column mode.
/// Moves the currently focused column to a new position.
fn map_col_move(key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char('h') => Action::ColMoveLeft,
        KeyCode::Char('l') => Action::ColMoveRight,
        KeyCode::Char('g') => Action::ColMoveFirst,
        KeyCode::Char('e') => Action::ColMoveLast,
        KeyCode::Char(c @ '1'..='9') => Action::ColMoveToPosition((c as usize) - b'0' as usize),
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
    Binding { key: "?", description: "Help", tutorial: true },
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
    Binding { key: "s", description: "Board metrics", tutorial: true },
    Binding { key: "u", description: "Undo last delete", tutorial: false },
    Binding { key: "r", description: "Reload board", tutorial: false },
    Binding { key: "/", description: "Search all", tutorial: false },
    Binding { key: "?", description: "Help", tutorial: false },
];

pub const FILTER_BINDINGS: &[Binding] = &[
    Binding { key: "t", description: "By tag", tutorial: false },
    Binding { key: "a", description: "By assignee", tutorial: false },
    Binding { key: "/", description: "Text search", tutorial: false },
];

pub const GOTO_BINDINGS: &[Binding] = &[
    Binding { key: "1-9", description: "Jump to column", tutorial: false },
    Binding { key: "b", description: "Backlog", tutorial: false },
    Binding { key: "d", description: "Done", tutorial: false },
    Binding { key: "g", description: "First card", tutorial: false },
    Binding { key: "e", description: "Last card", tutorial: false },
];

pub const COLUMN_BINDINGS: &[Binding] = &[
    Binding { key: "r", description: "Rename focused column",      tutorial: false },
    Binding { key: "a", description: "Add column before focused",  tutorial: false },
    Binding { key: "m", description: "Move focused column",        tutorial: false },
    Binding { key: "h", description: "Toggle column hidden",        tutorial: false },
    Binding { key: "s", description: "Toggle showing hidden cols", tutorial: false },
    Binding { key: "d", description: "Delete focused column",      tutorial: false },
];

pub const COL_MOVE_BINDINGS: &[Binding] = &[
    Binding { key: "1-9", description: "Move to position N",   tutorial: false },
    Binding { key: "h",   description: "Move column left",     tutorial: false },
    Binding { key: "l",   description: "Move column right",    tutorial: false },
    Binding { key: "g",   description: "Move column to first", tutorial: false },
    Binding { key: "e",   description: "Move column to last",  tutorial: false },
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
/// Must not duplicate entries already in NORMAL_BINDINGS with tutorial: true.
const TUTORIAL_EXTRA: &[Binding] = &[
    Binding { key: "g", description: "Goto mode (jump to columns)", tutorial: true },
    Binding { key: "c", description: "Column mode (hide/rename/…)", tutorial: true },
    Binding { key: "/", description: "Search cards", tutorial: true },
    Binding { key: "q", description: "Quit", tutorial: true },
];

/// All binding groups for the help overlay.
pub const HELP_GROUPS: &[BindingGroup] = &[
    BindingGroup { name: "Normal Mode", bindings: NORMAL_BINDINGS },
    BindingGroup { name: "Commands (Space)", bindings: SPACE_BINDINGS },
    BindingGroup { name: "Goto (g)", bindings: GOTO_BINDINGS },
    BindingGroup { name: "Column (c)", bindings: COLUMN_BINDINGS },
    BindingGroup { name: "Column Move (cm)", bindings: COL_MOVE_BINDINGS },
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
        Mode::Column => COLUMN_BINDINGS,
        Mode::ColMove => COL_MOVE_BINDINGS,
        Mode::FilterMenu => FILTER_BINDINGS,
        _ => &[],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    #[test]
    fn space_s_maps_to_show_metrics() {
        let action = map_key(key(KeyCode::Char('s')), &Mode::Space);
        assert_eq!(action, Action::ShowMetrics);
    }

    #[test]
    fn metrics_mode_esc_closes() {
        let action = map_key(key(KeyCode::Esc), &Mode::Metrics { scroll: 5 });
        assert_eq!(action, Action::ClosePanel);
    }

    #[test]
    fn metrics_mode_q_closes() {
        let action = map_key(key(KeyCode::Char('q')), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::ClosePanel);
    }

    #[test]
    fn metrics_mode_shift_j_scrolls_down() {
        let action = map_key(key(KeyCode::Char('J')), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::DetailScrollDown);
    }

    #[test]
    fn metrics_mode_shift_k_scrolls_up() {
        let action = map_key(key(KeyCode::Char('K')), &Mode::Metrics { scroll: 3 });
        assert_eq!(action, Action::DetailScrollUp);
    }

    #[test]
    fn metrics_mode_other_key_is_noop() {
        let action = map_key(key(KeyCode::Char('x')), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::None);
    }

    // ── Normal mode bindings ──

    #[test]
    fn normal_h_moves_left() {
        assert_eq!(map_key(key(KeyCode::Char('h')), &Mode::Normal), Action::FocusPrevColumn);
        assert_eq!(map_key(key(KeyCode::Left), &Mode::Normal), Action::FocusPrevColumn);
    }

    #[test]
    fn normal_l_moves_right() {
        assert_eq!(map_key(key(KeyCode::Char('l')), &Mode::Normal), Action::FocusNextColumn);
        assert_eq!(map_key(key(KeyCode::Right), &Mode::Normal), Action::FocusNextColumn);
    }

    #[test]
    fn normal_j_selects_next() {
        assert_eq!(map_key(key(KeyCode::Char('j')), &Mode::Normal), Action::SelectNextCard);
        assert_eq!(map_key(key(KeyCode::Down), &Mode::Normal), Action::SelectNextCard);
    }

    #[test]
    fn normal_k_selects_prev() {
        assert_eq!(map_key(key(KeyCode::Char('k')), &Mode::Normal), Action::SelectPrevCard);
        assert_eq!(map_key(key(KeyCode::Up), &Mode::Normal), Action::SelectPrevCard);
    }

    #[test]
    fn normal_shift_h_l_moves_card() {
        assert_eq!(map_key(key(KeyCode::Char('H')), &Mode::Normal), Action::MoveCardPrevColumn);
        assert_eq!(map_key(key(KeyCode::Char('L')), &Mode::Normal), Action::MoveCardNextColumn);
    }

    #[test]
    fn normal_c_enters_column_mode() {
        assert_eq!(map_key(key(KeyCode::Char('c')), &Mode::Normal), Action::EnterColumnMode);
    }

    #[test]
    fn normal_ctrl_c_quits_not_column_mode() {
        // CONTROL+c must map to Quit even though bare 'c' maps to EnterColumnMode.
        assert_eq!(map_key(key_ctrl(KeyCode::Char('c')), &Mode::Normal), Action::Quit);
    }

    #[test]
    fn normal_enter_opens_detail() {
        assert_eq!(map_key(key(KeyCode::Enter), &Mode::Normal), Action::OpenCardDetail);
    }

    #[test]
    fn normal_slash_starts_filter() {
        assert_eq!(map_key(key(KeyCode::Char('/')), &Mode::Normal), Action::StartFilter);
    }

    #[test]
    fn normal_colon_enters_command() {
        assert_eq!(map_key(key(KeyCode::Char(':')), &Mode::Normal), Action::EnterCommandMode);
    }

    #[test]
    fn normal_g_enters_goto() {
        assert_eq!(map_key(key(KeyCode::Char('g')), &Mode::Normal), Action::EnterGotoMode);
    }

    #[test]
    fn normal_space_enters_space_mode() {
        assert_eq!(map_key(key(KeyCode::Char(' ')), &Mode::Normal), Action::EnterSpaceMode);
    }

    #[test]
    fn normal_q_quits() {
        assert_eq!(map_key(key(KeyCode::Char('q')), &Mode::Normal), Action::Quit);
    }

    #[test]
    fn normal_u_undoes() {
        assert_eq!(map_key(key(KeyCode::Char('u')), &Mode::Normal), Action::Undo);
    }

    #[test]
    fn normal_question_mark_shows_help() {
        assert_eq!(map_key(key(KeyCode::Char('?')), &Mode::Normal), Action::ShowHelp);
    }

    #[test]
    fn normal_unmapped_key_is_noop() {
        assert_eq!(map_key(key(KeyCode::Char('x')), &Mode::Normal), Action::None);
    }

    // ── Input mode bindings ──

    fn key_ctrl(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::CONTROL)
    }

    #[test]
    fn input_enter_confirms() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key(KeyCode::Enter), &mode), Action::InputConfirm);
    }

    #[test]
    fn input_esc_cancels() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key(KeyCode::Esc), &mode), Action::InputCancel);
    }

    #[test]
    fn input_ctrl_a_homes() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key_ctrl(KeyCode::Char('a')), &mode), Action::InputHome);
    }

    #[test]
    fn input_ctrl_e_ends() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key_ctrl(KeyCode::Char('e')), &mode), Action::InputEnd);
    }

    #[test]
    fn input_ctrl_w_deletes_word() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key_ctrl(KeyCode::Char('w')), &mode), Action::InputDeleteWord);
    }

    #[test]
    fn input_char_inserts() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key(KeyCode::Char('a')), &mode), Action::InputChar('a'));
    }

    #[test]
    fn input_backspace_deletes() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key(KeyCode::Backspace), &mode), Action::InputBackspace);
    }

    #[test]
    fn input_left_right_moves() {
        let mode = Mode::Input {
            prompt: "Title".into(),
            buf: crate::app::TextBuffer::empty(),
            on_confirm: crate::app::InputTarget::NewCardTitle,
        };
        assert_eq!(map_key(key(KeyCode::Left), &mode), Action::InputLeft);
        assert_eq!(map_key(key(KeyCode::Right), &mode), Action::InputRight);
    }

    // ── Command mode bindings ──

    #[test]
    fn command_tab_completes() {
        let mode = Mode::Command { cmd: crate::command::CommandState::new() };
        assert_eq!(map_key(key(KeyCode::Tab), &mode), Action::InputComplete);
        assert_eq!(map_key(key(KeyCode::BackTab), &mode), Action::InputCompleteBack);
    }

    #[test]
    fn command_enter_confirms() {
        let mode = Mode::Command { cmd: crate::command::CommandState::new() };
        assert_eq!(map_key(key(KeyCode::Enter), &mode), Action::InputConfirm);
    }

    // ── Goto mode bindings ──

    #[test]
    fn goto_digit_jumps() {
        assert_eq!(map_key(key(KeyCode::Char('1')), &Mode::Goto), Action::JumpToColumn(0));
        assert_eq!(map_key(key(KeyCode::Char('3')), &Mode::Goto), Action::JumpToColumn(2));
    }

    #[test]
    fn goto_g_jumps_first() {
        assert_eq!(map_key(key(KeyCode::Char('g')), &Mode::Goto), Action::JumpToFirstCard);
    }

    #[test]
    fn goto_e_jumps_last() {
        assert_eq!(map_key(key(KeyCode::Char('e')), &Mode::Goto), Action::JumpToLastCard);
    }

    #[test]
    fn goto_b_jumps_backlog() {
        assert_eq!(map_key(key(KeyCode::Char('b')), &Mode::Goto), Action::JumpToBacklog);
    }

    #[test]
    fn goto_d_jumps_done() {
        assert_eq!(map_key(key(KeyCode::Char('d')), &Mode::Goto), Action::JumpToDone);
    }

    // ── Space mode bindings ──

    #[test]
    fn space_n_new_card() {
        assert_eq!(map_key(key(KeyCode::Char('n')), &Mode::Space), Action::NewCard);
    }

    #[test]
    fn space_d_delete_card() {
        assert_eq!(map_key(key(KeyCode::Char('d')), &Mode::Space), Action::DeleteCard);
    }

    #[test]
    fn space_e_edit_external() {
        assert_eq!(map_key(key(KeyCode::Char('e')), &Mode::Space), Action::EditCardExternal);
    }

    #[test]
    fn space_p_pick_priority() {
        assert_eq!(map_key(key(KeyCode::Char('p')), &Mode::Space), Action::PickPriority);
    }

    #[test]
    fn space_m_move_to_column() {
        assert_eq!(map_key(key(KeyCode::Char('m')), &Mode::Space), Action::MoveToColumn);
    }

    #[test]
    fn space_b_toggle_blocker() {
        assert_eq!(map_key(key(KeyCode::Char('b')), &Mode::Space), Action::ToggleBlocker);
    }

    #[test]
    fn space_question_mark_shows_help() {
        assert_eq!(map_key(key(KeyCode::Char('?')), &Mode::Space), Action::ShowHelp);
    }

    // ── Column mode bindings ──

    #[test]
    fn column_h_hides_focused_column() {
        assert_eq!(map_key(key(KeyCode::Char('h')), &Mode::Column), Action::ToggleFocusedColumnHidden);
    }

    #[test]
    fn column_s_toggles_hidden_columns() {
        assert_eq!(map_key(key(KeyCode::Char('s')), &Mode::Column), Action::ToggleHiddenColumns);
    }

    #[test]
    fn column_r_enters_rename_prefilled() {
        assert_eq!(map_key(key(KeyCode::Char('r')), &Mode::Column), Action::ColRenameSelected);
    }

    #[test]
    fn column_a_enters_add_prefilled() {
        assert_eq!(map_key(key(KeyCode::Char('a')), &Mode::Column), Action::ColAddBefore);
    }

    #[test]
    fn column_d_enters_remove_prefilled() {
        assert_eq!(map_key(key(KeyCode::Char('d')), &Mode::Column), Action::ColRemoveSelected);
    }

    #[test]
    fn column_esc_cancels() {
        assert_eq!(map_key(key(KeyCode::Esc), &Mode::Column), Action::None);
    }

    // ── Confirm mode bindings ──

    #[test]
    fn confirm_y_confirms() {
        let mode = Mode::Confirm {
            prompt: "Delete?".into(),
            on_confirm: crate::app::ConfirmTarget::DeleteCard("1".into()),
        };
        assert_eq!(map_key(key(KeyCode::Char('y')), &mode), Action::Confirm);
        assert_eq!(map_key(key(KeyCode::Enter), &mode), Action::Confirm);
    }

    #[test]
    fn confirm_n_denies() {
        let mode = Mode::Confirm {
            prompt: "Delete?".into(),
            on_confirm: crate::app::ConfirmTarget::DeleteCard("1".into()),
        };
        assert_eq!(map_key(key(KeyCode::Char('n')), &mode), Action::Deny);
        assert_eq!(map_key(key(KeyCode::Esc), &mode), Action::Deny);
    }

    // ── Picker mode bindings ──

    #[test]
    fn picker_j_k_navigates() {
        let mode = Mode::Picker {
            title: "Tags".into(),
            items: vec![],
            selected: 0,
            target: crate::app::PickerTarget::TagFilter,
        };
        assert_eq!(map_key(key(KeyCode::Char('j')), &mode), Action::SelectNextCard);
        assert_eq!(map_key(key(KeyCode::Char('k')), &mode), Action::SelectPrevCard);
    }

    #[test]
    fn picker_enter_confirms() {
        let mode = Mode::Picker {
            title: "Tags".into(),
            items: vec![],
            selected: 0,
            target: crate::app::PickerTarget::TagFilter,
        };
        assert_eq!(map_key(key(KeyCode::Enter), &mode), Action::InputConfirm);
    }

    // ── FilterMenu mode bindings ──

    #[test]
    fn filter_menu_t_starts_tag_filter() {
        assert_eq!(map_key(key(KeyCode::Char('t')), &Mode::FilterMenu), Action::StartTagFilter);
    }

    #[test]
    fn filter_menu_a_starts_assignee_filter() {
        assert_eq!(map_key(key(KeyCode::Char('a')), &Mode::FilterMenu), Action::StartAssigneeFilter);
    }

    // ── CardDetail mode bindings ──

    #[test]
    fn detail_e_edits_external() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('e')), &mode), Action::EditCardExternal);
    }

    #[test]
    fn detail_t_edits_tags() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('t')), &mode), Action::EditTags);
    }

    #[test]
    fn detail_p_cycles_priority() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('p')), &mode), Action::CyclePriority);
    }

    #[test]
    fn detail_j_k_navigates_cards() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('j')), &mode), Action::DetailNextCard);
        assert_eq!(map_key(key(KeyCode::Char('k')), &mode), Action::DetailPrevCard);
    }

    #[test]
    fn detail_esc_closes() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Esc), &mode), Action::ClosePanel);
    }

    // ── Binding registry tests ──

    #[test]
    fn normal_bindings_contains_question_mark_tutorial() {
        let entry = NORMAL_BINDINGS.iter().find(|b| b.key == "?");
        assert!(entry.is_some(), "? binding missing from NORMAL_BINDINGS");
        let entry = entry.unwrap();
        assert_eq!(entry.description, "Help");
        assert!(entry.tutorial, "? binding should be marked tutorial: true");
    }

    #[test]
    fn tutorial_extra_has_no_question_mark() {
        // ? is in NORMAL_BINDINGS (tutorial: true) — it must not also appear in
        // TUTORIAL_EXTRA, which would cause it to render twice in the tutorial overlay.
        assert!(
            TUTORIAL_EXTRA.iter().all(|b| b.key != "?"),
            "? must not appear in TUTORIAL_EXTRA — it is already in NORMAL_BINDINGS"
        );
    }

    #[test]
    fn space_bindings_question_mark_not_tutorial() {
        // ? in SPACE_BINDINGS should never be tutorial: true — that would duplicate
        // the ? entry already in NORMAL_BINDINGS in the tutorial overlay.
        let entry = SPACE_BINDINGS.iter().find(|b| b.key == "?").unwrap();
        assert!(!entry.tutorial, "? in SPACE_BINDINGS should be tutorial: false");
    }

    // ── mode_bindings tests ──

    #[test]
    fn mode_bindings_goto_returns_bindings() {
        let bindings = mode_bindings(&Mode::Goto);
        assert!(!bindings.is_empty());
    }

    #[test]
    fn mode_bindings_space_returns_bindings() {
        let bindings = mode_bindings(&Mode::Space);
        assert!(!bindings.is_empty());
    }

    #[test]
    fn mode_bindings_column_returns_bindings() {
        let bindings = mode_bindings(&Mode::Column);
        assert!(!bindings.is_empty());
    }

    #[test]
    fn mode_bindings_normal_returns_empty() {
        let bindings = mode_bindings(&Mode::Normal);
        assert!(bindings.is_empty());
    }
}
