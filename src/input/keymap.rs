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
        Mode::Picker { .. } => map_picker(key),
        Mode::Tutorial { .. } => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::DismissTutorial,
            KeyCode::Char('j') | KeyCode::Down => Action::DetailScrollDown,
            KeyCode::Char('k') | KeyCode::Up => Action::DetailScrollUp,
            _ => Action::None,
        },
        Mode::Help { .. } => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::ClosePanel,
            KeyCode::Char('j') | KeyCode::Down => Action::DetailScrollDown,
            KeyCode::Char('k') | KeyCode::Up => Action::DetailScrollUp,
            _ => Action::None,
        },
        Mode::Metrics { .. } => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::ClosePanel,
            KeyCode::Char('j') | KeyCode::Down => Action::DetailScrollDown,
            KeyCode::Char('k') | KeyCode::Up => Action::DetailScrollUp,
            _ => Action::None,
        },
        Mode::CardDetail { .. } => match key.code {
            KeyCode::Esc | KeyCode::Char('q') => Action::ClosePanel,
            KeyCode::Char('e') => Action::EditCardExternal,
            KeyCode::Char('j') | KeyCode::Down => Action::DetailScrollDown,
            KeyCode::Char('k') | KeyCode::Up => Action::DetailScrollUp,
            KeyCode::Tab => Action::DetailNextCard,
            KeyCode::BackTab => Action::DetailPrevCard,
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
        KeyCode::Char('m') => Action::ShowMetrics,
        KeyCode::Char('r') => Action::ReloadBoard,
        KeyCode::Char('s') => Action::StartSort,
        KeyCode::Char('n') => Action::FindNext,
        KeyCode::Char('N') => Action::FindPrev,
        KeyCode::Char('?') => Action::ShowHelp,
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
        KeyCode::Char('x') => Action::ArchiveCard,
        KeyCode::Char('u') => Action::Undo,
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
        KeyCode::Char('w') => Action::ColSetWip,
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
        KeyCode::Char('s') => Action::StartStalenessFilter,
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
// Used by help overlay, status bar hints, and minor-mode popup.
// ---------------------------------------------------------------------------

/// A documented keybinding for display in help/hints.
pub struct Binding {
    pub key: &'static str,
    pub description: &'static str,
}

/// A group of related bindings (one section in help overlay).
pub struct BindingGroup {
    pub name: &'static str,
    pub bindings: &'static [Binding],
}

pub const NORMAL_BINDINGS: &[Binding] = &[
    Binding { key: "h / l", description: "Switch columns" },
    Binding { key: "H / L", description: "Move card left/right" },
    Binding { key: "j / k", description: "Move between cards" },
    Binding { key: "n / N", description: "Next/prev match" },
    Binding { key: "Enter", description: "Open card detail" },
    Binding { key: "/", description: "Search cards" },
    Binding { key: "s", description: "Sort column" },
    Binding { key: "m", description: "Board metrics" },
    Binding { key: "r", description: "Reload board" },
    Binding { key: "u", description: "Undo last delete" },
    Binding { key: "?", description: "Help" },
    Binding { key: "Esc", description: "Clear filters" },
    Binding { key: "q", description: "Quit" },
];

pub const SPACE_BINDINGS: &[Binding] = &[
    Binding { key: "n", description: "New card" },
    Binding { key: "d", description: "Delete card" },
    Binding { key: "e", description: "Edit in $EDITOR" },
    Binding { key: "t", description: "Edit tags" },
    Binding { key: "a", description: "Edit assignees" },
    Binding { key: "p", description: "Set priority" },
    Binding { key: "m", description: "Move to column" },
    Binding { key: "b", description: "Toggle blocker" },
    Binding { key: "x", description: "Archive card" },
    Binding { key: "u", description: "Undo last delete" },
];

pub const FILTER_BINDINGS: &[Binding] = &[
    Binding { key: "t", description: "By tag" },
    Binding { key: "a", description: "By assignee" },
    Binding { key: "s", description: "By staleness" },
    Binding { key: "/", description: "Text search" },
];

pub const GOTO_BINDINGS: &[Binding] = &[
    Binding { key: "1-9", description: "Jump to column" },
    Binding { key: "b", description: "Backlog" },
    Binding { key: "d", description: "Done" },
    Binding { key: "g", description: "First card" },
    Binding { key: "e", description: "Last card" },
];

pub const COLUMN_BINDINGS: &[Binding] = &[
    Binding { key: "a", description: "Add column before focused" },
    Binding { key: "d", description: "Delete focused column" },
    Binding { key: "h", description: "Toggle column hidden" },
    Binding { key: "m", description: "Move focused column" },
    Binding { key: "r", description: "Rename focused column" },
    Binding { key: "s", description: "Toggle showing hidden cols" },
    Binding { key: "w", description: "Set WIP limit" },
];

pub const COL_MOVE_BINDINGS: &[Binding] = &[
    Binding { key: "1-9", description: "Move to position N" },
    Binding { key: "h",   description: "Move column left" },
    Binding { key: "l",   description: "Move column right" },
    Binding { key: "g",   description: "Move column to first" },
    Binding { key: "e",   description: "Move column to last" },
];

pub const DETAIL_BINDINGS: &[Binding] = &[
    Binding { key: "j / k", description: "Scroll" },
    Binding { key: "Tab/S-Tab", description: "Next/prev card" },
    Binding { key: "e", description: "Edit in $EDITOR" },
    Binding { key: "Esc", description: "Close" },
];

/// All binding groups for the help overlay.
pub const HELP_GROUPS: &[BindingGroup] = &[
    BindingGroup { name: "Normal Mode", bindings: NORMAL_BINDINGS },
    BindingGroup { name: "Commands (Space)", bindings: SPACE_BINDINGS },
    BindingGroup { name: "Goto (g)", bindings: GOTO_BINDINGS },
    BindingGroup { name: "Filter (f)", bindings: FILTER_BINDINGS },
    BindingGroup { name: "Column (c)", bindings: COLUMN_BINDINGS },
    BindingGroup { name: "Column Move (cm)", bindings: COL_MOVE_BINDINGS },
    BindingGroup { name: "Card Detail", bindings: DETAIL_BINDINGS },
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
    fn normal_m_maps_to_show_metrics() {
        let action = map_key(key(KeyCode::Char('m')), &Mode::Normal);
        assert_eq!(action, Action::ShowMetrics);
    }

    #[test]
    fn normal_r_maps_to_reload_board() {
        let action = map_key(key(KeyCode::Char('r')), &Mode::Normal);
        assert_eq!(action, Action::ReloadBoard);
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
    fn metrics_mode_j_scrolls_down() {
        let action = map_key(key(KeyCode::Char('j')), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::DetailScrollDown);
    }

    #[test]
    fn metrics_mode_k_scrolls_up() {
        let action = map_key(key(KeyCode::Char('k')), &Mode::Metrics { scroll: 3 });
        assert_eq!(action, Action::DetailScrollUp);
    }

    #[test]
    fn metrics_mode_down_arrow_scrolls_down() {
        let action = map_key(key(KeyCode::Down), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::DetailScrollDown);
    }

    #[test]
    fn metrics_mode_up_arrow_scrolls_up() {
        let action = map_key(key(KeyCode::Up), &Mode::Metrics { scroll: 3 });
        assert_eq!(action, Action::DetailScrollUp);
    }

    #[test]
    fn metrics_mode_other_key_is_noop() {
        let action = map_key(key(KeyCode::Char('x')), &Mode::Metrics { scroll: 0 });
        assert_eq!(action, Action::None);
    }

    // ── Help mode bindings ──

    #[test]
    fn help_mode_esc_closes() {
        let action = map_key(key(KeyCode::Esc), &Mode::Help { scroll: 5 });
        assert_eq!(action, Action::ClosePanel);
    }

    #[test]
    fn help_mode_q_closes() {
        let action = map_key(key(KeyCode::Char('q')), &Mode::Help { scroll: 0 });
        assert_eq!(action, Action::ClosePanel);
    }

    #[test]
    fn help_mode_j_scrolls_down() {
        let action = map_key(key(KeyCode::Char('j')), &Mode::Help { scroll: 0 });
        assert_eq!(action, Action::DetailScrollDown);
    }

    #[test]
    fn help_mode_k_scrolls_up() {
        let action = map_key(key(KeyCode::Char('k')), &Mode::Help { scroll: 3 });
        assert_eq!(action, Action::DetailScrollUp);
    }

    #[test]
    fn help_mode_down_arrow_scrolls_down() {
        let action = map_key(key(KeyCode::Down), &Mode::Help { scroll: 0 });
        assert_eq!(action, Action::DetailScrollDown);
    }

    #[test]
    fn help_mode_up_arrow_scrolls_up() {
        let action = map_key(key(KeyCode::Up), &Mode::Help { scroll: 3 });
        assert_eq!(action, Action::DetailScrollUp);
    }

    #[test]
    fn help_mode_other_key_is_noop() {
        let action = map_key(key(KeyCode::Char('x')), &Mode::Help { scroll: 0 });
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

    #[test]
    fn filter_menu_s_starts_staleness_filter() {
        assert_eq!(map_key(key(KeyCode::Char('s')), &Mode::FilterMenu), Action::StartStalenessFilter);
    }

    // ── CardDetail mode bindings ──

    #[test]
    fn detail_e_edits_external() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('e')), &mode), Action::EditCardExternal);
    }

    #[test]
    fn detail_j_k_scrolls() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('j')), &mode), Action::DetailScrollDown);
        assert_eq!(map_key(key(KeyCode::Char('k')), &mode), Action::DetailScrollUp);
    }

    #[test]
    fn detail_tab_navigates_cards() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Tab), &mode), Action::DetailNextCard);
        assert_eq!(map_key(key(KeyCode::BackTab), &mode), Action::DetailPrevCard);
    }

    #[test]
    fn detail_esc_closes() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Esc), &mode), Action::ClosePanel);
    }

    #[test]
    fn detail_q_closes() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('q')), &mode), Action::ClosePanel);
    }

    #[test]
    fn detail_down_arrow_scrolls_down() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Down), &mode), Action::DetailScrollDown);
    }

    #[test]
    fn detail_up_arrow_scrolls_up() {
        let mode = Mode::CardDetail { scroll: 3 };
        assert_eq!(map_key(key(KeyCode::Up), &mode), Action::DetailScrollUp);
    }

    #[test]
    fn detail_t_is_noop() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('t')), &mode), Action::None);
    }

    #[test]
    fn detail_p_is_noop() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('p')), &mode), Action::None);
    }

    #[test]
    fn detail_b_is_noop() {
        let mode = Mode::CardDetail { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('b')), &mode), Action::None);
    }

    // ── Tutorial mode bindings ──

    #[test]
    fn tutorial_j_scrolls_down() {
        let mode = Mode::Tutorial { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('j')), &mode), Action::DetailScrollDown);
    }

    #[test]
    fn tutorial_k_scrolls_up() {
        let mode = Mode::Tutorial { scroll: 3 };
        assert_eq!(map_key(key(KeyCode::Char('k')), &mode), Action::DetailScrollUp);
    }

    #[test]
    fn tutorial_esc_dismisses() {
        let mode = Mode::Tutorial { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Esc), &mode), Action::DismissTutorial);
    }

    #[test]
    fn tutorial_q_dismisses() {
        let mode = Mode::Tutorial { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('q')), &mode), Action::DismissTutorial);
    }

    #[test]
    fn tutorial_down_arrow_scrolls_down() {
        let mode = Mode::Tutorial { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Down), &mode), Action::DetailScrollDown);
    }

    #[test]
    fn tutorial_up_arrow_scrolls_up() {
        let mode = Mode::Tutorial { scroll: 3 };
        assert_eq!(map_key(key(KeyCode::Up), &mode), Action::DetailScrollUp);
    }

    #[test]
    fn tutorial_other_key_is_noop() {
        let mode = Mode::Tutorial { scroll: 0 };
        assert_eq!(map_key(key(KeyCode::Char('x')), &mode), Action::None);
    }

    // ── Binding registry tests ──

    #[test]
    fn normal_bindings_contains_question_mark() {
        let entry = NORMAL_BINDINGS.iter().find(|b| b.key == "?");
        assert!(entry.is_some(), "? binding missing from NORMAL_BINDINGS");
        assert_eq!(entry.unwrap().description, "Help");
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
