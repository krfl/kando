use std::time::{Duration, Instant};

use chrono::Utc;
use crossterm::event::{self, Event};
use ratatui::DefaultTerminal;

use fuzzy_matcher::skim::SkimMatcherV2;

use crate::board::age::run_auto_close;
use crate::board::storage::{find_kando_dir, load_board, load_trash, save_board, trash_card, restore_card, TrashEntry};
use crate::board::sync::{self, SyncState};
use crate::board::{Board, Card, Column};
use crate::input::action::Action;
use crate::input::keymap::map_key;

/// Reusable text editing buffer with cursor.
///
/// `cursor` is a **char index** (not byte index), always in `0..=char_count`.
#[derive(Debug, Clone)]
pub struct TextBuffer {
    pub input: String,
    pub cursor: usize,
}

impl TextBuffer {
    pub fn new(input: String) -> Self {
        let cursor = input.chars().count();
        Self { input, cursor }
    }

    pub fn empty() -> Self {
        Self { input: String::new(), cursor: 0 }
    }

    /// Convert a char index to a byte index.
    fn byte_offset(&self, char_idx: usize) -> usize {
        self.input
            .char_indices()
            .nth(char_idx)
            .map(|(i, _)| i)
            .unwrap_or(self.input.len())
    }

    pub fn insert(&mut self, c: char) {
        let byte_idx = self.byte_offset(self.cursor);
        self.input.insert(byte_idx, c);
        self.cursor += 1;
    }

    pub fn backspace(&mut self) {
        if self.cursor > 0 {
            let byte_idx = self.byte_offset(self.cursor - 1);
            self.input.remove(byte_idx);
            self.cursor -= 1;
        }
    }

    pub fn delete_word(&mut self) {
        let byte_pos = self.byte_offset(self.cursor);
        let before = &self.input[..byte_pos];
        let trimmed = before.trim_end();
        let start_byte = trimmed
            .char_indices()
            .rev()
            .find(|(_, c)| c.is_whitespace())
            .map(|(i, c)| i + c.len_utf8()) // byte after the whitespace char
            .unwrap_or(0);
        // Convert start_byte back to char index
        let start_char = self.input[..start_byte].chars().count();
        self.input.drain(start_byte..byte_pos);
        self.cursor = start_char;
    }

    pub fn move_left(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor < self.input.chars().count() {
            self.cursor += 1;
        }
    }

    pub fn home(&mut self) {
        self.cursor = 0;
    }

    pub fn end(&mut self) {
        self.cursor = self.input.chars().count();
    }
}

/// Current interaction mode.
#[derive(Debug, Clone)]
pub enum Mode {
    Normal,
    Goto,
    Space,
    View,
    FilterMenu,
    Input {
        prompt: &'static str,
        buf: TextBuffer,
        on_confirm: InputTarget,
    },
    Confirm {
        prompt: &'static str,
        on_confirm: ConfirmTarget,
    },
    Filter {
        buf: TextBuffer,
    },
    Picker {
        title: &'static str,
        items: Vec<(String, bool)>,
        selected: usize,
        target: PickerTarget,
    },
    Command {
        cmd: crate::command::CommandState,
    },
    CardDetail {
        scroll: u16,
    },
    Metrics {
        scroll: u16,
    },
    Tutorial,
    Help,
}

#[derive(Debug, Clone)]
pub enum InputTarget {
    NewCardTitle,
    EditTags,
    EditAssignees,
}

#[derive(Debug, Clone)]
pub enum ConfirmTarget {
    DeleteCard(String), // card id
    WipLimitMove {
        from_col: usize,
        card_idx: usize,
        to_col: usize,
    },
}

#[derive(Debug, Clone)]
pub enum PickerTarget {
    Priority,
    MoveToColumn,
    TagFilter,
    AssigneeFilter,
}

/// Notification severity for statusbar coloring.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotificationLevel {
    Info,
    Error,
}

/// Global application state.
pub struct AppState {
    pub mode: Mode,
    pub focused_column: usize,
    pub selected_card: usize,
    pub show_hidden_columns: bool,
    pub active_filter: Option<String>,
    pub active_tag_filters: Vec<String>,
    pub active_assignee_filters: Vec<String>,
    pub notification: Option<String>,
    pub notification_level: NotificationLevel,
    pub notification_expires: Option<Instant>,
    pub should_quit: bool,
    pub sync_state: Option<SyncState>,
    /// Last deleted card for single-level undo (`u` key).
    pub last_delete: Option<TrashEntry>,
    /// True if a card was deleted this session and `last_delete` hasn't been
    /// cleared by a subsequent mutation. Used to gate cross-session undo so
    /// that `u` doesn't silently restore an unrelated card from a previous
    /// session when the user simply hasn't deleted anything this session.
    pub deleted_this_session: bool,
    /// Cached trash entry IDs for `:restore` command palette/completion.
    pub cached_trash_ids: Vec<(String, String)>, // (id, title)
    /// Use Nerd Font glyphs instead of ASCII icons.
    pub nerd_font: bool,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            mode: Mode::Normal,
            focused_column: 0,
            selected_card: 0,
            show_hidden_columns: false,
            active_filter: None,
            active_tag_filters: Vec::new(),
            active_assignee_filters: Vec::new(),
            notification: None,
            notification_level: NotificationLevel::Info,
            notification_expires: None,
            should_quit: false,
            sync_state: None,
            last_delete: None,
            deleted_this_session: false,
            cached_trash_ids: Vec::new(),
            nerd_font: false,
        }
    }

    /// Get a reference to the currently selected card.
    pub fn selected_card_ref<'a>(&self, board: &'a Board) -> Option<&'a Card> {
        board
            .columns
            .get(self.focused_column)
            .and_then(|col| col.cards.get(self.selected_card))
    }

    /// Get the tags and assignees of the selected card (for completion context).
    pub fn selected_card_metadata(&self, board: &Board) -> (Vec<String>, Vec<String>) {
        self.selected_card_ref(board)
            .map(|c| (c.tags.clone(), c.assignees.clone()))
            .unwrap_or_default()
    }

    /// Show a transient notification.
    pub fn notify(&mut self, msg: impl Into<String>) {
        self.notification = Some(msg.into());
        self.notification_level = NotificationLevel::Info;
        self.notification_expires = Some(Instant::now() + Duration::from_secs(3));
    }

    /// Show a transient error notification (rendered in red).
    pub fn notify_error(&mut self, msg: impl Into<String>) {
        self.notification = Some(msg.into());
        self.notification_level = NotificationLevel::Error;
        self.notification_expires = Some(Instant::now() + Duration::from_secs(3));
    }

    /// Clear expired notifications.
    pub fn tick_notification(&mut self) {
        if let Some(expires) = self.notification_expires {
            if Instant::now() >= expires {
                self.notification = None;
                self.notification_level = NotificationLevel::Info;
                self.notification_expires = None;
            }
        }
    }

    /// Clamp the selected card index to the column's card count.
    pub fn clamp_selection(&mut self, board: &Board) {
        if let Some(col) = board.columns.get(self.focused_column) {
            if col.cards.is_empty() {
                self.selected_card = 0;
            } else if self.selected_card >= col.cards.len() {
                self.selected_card = col.cards.len() - 1;
            }
        }
    }

    /// Whether any card filter is currently active.
    pub fn has_active_filter(&self) -> bool {
        self.active_filter.is_some()
            || !self.active_tag_filters.is_empty()
            || !self.active_assignee_filters.is_empty()
    }

    /// Clamp selection to the nearest visible card under active filters.
    /// Falls back to the regular unfiltered clamp when no filters are active.
    pub fn clamp_selection_filtered(&mut self, board: &Board) {
        if !self.has_active_filter() {
            self.clamp_selection(board);
            return;
        }
        if let Some(col) = board.columns.get(self.focused_column) {
            let visible = visible_card_indices(col, self);
            if visible.is_empty() {
                self.selected_card = 0;
            } else if !visible.contains(&self.selected_card) {
                // Snap to nearest visible card
                self.selected_card = *visible
                    .iter()
                    .min_by_key(|&&idx| {
                        (idx as isize - self.selected_card as isize).unsigned_abs()
                    })
                    .expect("visible is non-empty");
            }
        }
    }
}

/// Return the unfiltered indices of cards visible under the current filters.
fn visible_card_indices(col: &Column, state: &AppState) -> Vec<usize> {
    let matcher = SkimMatcherV2::default();
    col.cards
        .iter()
        .enumerate()
        .filter(|(_, card)| {
            crate::board::card_is_visible(
                card,
                state.active_filter.as_deref(),
                &state.active_tag_filters,
                &state.active_assignee_filters,
                &matcher,
            )
        })
        .map(|(i, _)| i)
        .collect()
}

/// Sync `active_filter` from the current filter buffer.
fn sync_filter(state: &mut AppState) {
    if let Mode::Filter { buf } = &state.mode {
        state.active_filter = if buf.input.is_empty() {
            None
        } else {
            Some(buf.input.clone())
        };
    }
}

/// Find the next visible column in a given direction from `from` (exclusive).
/// Returns `None` if no visible column exists in that direction.
fn next_visible_column(board: &Board, from: usize, forward: bool, show_hidden: bool) -> Option<usize> {
    if forward {
        (from + 1..board.columns.len())
            .find(|&i| show_hidden || !board.columns[i].hidden)
    } else {
        (0..from)
            .rev()
            .find(|&i| show_hidden || !board.columns[i].hidden)
    }
}

/// Run auto-close, save if any cards were closed, sync, and notify.
fn handle_auto_close(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<()> {
    let mut messages = Vec::new();

    let closed = run_auto_close(board, Utc::now());
    if !closed.is_empty() {
        save_board(kando_dir, board)?;
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, kando_dir, "Auto-close stale cards");
        }
        messages.push(format!(
            "{} card{} auto-closed",
            closed.len(),
            if closed.len() == 1 { "" } else { "s" }
        ));
    }

    // Purge old trash entries
    use crate::board::storage::purge_trash;
    if let Ok(purged) = purge_trash(kando_dir, board.policies.trash_purge_days) {
        if !purged.is_empty() {
            // Invalidate undo if the purged card was the last-deleted one
            if let Some(ref ld) = state.last_delete {
                if purged.contains(&ld.id) {
                    state.last_delete = None;
                }
            }
            messages.push(format!(
                "{} trashed card{} purged",
                purged.len(),
                if purged.len() == 1 { "" } else { "s" }
            ));
        }
    }

    if !messages.is_empty() {
        state.notify(messages.join("; "));
    }

    Ok(())
}

/// Try to move the selected card to an adjacent visible column.
/// Returns `Ok(Some(sync_msg))` if the card was moved, `Ok(None)` if WIP confirmation
/// was triggered or no move was possible.
fn try_move_card(
    board: &mut Board,
    state: &mut AppState,
    forward: bool,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let to = match next_visible_column(board, state.focused_column, forward, state.show_hidden_columns) {
        Some(to) => to,
        None => return Ok(None),
    };
    let col = match board.columns.get(state.focused_column) {
        Some(col) if !col.cards.is_empty() => col,
        _ => return Ok(None),
    };
    let card_idx = state.selected_card.min(col.cards.len() - 1);
    let from = state.focused_column;

    if board.columns[to].is_over_wip_limit() {
        state.mode = Mode::Confirm {
            prompt: "Column over WIP limit, move anyway?",
            on_confirm: ConfirmTarget::WipLimitMove { from_col: from, card_idx, to_col: to },
        };
        return Ok(None);
    }

    board.move_card(from, card_idx, to);
    board.columns[to].sort_cards();
    state.focused_column = to;
    state.clamp_selection_filtered(board);
    save_board(kando_dir, board)?;
    state.notify("Card moved");
    Ok(Some("Move card".into()))
}

/// Main TUI application loop.
pub fn run(terminal: &mut DefaultTerminal, start_dir: &std::path::Path, nerd_font_flag: bool) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(start_dir)?;
    let mut board = load_board(&kando_dir)?;
    let mut state = AppState::new();

    // CLI --nerd-font flag overrides config; otherwise use board config value
    state.nerd_font = nerd_font_flag || board.nerd_font;

    // Initialize git sync if configured
    if let Some(ref branch) = board.sync_branch {
        match sync::init_shadow(&kando_dir, branch) {
            Ok(mut sync_state) => {
                let status = sync::pull(&mut sync_state, &kando_dir);
                if status == sync::SyncStatus::Updated {
                    // Reload board after pulling
                    board = load_board(&kando_dir)?;
                    state.notify("Synced from remote");
                }
                state.sync_state = Some(sync_state);
            }
            Err(e) => {
                state.notify(format!("Sync init failed: {e}"));
            }
        }
    }

    // Run auto-close on startup
    handle_auto_close(&mut board, &mut state, &kando_dir)?;

    state.clamp_selection(&board);

    // Show tutorial on first launch
    if !board.tutorial_shown {
        state.mode = Mode::Tutorial;
    }

    let mut last_auto_close = Instant::now();
    let auto_close_interval = Duration::from_secs(60);
    let mut last_sync = Instant::now();
    let sync_interval = Duration::from_secs(30);

    loop {
        // Tick
        state.tick_notification();

        // Periodic auto-close
        if last_auto_close.elapsed() >= auto_close_interval {
            handle_auto_close(&mut board, &mut state, &kando_dir)?;
            last_auto_close = Instant::now();
        }

        // Periodic sync pull
        if last_sync.elapsed() >= sync_interval {
            if let Some(ref mut sync_state) = state.sync_state {
                let status = sync::pull(sync_state, &kando_dir);
                if status == sync::SyncStatus::Updated {
                    if let Ok(new_board) = load_board(&kando_dir) {
                        board = new_board;
                        state.clamp_selection(&board);
                        state.notify("Synced from remote");
                    }
                }
            }
            last_sync = Instant::now();
        }

        // Render
        let now = Utc::now();
        terminal.draw(|f| crate::ui::render(f, &board, &state, now))?;

        // Handle input
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                let action = map_key(key, &state.mode);
                process_action(&mut board, &mut state, action, terminal, &kando_dir)?;

                if state.should_quit {
                    break;
                }
            }
        }
    }

    Ok(())
}

fn process_action(
    board: &mut Board,
    state: &mut AppState,
    action: Action,
    terminal: &mut DefaultTerminal,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<()> {
    let was_minor_mode = matches!(state.mode, Mode::Goto | Mode::Space | Mode::View | Mode::FilterMenu);
    let mut sync_message: Option<String> = None;

    match action {
        Action::None => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
        }

        // Navigation
        Action::FocusPrevColumn
        | Action::FocusNextColumn
        | Action::SelectPrevCard
        | Action::SelectNextCard
        | Action::CycleNextCard
        | Action::CyclePrevCard => {
            handle_navigation(board, state, action, was_minor_mode);
        }

        // Goto / Jump
        Action::JumpToColumn(_)
        | Action::JumpToFirstCard
        | Action::JumpToLastCard
        | Action::JumpToBacklog
        | Action::JumpToDone => {
            handle_goto(board, state, action);
        }

        // Card movement & actions
        Action::MoveCardPrevColumn
        | Action::MoveCardNextColumn
        | Action::NewCard
        | Action::DeleteCard
        | Action::EditCardExternal
        | Action::CyclePriority
        | Action::PickPriority
        | Action::MoveToColumn
        | Action::ToggleBlocker
        | Action::EditTags
        | Action::EditAssignees
        | Action::OpenCardDetail
        | Action::ClosePanel
        | Action::DetailScrollDown
        | Action::DetailScrollUp
        | Action::DetailNextCard
        | Action::DetailPrevCard => {
            sync_message = handle_card_action(board, state, action, terminal, kando_dir, was_minor_mode)?;
        }

        // View toggles
        Action::ToggleHiddenColumns => {
            handle_view_toggle(board, state, action);
        }

        // Filter / tag filter
        Action::StartFilter | Action::StartTagFilter | Action::StartAssigneeFilter => {
            handle_filter_start(board, state, action);
        }

        // Text input delegation
        Action::InputChar(_)
        | Action::InputBackspace
        | Action::InputLeft
        | Action::InputRight
        | Action::InputHome
        | Action::InputEnd
        | Action::InputDeleteWord
        | Action::InputComplete
        | Action::InputCompleteBack
        | Action::InputConfirm
        | Action::InputCancel => {
            sync_message = handle_input(board, state, action, kando_dir)?;
        }

        // Confirmation
        Action::Confirm | Action::Deny => {
            sync_message = handle_confirm(board, state, action, kando_dir)?;
        }

        // Mode entry
        Action::EnterGotoMode => state.mode = Mode::Goto,
        Action::EnterSpaceMode => state.mode = Mode::Space,
        Action::EnterViewMode => state.mode = Mode::View,
        Action::EnterFilterMode => state.mode = Mode::FilterMenu,
        Action::EnterCommandMode => {
            state.cached_trash_ids = load_trash(kando_dir)
                .into_iter()
                .map(|e| (e.id, e.title))
                .collect();
            state.mode = Mode::Command { cmd: crate::command::CommandState::new() };
        }

        // Undo last delete
        Action::Undo => {
            if was_minor_mode { state.mode = Mode::Normal; }
            sync_message = handle_undo(board, state, kando_dir)?;
        }

        // Board-level actions
        Action::ReloadBoard => {
            state.mode = Mode::Normal;
            *board = load_board(kando_dir)?;
            state.clamp_selection_filtered(board);
            state.notify("Board reloaded");
        }
        Action::ShowHelp => state.mode = Mode::Help,
        Action::ShowMetrics => state.mode = Mode::Metrics { scroll: 0 },
        Action::DismissTutorial => {
            state.mode = Mode::Normal;
            board.tutorial_shown = true;
            save_board(kando_dir, board)?;
        }
        Action::Quit => {
            match &state.mode {
                Mode::Normal => state.should_quit = true,
                _ => state.mode = Mode::Normal,
            }
        }
        Action::ClearFilters => {
            if state.active_filter.is_some() || !state.active_tag_filters.is_empty() || !state.active_assignee_filters.is_empty() {
                state.active_filter = None;
                state.active_tag_filters.clear();
                state.active_assignee_filters.clear();
                state.notify("Filters cleared");
            }
        }
    }

    // Sync to remote if any mutation was saved
    if let Some(msg) = sync_message {
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, kando_dir, &msg);
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Handler: Navigation (column focus, card selection, cycling)
// ---------------------------------------------------------------------------

fn handle_navigation(board: &Board, state: &mut AppState, action: Action, was_minor_mode: bool) {
    match action {
        Action::FocusPrevColumn => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = next_visible_column(board, state.focused_column, false, state.show_hidden_columns) {
                state.focused_column = col;
                state.clamp_selection_filtered(board);
            }
        }
        Action::FocusNextColumn => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = next_visible_column(board, state.focused_column, true, state.show_hidden_columns) {
                state.focused_column = col;
                state.clamp_selection_filtered(board);
            }
        }
        Action::SelectPrevCard => {
            match &mut state.mode {
                Mode::Picker { selected, .. } => {
                    if *selected > 0 { *selected -= 1; }
                }
                _ => {
                    if was_minor_mode { state.mode = Mode::Normal; }
                    if let Some(col) = board.columns.get(state.focused_column) {
                        let visible = visible_card_indices(col, state);
                        if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                            if pos > 0 {
                                state.selected_card = visible[pos - 1];
                            }
                        } else if let Some(&first) = visible.first() {
                            state.selected_card = first;
                        }
                    }
                }
            }
        }
        Action::SelectNextCard => {
            match &mut state.mode {
                Mode::Picker { selected, items, .. } => {
                    if *selected + 1 < items.len() { *selected += 1; }
                }
                _ => {
                    if was_minor_mode { state.mode = Mode::Normal; }
                    if let Some(col) = board.columns.get(state.focused_column) {
                        let visible = visible_card_indices(col, state);
                        if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                            if pos + 1 < visible.len() {
                                state.selected_card = visible[pos + 1];
                            }
                        } else if let Some(&first) = visible.first() {
                            state.selected_card = first;
                        }
                    }
                }
            }
        }
        Action::CycleNextCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                let pos = visible.iter().position(|&i| i == state.selected_card);
                let advanced = match pos {
                    Some(p) if p + 1 < visible.len() => {
                        state.selected_card = visible[p + 1];
                        true
                    }
                    _ => false,
                };
                if !advanced {
                    let mut from = state.focused_column;
                    while let Some(next) = next_visible_column(board, from, true, state.show_hidden_columns) {
                        let next_visible = visible_card_indices(&board.columns[next], state);
                        if let Some(&first) = next_visible.first() {
                            state.focused_column = next;
                            state.selected_card = first;
                            break;
                        }
                        from = next;
                    }
                }
            }
        }
        Action::CyclePrevCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                let pos = visible.iter().position(|&i| i == state.selected_card);
                let retreated = match pos {
                    Some(p) if p > 0 => {
                        state.selected_card = visible[p - 1];
                        true
                    }
                    _ => false,
                };
                if !retreated {
                    let mut from = state.focused_column;
                    while let Some(prev) = next_visible_column(board, from, false, state.show_hidden_columns) {
                        let prev_visible = visible_card_indices(&board.columns[prev], state);
                        if let Some(&last) = prev_visible.last() {
                            state.focused_column = prev;
                            state.selected_card = last;
                            break;
                        }
                        from = prev;
                    }
                }
            }
        }
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Handler: Goto / Jump actions
// ---------------------------------------------------------------------------

fn handle_goto(board: &Board, state: &mut AppState, action: Action) {
    state.mode = Mode::Normal;
    match action {
        Action::JumpToColumn(idx) => {
            let visible: Vec<usize> = board
                .columns
                .iter()
                .enumerate()
                .filter(|(_, c)| !c.hidden || state.show_hidden_columns)
                .map(|(i, _)| i)
                .collect();
            if idx < visible.len() {
                state.focused_column = visible[idx];
                state.clamp_selection_filtered(board);
            }
        }
        Action::JumpToFirstCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                if let Some(&first) = visible.first() {
                    state.selected_card = first;
                }
            }
        }
        Action::JumpToLastCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                if let Some(&last) = visible.last() {
                    state.selected_card = last;
                }
            }
        }
        Action::JumpToBacklog => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "backlog" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.clamp_selection_filtered(board);
            }
        }
        Action::JumpToDone => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "done" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.clamp_selection_filtered(board);
            }
        }
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Handler: Card actions (CRUD, priority, tags, movement, detail view)
// ---------------------------------------------------------------------------

fn handle_card_action<B: ratatui::backend::Backend>(
    board: &mut Board,
    state: &mut AppState,
    action: Action,
    terminal: &mut ratatui::Terminal<B>,
    kando_dir: &std::path::Path,
    was_minor_mode: bool,
) -> color_eyre::Result<Option<String>> {
    let mut sync_message = None;

    // Clear undo state on any card-mutating action (except delete itself, which sets it).
    if !matches!(action, Action::DeleteCard | Action::OpenCardDetail | Action::ClosePanel
        | Action::DetailScrollDown | Action::DetailScrollUp | Action::DetailNextCard | Action::DetailPrevCard) {
        state.last_delete = None;
        state.deleted_this_session = false;
    }

    // Guard: skip card-targeting actions when the selected card is filtered out.
    // Actions like NewCard, ClosePanel, detail scrolling/navigation don't target a specific card
    // (detail nav does its own visible_card_indices check internally).
    let needs_visible_card = !matches!(
        action,
        Action::NewCard
            | Action::ClosePanel
            | Action::DetailScrollDown
            | Action::DetailScrollUp
            | Action::DetailNextCard
            | Action::DetailPrevCard
    );
    if needs_visible_card && state.has_active_filter() {
        if let Some(col) = board.columns.get(state.focused_column) {
            let visible = visible_card_indices(col, state);
            if !visible.contains(&state.selected_card) {
                return Ok(None);
            }
        }
    }

    match action {
        Action::MoveCardPrevColumn => {
            sync_message = try_move_card(board, state, false, kando_dir)?;
        }
        Action::MoveCardNextColumn => {
            sync_message = try_move_card(board, state, true, kando_dir)?;
        }
        Action::NewCard => {
            state.mode = Mode::Input {
                prompt: "New card",
                buf: TextBuffer::empty(),
                on_confirm: InputTarget::NewCardTitle,
            };
        }
        Action::DeleteCard => {
            if let Some(card) = state.selected_card_ref(board) {
                let id = card.id.clone();
                state.mode = Mode::Confirm {
                    prompt: "Delete card?",
                    on_confirm: ConfirmTarget::DeleteCard(id),
                };
            }
        }
        Action::EditCardExternal => {
            state.mode = Mode::Normal;
            let card_info = state.selected_card_ref(board).map(|c| {
                (c.id.clone(), board.columns[state.focused_column].slug.clone())
            });
            if let Some((card_id, col_slug)) = card_info {
                let card_path = kando_dir.join("columns").join(&col_slug).join(format!("{card_id}.md"));

                save_board(kando_dir, board)?;

                let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vi".to_string());
                let editor_trimmed = editor.trim();
                if editor_trimmed.is_empty() {
                    state.notify_error("$EDITOR is empty");
                    return Ok(None);
                }

                crossterm::terminal::disable_raw_mode()?;
                crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen)?;

                // Use sh -c so $EDITOR with arguments (e.g. "code --wait") works correctly
                let editor_result = std::process::Command::new("sh")
                    .arg("-c")
                    .arg(format!("{editor_trimmed} \"$1\""))
                    .arg("--") // $0
                    .arg(&card_path) // $1
                    .status();

                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::EnterAlternateScreen,
                )?;
                crossterm::terminal::enable_raw_mode()?;

                terminal.clear()?;

                // Always reload — the user may have saved before the editor exited
                *board = load_board(kando_dir)?;
                if let Some((col_idx, card_idx)) = board.find_card(&card_id) {
                    board.columns[col_idx].cards[card_idx].touch();
                    board.columns[col_idx].sort_cards();
                    save_board(kando_dir, board)?;
                    sync_message = Some("Edit card".into());
                }
                state.clamp_selection_filtered(board);

                match editor_result {
                    Ok(status) if status.success() => {
                        state.notify("Card updated");
                    }
                    Ok(status) => {
                        state.notify_error(format!("Editor exited with {status}"));
                    }
                    Err(e) => {
                        state.notify_error(format!("Failed to launch editor: {e}"));
                    }
                }
            }
        }
        Action::CyclePriority => {
            if was_minor_mode { state.mode = Mode::Normal; }
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                card.priority = card.priority.next();
                card.touch();
                let priority_str = format!("Priority: {}", card.priority);
                board.columns[col_idx].sort_cards();
                save_board(kando_dir, board)?;
                sync_message = Some("Change priority".into());
                state.clamp_selection_filtered(board);
                state.notify(priority_str);
            }
        }
        Action::PickPriority => {
            use crate::board::Priority;
            state.mode = Mode::Normal;
            if let Some(card) = state.selected_card_ref(board) {
                let current = card.priority;
                let items: Vec<(String, bool)> = Priority::ALL
                    .iter()
                    .map(|p| (p.as_str().to_string(), *p == current))
                    .collect();
                let selected = Priority::ALL.iter().position(|p| *p == current).unwrap_or(0);
                state.mode = Mode::Picker {
                    title: "priority",
                    items,
                    selected,
                    target: PickerTarget::Priority,
                };
            }
        }
        Action::MoveToColumn => {
            state.mode = Mode::Normal;
            if state.selected_card_ref(board).is_some() {
                let items: Vec<(String, bool)> = board
                    .columns
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| *i != state.focused_column)
                    .map(|(_, col)| (col.name.clone(), false))
                    .collect();
                if items.is_empty() {
                    state.notify("No other columns");
                } else {
                    state.mode = Mode::Picker {
                        title: "move to column",
                        items,
                        selected: 0,
                        target: PickerTarget::MoveToColumn,
                    };
                }
            }
        }
        Action::ToggleBlocker => {
            if was_minor_mode { state.mode = Mode::Normal; }
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                card.blocked = !card.blocked;
                card.touch();
                let msg = if card.blocked { "Card blocked" } else { "Blocker removed" };
                board.columns[col_idx].sort_cards();
                save_board(kando_dir, board)?;
                sync_message = Some("Toggle blocker".into());
                state.clamp_selection_filtered(board);
                state.notify(msg);
            }
        }
        Action::EditTags => {
            if let Some(card) = state.selected_card_ref(board) {
                let current = card.tags.join(", ");
                state.mode = Mode::Input {
                    prompt: "Tags (comma-separated)",
                    buf: TextBuffer::new(current),
                    on_confirm: InputTarget::EditTags,
                };
            } else {
                state.mode = Mode::Normal;
            }
        }
        Action::EditAssignees => {
            if let Some(card) = state.selected_card_ref(board) {
                let current = card.assignees.join(", ");
                state.mode = Mode::Input {
                    prompt: "Assignees (comma-separated)",
                    buf: TextBuffer::new(current),
                    on_confirm: InputTarget::EditAssignees,
                };
            } else {
                state.mode = Mode::Normal;
            }
        }
        Action::OpenCardDetail => {
            if state.selected_card_ref(board).is_some() {
                state.mode = Mode::CardDetail { scroll: 0 };
            }
        }
        Action::ClosePanel => {
            state.mode = Mode::Normal;
        }
        Action::DetailScrollDown => {
            match &mut state.mode {
                Mode::CardDetail { scroll } | Mode::Metrics { scroll } => {
                    *scroll = scroll.saturating_add(1);
                }
                _ => {}
            }
        }
        Action::DetailScrollUp => {
            match &mut state.mode {
                Mode::CardDetail { scroll } | Mode::Metrics { scroll } => {
                    *scroll = scroll.saturating_sub(1);
                }
                _ => {}
            }
        }
        Action::DetailNextCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                    if pos + 1 < visible.len() {
                        state.selected_card = visible[pos + 1];
                        state.mode = Mode::CardDetail { scroll: 0 };
                    }
                }
            }
        }
        Action::DetailPrevCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state);
                if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                    if pos > 0 {
                        state.selected_card = visible[pos - 1];
                        state.mode = Mode::CardDetail { scroll: 0 };
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    Ok(sync_message)
}

// ---------------------------------------------------------------------------
// Handler: View toggles
// ---------------------------------------------------------------------------

fn handle_view_toggle(board: &Board, state: &mut AppState, action: Action) {
    state.mode = Mode::Normal;
    match action {
        Action::ToggleHiddenColumns => {
            state.show_hidden_columns = !state.show_hidden_columns;
            if !state.show_hidden_columns {
                if let Some(col) = board.columns.get(state.focused_column) {
                    if col.hidden {
                        if let Some(idx) = board.columns.iter().position(|c| !c.hidden) {
                            state.focused_column = idx;
                            state.clamp_selection_filtered(board);
                        }
                    }
                }
            }
            state.notify(if state.show_hidden_columns {
                "Showing hidden columns"
            } else {
                "Hiding hidden columns"
            });
        }
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Handler: Filter / tag filter start
// ---------------------------------------------------------------------------

fn handle_filter_start(board: &Board, state: &mut AppState, action: Action) {
    match action {
        Action::StartFilter => {
            state.mode = Mode::Filter {
                buf: TextBuffer::empty(),
            };
        }
        Action::StartTagFilter => {
            let tags = board.all_tags();
            if tags.is_empty() {
                state.mode = Mode::Normal;
                state.notify("No tags on the board");
            } else {
                let items: Vec<(String, bool)> = tags
                    .iter()
                    .map(|(tag, _count)| {
                        let active = state.active_tag_filters.contains(tag);
                        (tag.clone(), active)
                    })
                    .collect();
                state.mode = Mode::Picker {
                    title: "filter by tag",
                    items,
                    selected: 0,
                    target: PickerTarget::TagFilter,
                };
            }
        }
        Action::StartAssigneeFilter => {
            let assignees = board.all_assignees();
            if assignees.is_empty() {
                state.mode = Mode::Normal;
                state.notify("No assignees on the board");
            } else {
                let items: Vec<(String, bool)> = assignees
                    .iter()
                    .map(|(name, _count)| {
                        let active = state.active_assignee_filters.contains(name);
                        (name.clone(), active)
                    })
                    .collect();
                state.mode = Mode::Picker {
                    title: "filter by assignee",
                    items,
                    selected: 0,
                    target: PickerTarget::AssigneeFilter,
                };
            }
        }
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Handler: Text input (char entry, cursor movement, confirm, cancel)
// ---------------------------------------------------------------------------

fn handle_input(
    board: &mut Board,
    state: &mut AppState,
    action: Action,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let mut sync_message = None;

    match action {
        Action::InputChar(c) => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.insert(c),
                Mode::Command { cmd } => { cmd.buf.insert(c); crate::command::clear_completion(cmd); }
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
        }
        Action::InputBackspace => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.backspace(),
                Mode::Command { cmd } => { cmd.buf.backspace(); crate::command::clear_completion(cmd); }
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
        }
        Action::InputLeft => {
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.move_left(),
                Mode::Command { cmd } => { crate::command::clear_completion(cmd); cmd.buf.move_left(); }
                _ => {}
            }
        }
        Action::InputRight => {
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.move_right(),
                Mode::Command { cmd } => { crate::command::clear_completion(cmd); cmd.buf.move_right(); }
                _ => {}
            }
        }
        Action::InputHome => {
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.home(),
                Mode::Command { cmd } => { crate::command::clear_completion(cmd); cmd.buf.home(); }
                _ => {}
            }
        }
        Action::InputEnd => {
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.end(),
                Mode::Command { cmd } => { crate::command::clear_completion(cmd); cmd.buf.end(); }
                _ => {}
            }
        }
        Action::InputDeleteWord => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.delete_word(),
                Mode::Command { cmd } => { cmd.buf.delete_word(); crate::command::clear_completion(cmd); }
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
        }
        Action::InputConfirm => {
            sync_message = handle_input_confirm(board, state, kando_dir)?;
        }
        Action::InputCancel => {
            match &state.mode {
                Mode::Filter { .. } => {
                    state.active_filter = None;
                }
                Mode::Picker { .. } | Mode::Command { .. } => {
                    // Just close — no side effects
                }
                _ => {}
            }
            state.mode = Mode::Normal;
        }
        Action::InputComplete | Action::InputCompleteBack => {
            let forward = matches!(action, Action::InputComplete);
            // Extract card data before mutably borrowing state.mode
            let (card_tags, card_assignees) = state.selected_card_metadata(board);
            let trash_ids = state.cached_trash_ids.clone();
            if let Mode::Command { cmd } = &mut state.mode {
                crate::command::cycle_completion(cmd, board, &card_tags, &card_assignees, &trash_ids, forward);
            }
        }
        _ => unreachable!(),
    }

    Ok(sync_message)
}

/// Process InputConfirm for Input, Filter, and Picker modes.
fn handle_input_confirm(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let mut sync_message = None;
    let old_mode = std::mem::replace(&mut state.mode, Mode::Normal);

    match old_mode {
        Mode::Input {
            buf,
            on_confirm: InputTarget::NewCardTitle,
            ..
        } => {
            let title = buf.input.trim().to_string();
            if !title.is_empty() {
                let id = board.next_card_id();
                let card = Card::new(id, title);
                if let Some(col) = board.columns.get_mut(state.focused_column) {
                    col.cards.push(card);
                    col.sort_cards();
                }
                save_board(kando_dir, board)?;
                sync_message = Some("Create card".into());
                state.notify("Card created");
            }
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::EditTags,
            ..
        } => {
            let tags: Vec<String> = buf.input
                .split(',')
                .map(|t| t.trim().to_lowercase())
                .filter(|t| !t.is_empty())
                .collect();
            if let Some(col) = board.columns.get_mut(state.focused_column) {
                if let Some(card) = col.cards.get_mut(state.selected_card) {
                    card.tags = tags;
                    card.touch();
                }
                col.sort_cards();
            }
            save_board(kando_dir, board)?;
            sync_message = Some("Update tags".into());
            state.clamp_selection_filtered(board);
            state.notify("Tags updated");
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::EditAssignees,
            ..
        } => {
            let assignees: Vec<String> = buf.input
                .split(',')
                .map(|a| a.trim().to_lowercase())
                .filter(|a| !a.is_empty())
                .collect();
            if let Some(col) = board.columns.get_mut(state.focused_column) {
                if let Some(card) = col.cards.get_mut(state.selected_card) {
                    card.assignees = assignees;
                    card.touch();
                }
                col.sort_cards();
            }
            save_board(kando_dir, board)?;
            sync_message = Some("Update assignees".into());
            state.clamp_selection_filtered(board);
            state.notify("Assignees updated");
        }
        Mode::Filter { buf } => {
            if buf.input.trim().is_empty() {
                state.active_filter = None;
            } else {
                state.active_filter = Some(buf.input);
            }
            state.clamp_selection_filtered(board);
        }
        Mode::Picker { mut items, selected, target, title } => {
            match target {
                PickerTarget::TagFilter => {
                    if let Some((tag, _)) = items.get(selected) {
                        let tag = tag.clone();
                        if state.active_tag_filters.contains(&tag) {
                            state.active_tag_filters.retain(|t| *t != tag);
                        } else {
                            state.active_tag_filters.push(tag);
                        }
                    }
                    for (tag, active) in items.iter_mut() {
                        *active = state.active_tag_filters.contains(tag);
                    }
                    state.clamp_selection_filtered(board);
                    state.mode = Mode::Picker { title, items, selected, target: PickerTarget::TagFilter };
                }
                PickerTarget::AssigneeFilter => {
                    if let Some((name, _)) = items.get(selected) {
                        let name = name.clone();
                        if state.active_assignee_filters.contains(&name) {
                            state.active_assignee_filters.retain(|a| *a != name);
                        } else {
                            state.active_assignee_filters.push(name);
                        }
                    }
                    for (name, active) in items.iter_mut() {
                        *active = state.active_assignee_filters.contains(name);
                    }
                    state.clamp_selection_filtered(board);
                    state.mode = Mode::Picker { title, items, selected, target: PickerTarget::AssigneeFilter };
                }
                PickerTarget::Priority => {
                    use crate::board::Priority;
                    if let Some(priority) = Priority::ALL.get(selected) {
                        let col_idx = state.focused_column;
                        let card_idx = state.selected_card;
                        if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                            card.priority = *priority;
                            card.touch();
                            let priority_str = format!("Priority: {}", card.priority);
                            board.columns[col_idx].sort_cards();
                            save_board(kando_dir, board)?;
                            sync_message = Some("Change priority".into());
                            state.clamp_selection_filtered(board);
                            state.notify(priority_str);
                        }
                    }
                }
                PickerTarget::MoveToColumn => {
                    if let Some((col_name, _)) = items.get(selected) {
                        let target_col = board.columns.iter().enumerate()
                            .filter(|(i, _)| *i != state.focused_column)
                            .find(|(_, c)| c.name == *col_name)
                            .map(|(i, _)| i);
                        if let Some(to) = target_col {
                            let from = state.focused_column;
                            let card_idx = state.selected_card.min(
                                board.columns[from].cards.len().saturating_sub(1)
                            );
                            board.move_card(from, card_idx, to);
                            board.columns[to].sort_cards();
                            state.focused_column = to;
                            state.clamp_selection_filtered(board);
                            save_board(kando_dir, board)?;
                            sync_message = Some("Move card".into());
                            state.notify(format!("Moved to {col_name}"));
                        }
                    }
                }
            }
        }
        Mode::Command { cmd } => {
            sync_message = crate::command::execute_command(board, state, &cmd.buf.input, kando_dir)?;
        }
        _ => {}
    }

    Ok(sync_message)
}

// ---------------------------------------------------------------------------
// Handler: Undo last delete
// ---------------------------------------------------------------------------

fn handle_undo(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    // Use the in-session last_delete if available.
    // Fall back to the most recently trashed entry in _meta.toml only if a
    // deletion actually happened this session (deleted_this_session). This
    // prevents `u` from silently restoring an unrelated card from a previous
    // session when last_delete was cleared by a subsequent mutation.
    let entry = if state.last_delete.is_some() {
        state.last_delete.take()
    } else if state.deleted_this_session {
        // last_delete was cleared (e.g. `:restore` used it), fall back to disk.
        // Sort by the `deleted` ISO-8601 string so we get the most recent entry
        // regardless of storage order in _meta.toml.
        let mut entries = load_trash(kando_dir);
        entries.sort_unstable_by(|a, b| a.deleted.cmp(&b.deleted));
        entries.pop()
    } else {
        None
    };

    let Some(entry) = entry else {
        state.notify("Nothing to undo");
        return Ok(None);
    };

    // Resolve target column — notify if we had to fall back to the first column.
    let (target_col, used_fallback) = board.columns.iter()
        .position(|c| c.slug == entry.from_column)
        .map(|i| (i, false))
        .unwrap_or((0, true));
    let target_slug = board.columns[target_col].slug.clone();
    let card_id = entry.id.clone();

    match restore_card(kando_dir, &card_id, &target_slug) {
        Ok(()) => {
            // last_delete was already consumed by .take() above; just clear the session flag.
            state.deleted_this_session = false;
            *board = load_board(kando_dir)?;
            if let Some((col_idx, card_idx)) = board.find_card(&card_id) {
                state.focused_column = col_idx;
                state.selected_card = card_idx;
            } else {
                state.focused_column = target_col;
                state.clamp_selection_filtered(board);
            }
            if used_fallback {
                state.notify(format!(
                    "Restored: {} (original column gone, moved to {})",
                    entry.title,
                    board.columns[target_col].name,
                ));
            } else {
                state.notify(format!("Restored: {}", entry.title));
            }
            Ok(Some("Restore card".into()))
        }
        Err(e) => {
            // Do NOT clear last_delete — let the user retry
            state.notify_error(format!("Restore failed: {e}"));
            Ok(None)
        }
    }
}

// ---------------------------------------------------------------------------
// Handler: Confirmation (delete card, WIP limit override)
// ---------------------------------------------------------------------------

fn handle_confirm(
    board: &mut Board,
    state: &mut AppState,
    action: Action,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let mut sync_message = None;

    match action {
        Action::Confirm => {
            match &state.mode {
                Mode::Confirm {
                    on_confirm: ConfirmTarget::DeleteCard(id),
                    ..
                } => {
                    let id = id.clone();
                    if let Some((col_idx, card_idx)) = board.find_card(&id) {
                        let col_slug = board.columns[col_idx].slug.clone();
                        let card_title = board.columns[col_idx].cards[card_idx].title.clone();
                        // Trash the file BEFORE removing from board (save_board deletes orphans)
                        match trash_card(kando_dir, &col_slug, &id, &card_title) {
                            Ok(entry) => {
                                board.columns[col_idx].cards.remove(card_idx);
                                save_board(kando_dir, board)?;
                                sync_message = Some("Delete card".into());
                                state.clamp_selection_filtered(board);
                                state.last_delete = Some(entry);
                                state.deleted_this_session = true;
                                state.notify("Card deleted (u to undo)");
                            }
                            Err(e) => {
                                state.notify_error(format!("Delete failed (could not trash): {e}"));
                            }
                        }
                    }
                }
                Mode::Confirm {
                    on_confirm: ConfirmTarget::WipLimitMove { from_col, card_idx, to_col },
                    ..
                } => {
                    let (from, ci, to) = (*from_col, *card_idx, *to_col);
                    board.move_card(from, ci, to);
                    board.columns[to].sort_cards();
                    state.focused_column = to;
                    state.clamp_selection_filtered(board);
                    save_board(kando_dir, board)?;
                    sync_message = Some("Move card".into());
                    state.notify("Card moved");
                }
                _ => {}
            }
            state.mode = Mode::Normal;
        }
        Action::Deny => {
            state.mode = Mode::Normal;
        }
        _ => unreachable!(),
    }

    Ok(sync_message)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::{Board, Card, Column, Policies, Priority};

    /// Create a test board with the given column names, each with the given card titles.
    fn test_board(columns: &[(&str, &[&str])]) -> Board {
        let cols = columns
            .iter()
            .enumerate()
            .map(|(i, (name, cards))| {
                let slug = name.to_lowercase().replace(' ', "-");
                Column {
                    slug,
                    name: name.to_string(),
                    order: i as u32,
                    wip_limit: None,
                    hidden: false,
                    cards: cards
                        .iter()
                        .enumerate()
                        .map(|(j, title)| Card::new(format!("{i:02}{j:02}"), title.to_string()))
                        .collect(),
                }
            })
            .collect();
        Board {
            name: "Test".into(),
            next_card_id: 100,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            nerd_font: false,
            created_at: None,
            columns: cols,
        }
    }

    // -----------------------------------------------------------------------
    // Navigation tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_focus_next_column() {
        let board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        let mut state = AppState::new();
        handle_navigation(&board, &mut state, Action::FocusNextColumn, false);
        assert_eq!(state.focused_column, 1);
        handle_navigation(&board, &mut state, Action::FocusNextColumn, false);
        assert_eq!(state.focused_column, 2);
        // At last column — stays
        handle_navigation(&board, &mut state, Action::FocusNextColumn, false);
        assert_eq!(state.focused_column, 2);
    }

    #[test]
    fn test_focus_prev_column() {
        let board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        let mut state = AppState::new();
        state.focused_column = 2;
        handle_navigation(&board, &mut state, Action::FocusPrevColumn, false);
        assert_eq!(state.focused_column, 1);
        handle_navigation(&board, &mut state, Action::FocusPrevColumn, false);
        assert_eq!(state.focused_column, 0);
        // At first column — stays
        handle_navigation(&board, &mut state, Action::FocusPrevColumn, false);
        assert_eq!(state.focused_column, 0);
    }

    #[test]
    fn test_focus_skips_hidden_columns() {
        let mut board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        board.columns[1].hidden = true;
        let mut state = AppState::new();
        handle_navigation(&board, &mut state, Action::FocusNextColumn, false);
        assert_eq!(state.focused_column, 2);
    }

    #[test]
    fn test_select_next_prev_card() {
        let board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        handle_navigation(&board, &mut state, Action::SelectNextCard, false);
        assert_eq!(state.selected_card, 1);
        handle_navigation(&board, &mut state, Action::SelectNextCard, false);
        assert_eq!(state.selected_card, 2);
        // At last card — stays
        handle_navigation(&board, &mut state, Action::SelectNextCard, false);
        assert_eq!(state.selected_card, 2);
        // Go back
        handle_navigation(&board, &mut state, Action::SelectPrevCard, false);
        assert_eq!(state.selected_card, 1);
    }

    #[test]
    fn test_select_in_picker_mode() {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Picker {
            title: "test",
            items: vec![("a".into(), false), ("b".into(), false), ("c".into(), false)],
            selected: 0,
            target: PickerTarget::Priority,
        };
        handle_navigation(&board, &mut state, Action::SelectNextCard, false);
        if let Mode::Picker { selected, .. } = &state.mode {
            assert_eq!(*selected, 1);
        } else {
            panic!("Expected Picker mode");
        }
    }

    #[test]
    fn test_cycle_next_wraps_to_next_column() {
        let board = test_board(&[("A", &["c1"]), ("B", &["c2", "c3"])]);
        let mut state = AppState::new();
        // Card 0 of column 0 — cycling forward should jump to column 1, card 0
        handle_navigation(&board, &mut state, Action::CycleNextCard, false);
        assert_eq!(state.focused_column, 1);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn test_cycle_prev_wraps_to_prev_column() {
        let board = test_board(&[("A", &["c1", "c2"]), ("B", &["c3"])]);
        let mut state = AppState::new();
        state.focused_column = 1;
        state.selected_card = 0;
        handle_navigation(&board, &mut state, Action::CyclePrevCard, false);
        assert_eq!(state.focused_column, 0);
        assert_eq!(state.selected_card, 1); // last card of prev column
    }

    #[test]
    fn test_navigation_clears_minor_mode() {
        let board = test_board(&[("A", &[]), ("B", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Goto;
        handle_navigation(&board, &mut state, Action::FocusNextColumn, true);
        assert!(matches!(state.mode, Mode::Normal));
    }

    // -----------------------------------------------------------------------
    // Goto / Jump tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_jump_to_column() {
        let board = test_board(&[("A", &["c1"]), ("B", &["c2"]), ("C", &["c3"])]);
        let mut state = AppState::new();
        handle_goto(&board, &mut state, Action::JumpToColumn(2));
        assert_eq!(state.focused_column, 2);
        assert_eq!(state.selected_card, 0);
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn test_jump_to_column_skips_hidden() {
        let mut board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        board.columns[1].hidden = true;
        let mut state = AppState::new();
        // Jump to visible index 1 — should skip hidden B and land on C (index 2)
        handle_goto(&board, &mut state, Action::JumpToColumn(1));
        assert_eq!(state.focused_column, 2);
    }

    #[test]
    fn test_jump_to_first_last_card() {
        let board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 1;
        handle_goto(&board, &mut state, Action::JumpToLastCard);
        assert_eq!(state.selected_card, 2);
        handle_goto(&board, &mut state, Action::JumpToFirstCard);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn test_jump_to_backlog() {
        let mut board = test_board(&[("Todo", &[]), ("Backlog", &["c1"])]);
        board.columns[1].slug = "backlog".to_string();
        let mut state = AppState::new();
        handle_goto(&board, &mut state, Action::JumpToBacklog);
        assert_eq!(state.focused_column, 1);
    }

    #[test]
    fn test_jump_to_done() {
        let mut board = test_board(&[("Todo", &[]), ("Done", &["c1"])]);
        board.columns[1].slug = "done".to_string();
        let mut state = AppState::new();
        handle_goto(&board, &mut state, Action::JumpToDone);
        assert_eq!(state.focused_column, 1);
    }

    // -----------------------------------------------------------------------
    // View toggle tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_toggle_hidden_columns() {
        let board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        assert!(!state.show_hidden_columns);
        handle_view_toggle(&board, &mut state, Action::ToggleHiddenColumns);
        assert!(state.show_hidden_columns);
        assert_eq!(state.notification.as_deref(), Some("Showing hidden columns"));
        handle_view_toggle(&board, &mut state, Action::ToggleHiddenColumns);
        assert!(!state.show_hidden_columns);
        assert_eq!(state.notification.as_deref(), Some("Hiding hidden columns"));
    }

    #[test]
    fn test_toggle_hidden_refocuses_if_on_hidden() {
        let mut board = test_board(&[("A", &[]), ("B", &[])]);
        board.columns[1].hidden = true;
        let mut state = AppState::new();
        state.show_hidden_columns = true;
        state.focused_column = 1;
        handle_view_toggle(&board, &mut state, Action::ToggleHiddenColumns);
        assert_eq!(state.focused_column, 0);
    }

    // -----------------------------------------------------------------------
    // Filter start tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_start_filter() {
        let board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        handle_filter_start(&board, &mut state, Action::StartFilter);
        assert!(matches!(state.mode, Mode::Filter { .. }));
    }

    #[test]
    fn test_start_tag_filter_no_tags() {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        handle_filter_start(&board, &mut state, Action::StartTagFilter);
        assert!(matches!(state.mode, Mode::Normal));
        assert_eq!(state.notification.as_deref(), Some("No tags on the board"));
    }

    #[test]
    fn test_start_tag_filter_with_tags() {
        let mut board = test_board(&[("A", &["c1"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];
        let mut state = AppState::new();
        handle_filter_start(&board, &mut state, Action::StartTagFilter);
        assert!(matches!(state.mode, Mode::Picker { target: PickerTarget::TagFilter, .. }));
    }

    // -----------------------------------------------------------------------
    // Input handler tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_input_char_in_input_mode() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "test",
            buf: TextBuffer::empty(),
            on_confirm: InputTarget::NewCardTitle,
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputChar('h'), kando_dir).unwrap();
        handle_input(&mut board, &mut state, Action::InputChar('i'), kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "hi");
            assert_eq!(buf.cursor, 2);
        } else {
            panic!("Expected Input mode");
        }
    }

    #[test]
    fn test_input_char_in_filter_syncs() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Filter { buf: TextBuffer::empty() };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputChar('x'), kando_dir).unwrap();
        assert_eq!(state.active_filter.as_deref(), Some("x"));
    }

    #[test]
    fn test_input_backspace() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "test",
            buf: TextBuffer::new("abc".into()),
            on_confirm: InputTarget::NewCardTitle,
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputBackspace, kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "ab");
        } else {
            panic!("Expected Input mode");
        }
    }

    #[test]
    fn test_input_cursor_movement() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "test",
            buf: TextBuffer::new("hello".into()),
            on_confirm: InputTarget::NewCardTitle,
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputLeft, kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode { assert_eq!(buf.cursor, 4); }
        handle_input(&mut board, &mut state, Action::InputHome, kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode { assert_eq!(buf.cursor, 0); }
        handle_input(&mut board, &mut state, Action::InputRight, kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode { assert_eq!(buf.cursor, 1); }
        handle_input(&mut board, &mut state, Action::InputEnd, kando_dir).unwrap();
        if let Mode::Input { buf, .. } = &state.mode { assert_eq!(buf.cursor, 5); }
    }

    #[test]
    fn test_input_cancel_clears_filter() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.active_filter = Some("test".into());
        state.mode = Mode::Filter { buf: TextBuffer::new("test".into()) };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputCancel, kando_dir).unwrap();
        assert!(state.active_filter.is_none());
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn test_input_cancel_picker_keeps_tag_filters() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["bug".into()];
        state.mode = Mode::Picker {
            title: "test",
            items: vec![("bug".into(), true)],
            selected: 0,
            target: PickerTarget::TagFilter,
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputCancel, kando_dir).unwrap();
        assert_eq!(state.active_tag_filters, vec!["bug".to_string()]);
        assert!(matches!(state.mode, Mode::Normal));
    }

    // -----------------------------------------------------------------------
    // InputConfirm tests (filter confirm — no I/O needed)
    // -----------------------------------------------------------------------

    #[test]
    fn test_filter_confirm_sets_active_filter() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Filter { buf: TextBuffer::new("search".into()) };
        let kando_dir = std::path::Path::new("/tmp/fake");
        let sync = handle_input_confirm(&mut board, &mut state, kando_dir).unwrap();
        assert!(sync.is_none());
        assert_eq!(state.active_filter.as_deref(), Some("search"));
    }

    #[test]
    fn test_filter_confirm_empty_clears() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.active_filter = Some("old".into());
        state.mode = Mode::Filter { buf: TextBuffer::new("  ".into()) };
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input_confirm(&mut board, &mut state, kando_dir).unwrap();
        assert!(state.active_filter.is_none());
    }

    #[test]
    fn test_tag_filter_toggle() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.mode = Mode::Picker {
            title: "filter by tag",
            items: vec![("bug".into(), false), ("ui".into(), false)],
            selected: 0,
            target: PickerTarget::TagFilter,
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        // Toggle "bug" on
        handle_input_confirm(&mut board, &mut state, kando_dir).unwrap();
        assert_eq!(state.active_tag_filters, vec!["bug".to_string()]);
        // Should stay in Picker mode
        assert!(matches!(state.mode, Mode::Picker { .. }));
        // Toggle "bug" off
        handle_input_confirm(&mut board, &mut state, kando_dir).unwrap();
        assert!(state.active_tag_filters.is_empty());
    }

    // -----------------------------------------------------------------------
    // Card action + confirm tests (with temp filesystem)
    // -----------------------------------------------------------------------

    fn setup_kando_dir() -> (tempfile::TempDir, std::path::PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        crate::board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        (dir, kando_dir)
    }

    #[test]
    fn test_new_card_confirm() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "New card",
            buf: TextBuffer::new("My task".into()),
            on_confirm: InputTarget::NewCardTitle,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Create card"));
        assert_eq!(board.columns[0].cards.len(), 1);
        assert_eq!(board.columns[0].cards[0].title, "My task");
        assert_eq!(state.notification.as_deref(), Some("Card created"));
    }

    #[test]
    fn test_new_card_confirm_empty_title() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "New card",
            buf: TextBuffer::new("  ".into()),
            on_confirm: InputTarget::NewCardTitle,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_none());
        assert!(board.columns[0].cards.is_empty());
    }

    #[test]
    fn test_edit_tags_confirm() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Test".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("Bug, UI, feature".into()),
            on_confirm: InputTarget::EditTags,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Update tags"));
        assert_eq!(board.columns[0].cards[0].tags, vec!["bug", "ui", "feature"]);
    }

    #[test]
    fn test_delete_card_confirm() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Doomed".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Confirm {
            prompt: "Delete card?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        let sync = handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Delete card"));
        assert!(board.columns[0].cards.is_empty());
        assert_eq!(state.notification.as_deref(), Some("Card deleted (u to undo)"));
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn test_deny_returns_to_normal() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("0000".into()),
        };
        let kando_dir = std::path::Path::new("/tmp/fake");
        let sync = handle_confirm(&mut board, &mut state, Action::Deny, kando_dir).unwrap();
        assert!(sync.is_none());
        assert!(matches!(state.mode, Mode::Normal));
        assert_eq!(board.columns[0].cards.len(), 1);
    }

    #[test]
    fn test_wip_limit_confirm() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Card".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Confirm {
            prompt: "Over WIP limit",
            on_confirm: ConfirmTarget::WipLimitMove { from_col: 0, card_idx: 0, to_col: 1 },
        };
        let sync = handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Move card"));
        assert!(board.columns[0].cards.is_empty());
        assert_eq!(board.columns[1].cards.len(), 1);
        assert_eq!(state.focused_column, 1);
    }

    // -----------------------------------------------------------------------
    // Card action mode entry tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_new_card_enters_input_mode() {
        let mut board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::NewCard, &mut terminal, kando_dir, false).unwrap();
        assert!(matches!(state.mode, Mode::Input { on_confirm: InputTarget::NewCardTitle, .. }));
    }

    #[test]
    fn test_delete_card_enters_confirm_mode() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::DeleteCard, &mut terminal, kando_dir, false).unwrap();
        assert!(matches!(state.mode, Mode::Confirm { on_confirm: ConfirmTarget::DeleteCard(_), .. }));
    }

    #[test]
    fn test_open_card_detail() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::OpenCardDetail, &mut terminal, kando_dir, false).unwrap();
        assert!(matches!(state.mode, Mode::CardDetail { scroll: 0 }));
    }

    #[test]
    fn test_detail_scroll() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::CardDetail { scroll: 0 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::DetailScrollDown, &mut terminal, kando_dir, false).unwrap();
        if let Mode::CardDetail { scroll } = state.mode { assert_eq!(scroll, 1); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::CardDetail { scroll } = state.mode { assert_eq!(scroll, 0); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::CardDetail { scroll } = state.mode { assert_eq!(scroll, 0); }
    }

    #[test]
    fn test_close_panel() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::CardDetail { scroll: 5 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::ClosePanel, &mut terminal, kando_dir, false).unwrap();
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn test_cycle_priority() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Test".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let sync = handle_card_action(
            &mut board, &mut state, Action::CyclePriority,
            &mut terminal, &kando_dir, false,
        ).unwrap();
        assert_eq!(sync.as_deref(), Some("Change priority"));
        assert_eq!(board.columns[0].cards[0].priority, Priority::High);
    }

    #[test]
    fn test_toggle_blocker() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Test".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let sync = handle_card_action(
            &mut board, &mut state, Action::ToggleBlocker,
            &mut terminal, &kando_dir, false,
        ).unwrap();
        assert_eq!(sync.as_deref(), Some("Toggle blocker"));
        assert!(board.columns[0].cards[0].blocked);
        assert_eq!(state.notification.as_deref(), Some("Card blocked"));
    }

    // -----------------------------------------------------------------------
    // Trash / undo tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_delete_populates_last_delete() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Doomed".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Confirm {
            prompt: "Delete card?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        // last_delete is set for undo
        assert!(state.last_delete.is_some());
        let entry = state.last_delete.as_ref().unwrap();
        assert_eq!(entry.id, "001");
        assert_eq!(entry.title, "Doomed");
        assert_eq!(entry.from_column, "backlog");
    }

    #[test]
    fn test_undo_restores_card() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Undone".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();

        // Delete the card
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert!(board.columns[0].cards.is_empty());

        // Undo via production code path
        let sync = handle_undo(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Restore card"));

        // Card is back in the first column
        assert_eq!(board.columns[0].cards.len(), 1);
        assert_eq!(board.columns[0].cards[0].title, "Undone");
        assert!(state.notification.as_deref().unwrap().contains("Restored"));
        assert!(state.last_delete.is_none());
    }

    #[test]
    fn test_delete_card_file_goes_to_trash() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Filed".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();

        // Card file is in .trash, not in column directory
        let col_file = kando_dir.join("columns/backlog/001.md");
        let trash_file = kando_dir.join(".trash/001.md");
        assert!(!col_file.exists());
        assert!(trash_file.exists());
    }

    #[test]
    fn test_undo_restores_to_original_column() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        // Put card in column 1 (in-progress)
        board.columns[1].cards.push(Card::new("001".into(), "WIP".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.focused_column = 1;

        // Delete
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert_eq!(state.last_delete.as_ref().unwrap().from_column, "in-progress");

        // Undo via production code path
        handle_undo(&mut board, &mut state, &kando_dir).unwrap();

        // Card is back in column 1 (in-progress), cursor focused there
        assert_eq!(board.columns[1].cards.len(), 1);
        assert_eq!(board.columns[1].cards[0].title, "WIP");
        assert_eq!(state.focused_column, 1);
    }

    #[test]
    fn test_undo_nothing_to_undo() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut state = AppState::new();

        let sync = handle_undo(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_none());
        assert_eq!(state.notification.as_deref(), Some("Nothing to undo"));
    }

    #[test]
    fn test_undo_focuses_restored_card() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Keep".into()));
        board.columns[0].cards.push(Card::new("002".into(), "Delete".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.selected_card = 1;

        // Delete card 002
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("002".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();

        // Undo and check cursor lands on restored card
        handle_undo(&mut board, &mut state, &kando_dir).unwrap();
        let restored = &board.columns[state.focused_column].cards[state.selected_card];
        assert_eq!(restored.id, "002");
    }

    #[test]
    fn test_restore_via_command_clears_last_delete() {
        // Regression: :restore used to leave last_delete set, so pressing `u`
        // afterwards would produce "Restore failed: No such file or directory".
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Bug".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();

        // Step 1: delete the card (sets last_delete)
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert!(state.last_delete.is_some());

        // Step 2: restore via :restore command
        crate::command::execute_command(&mut board, &mut state, "restore 001", &kando_dir).unwrap();
        assert_eq!(board.columns[0].cards.len(), 1);
        assert_eq!(state.notification.as_deref(), Some("Restored: Bug")); // #4: assert restore notification

        // Step 3: last_delete must be cleared — undo should say "Nothing to undo"
        assert!(state.last_delete.is_none());
        let sync = handle_undo(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_none());
        assert_eq!(state.notification.as_deref(), Some("Nothing to undo"));
    }

    #[test]
    fn test_restore_different_card_preserves_last_delete() {
        // :restore B (a different card) must leave last_delete for A intact.
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Alpha".into()));
        board.columns[0].cards.push(Card::new("002".into(), "Beta".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();

        // Delete 001 — arms last_delete for 001
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert_eq!(state.last_delete.as_ref().unwrap().id, "001");

        // Delete 002 as well (its last_delete entry is now 002, overwriting 001)
        // … but we want to test the case where we restore a *different* card than
        // the one in last_delete. Re-set last_delete manually back to 001.
        trash_card(&kando_dir, "backlog", "002", "Beta").unwrap();
        state.last_delete = Some(crate::board::storage::TrashEntry {
            id: "001".into(),
            deleted: chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
            from_column: "backlog".into(),
            title: "Alpha".into(),
        });

        // Restore 002 (different from last_delete's 001)
        crate::command::execute_command(&mut board, &mut state, "restore 002", &kando_dir).unwrap();

        // last_delete for 001 must still be set — u should still work
        assert_eq!(state.last_delete.as_ref().unwrap().id, "001");
        let sync = handle_undo(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Restore card"));
        assert!(board.columns[0].cards.iter().any(|c| c.id == "001"));
    }

    #[test]
    fn test_mutating_command_clears_last_delete() {
        // Commands like :move should clear last_delete, mirroring the keyboard handler.
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Deleted".into()));
        board.columns[0].cards.push(Card::new("002".into(), "Mover".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();

        // Delete 001 — arms last_delete
        state.mode = Mode::Confirm {
            prompt: "Delete?",
            on_confirm: ConfirmTarget::DeleteCard("001".into()),
        };
        handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert!(state.last_delete.is_some());

        // :move selected card to done — should clear last_delete.
        // After deleting 001, card 002 is at index 0 in backlog.
        state.focused_column = 0;
        state.selected_card = 0;
        crate::command::execute_command(&mut board, &mut state, "move done", &kando_dir).unwrap();
        assert!(state.last_delete.is_none());
    }

    #[test]
    fn test_metrics_scroll() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Metrics { scroll: 0 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");

        // Scroll down
        handle_card_action(&mut board, &mut state, Action::DetailScrollDown, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Metrics { scroll } = state.mode { assert_eq!(scroll, 1); } else { panic!("expected Mode::Metrics"); }

        // Scroll up
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Metrics { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Metrics"); }

        // Scroll up at 0 stays at 0 (saturating)
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Metrics { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Metrics"); }
    }

    #[test]
    fn test_close_metrics_panel() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Metrics { scroll: 3 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::ClosePanel, &mut terminal, kando_dir, false).unwrap();
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn test_move_card_sets_completed_integration() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "Test card".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();

        // The board has columns: backlog(0), in-progress(1), done(2), archive(3)
        // Move card from backlog to done
        let done_idx = board.columns.iter().position(|c| c.slug == "done").unwrap();
        board.move_card(0, 0, done_idx);
        crate::board::storage::save_board(&kando_dir, &board).unwrap();

        // Reload and verify completed is set
        let reloaded = crate::board::storage::load_board(&kando_dir).unwrap();
        let done_cards = &reloaded.columns[done_idx].cards;
        assert_eq!(done_cards.len(), 1);
        assert!(done_cards[0].completed.is_some(), "card moved to done should have completed timestamp");

        // Compute metrics — should find 1 completed card
        let metrics = crate::board::metrics::compute_metrics(&reloaded, None);
        assert_eq!(metrics.total_completed, 1);
        assert!(metrics.time_stats.is_some());
    }
}
