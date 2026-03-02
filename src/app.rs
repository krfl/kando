use std::time::{Duration, Instant};

use chrono::Utc;
use crossterm::event::{self, Event};
use ratatui::DefaultTerminal;

use fuzzy_matcher::skim::SkimMatcherV2;

use crate::board::age::{run_auto_archive, run_auto_close};
use crate::board::storage::{append_activity, find_kando_dir, load_board, load_local_config, load_trash, save_board, save_local_config, trash_card, restore_card, TrashEntry};
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

/// Tab-completion state for comma-separated input fields.
#[derive(Debug, Clone)]
pub struct CompletionState {
    pub candidates: Vec<String>,
    pub index: usize,
}

/// Visible completion hint shown as a popup while editing tags/assignees.
#[derive(Debug, Clone)]
pub struct CompletionHint {
    pub candidates: Vec<String>,
    pub selected: Option<usize>,
}

/// Current interaction mode.
#[derive(Debug, Clone)]
pub enum Mode {
    Normal,
    Goto,
    Space,
    Column,
    ColMove,
    FilterMenu,
    Input {
        prompt: &'static str,
        buf: TextBuffer,
        on_confirm: InputTarget,
        completion: Option<CompletionState>,
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
    CardDetail {
        scroll: u16,
    },
    Metrics {
        scroll: u16,
    },
    Tutorial {
        scroll: u16,
    },
    Help {
        scroll: u16,
    },
}

#[derive(Debug, Clone)]
pub enum InputTarget {
    NewCardTitle,
    EditTags,
    EditAssignees,
    WipLimit,
    ColRename(String), // holds slug of column being renamed
    ColAdd,
    PipeCommand,
    DueDate,
}

#[derive(Debug, Clone)]
pub enum ConfirmTarget {
    DeleteCard(String), // card id
    WipLimitMove {
        from_col: usize,
        card_idx: usize,
        to_col: usize,
    },
    ColRemove(String), // slug of column to remove
}

#[derive(Debug, Clone)]
pub enum PickerTarget {
    Priority,
    MoveToColumn,
    SortColumn,
    TagFilter,
    AssigneeFilter,
    StalenessFilter,
    DueDate,
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
    pub active_staleness_filters: Vec<String>,
    pub active_overdue_filter: bool,
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
    /// Ghost text hint for tab-completion (shown dimmed after cursor).
    pub ghost_text: Option<String>,
    /// Completion candidates popup for tag/assignee input.
    pub completion_hint: Option<CompletionHint>,
    /// Use Nerd Font glyphs instead of ASCII icons.
    pub nerd_font: bool,
    /// Whether the first-launch tutorial has been shown (from local.toml).
    pub tutorial_shown: bool,
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
            active_staleness_filters: Vec::new(),
            active_overdue_filter: false,
            notification: None,
            notification_level: NotificationLevel::Info,
            notification_expires: None,
            should_quit: false,
            sync_state: None,
            last_delete: None,
            deleted_this_session: false,
            ghost_text: None,
            completion_hint: None,
            nerd_font: false,
            tutorial_shown: false,
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
    #[cfg(test)]
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
            || !self.active_staleness_filters.is_empty()
            || self.active_overdue_filter
    }

    /// Clamp selection to the nearest visible card under active filters.
    /// Falls back to the regular unfiltered clamp when no filters are active.
    pub fn clamp_selection_filtered(&mut self, board: &Board) {
        if !self.has_active_filter() {
            self.clamp_selection(board);
            return;
        }
        if let Some(col) = board.columns.get(self.focused_column) {
            let visible = visible_card_indices(col, self, board);
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
fn visible_card_indices(col: &Column, state: &AppState, board: &Board) -> Vec<usize> {
    let matcher = SkimMatcherV2::default();
    let now = Utc::now();
    col.cards
        .iter()
        .enumerate()
        .filter(|(_, card)| {
            crate::board::card_is_visible(
                card,
                state.active_filter.as_deref(),
                &state.active_tag_filters,
                &state.active_assignee_filters,
                &state.active_staleness_filters,
                state.active_overdue_filter,
                &board.policies,
                now,
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

/// Jump to the next (or previous) visible card across all columns,
/// wrapping around. Used by `n` / `N` after a filter is active.
fn jump_to_visible_card(board: &Board, state: &mut AppState, forward: bool) {
    // Collect all (col_idx, card_idx) pairs for visible cards across all columns.
    let matcher = SkimMatcherV2::default();
    let now = Utc::now();
    let mut all_visible: Vec<(usize, usize)> = Vec::new();
    for (ci, col) in board.columns.iter().enumerate() {
        if col.hidden && !state.show_hidden_columns {
            continue;
        }
        for (cardi, card) in col.cards.iter().enumerate() {
            if crate::board::card_is_visible(
                card,
                state.active_filter.as_deref(),
                &state.active_tag_filters,
                &state.active_assignee_filters,
                &state.active_staleness_filters,
                state.active_overdue_filter,
                &board.policies,
                now,
                &matcher,
            ) {
                all_visible.push((ci, cardi));
            }
        }
    }
    if all_visible.is_empty() {
        state.notify("No matches");
        return;
    }
    let current = (state.focused_column, state.selected_card);
    let pos = all_visible.iter().position(|&v| v == current);
    let next = if forward {
        match pos {
            Some(p) => all_visible[(p + 1) % all_visible.len()],
            None => all_visible[0],
        }
    } else {
        match pos {
            Some(p) => all_visible[(p + all_visible.len() - 1) % all_visible.len()],
            None => *all_visible.last().unwrap(),
        }
    };
    state.focused_column = next.0;
    state.selected_card = next.1;
}

/// Run auto-close, save if any cards were closed, sync, and notify.
fn handle_auto_close(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<()> {
    let mut messages = Vec::new();
    let now = Utc::now();

    let closed = run_auto_close(board, now);
    if !closed.is_empty() {
        save_board(kando_dir, board)?;
        let target_name = board.columns.iter()
            .find(|c| c.slug == board.policies.auto_close_target)
            .map(|c| c.name.clone())
            .unwrap_or_else(|| board.policies.auto_close_target.clone());
        for card in &closed {
            let from_name = board.columns.iter()
                .find(|c| c.slug == card.from_col_slug)
                .map(|c| c.name.clone())
                .unwrap_or_else(|| card.from_col_slug.clone());
            append_activity(kando_dir, "auto-close", &card.id, &card.title,
                &[("from", &from_name), ("to", &target_name)]);
        }
        let n = closed.len();
        let sync_msg = format!("Auto-close {} stale card{}", n, if n == 1 { "" } else { "s" });
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, kando_dir, &sync_msg);
        }
        messages.push(format!(
            "{} card{} auto-closed",
            n,
            if n == 1 { "" } else { "s" }
        ));
    }

    // Auto-archive completed cards from the done column
    let aa = run_auto_archive(board, now);
    if !aa.is_empty() {
        save_board(kando_dir, board)?;
        let archive_name = board.columns.iter()
            .find(|c| c.slug == "archive")
            .map(|c| c.name.clone())
            .unwrap_or_else(|| "Archive".to_string());
        for card in &aa {
            let from_name = board.columns.iter()
                .find(|c| c.slug == card.from_col_slug)
                .map(|c| c.name.clone())
                .unwrap_or_else(|| card.from_col_slug.clone());
            append_activity(kando_dir, "archive", &card.id, &card.title,
                &[("from", &from_name), ("to", &archive_name)]);
        }
        let n = aa.len();
        let sync_msg = format!("Auto-archive {} completed card{}", n, if n == 1 { "" } else { "s" });
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, kando_dir, &sync_msg);
        }
        messages.push(format!(
            "{} card{} auto-archived",
            n,
            if n == 1 { "" } else { "s" }
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
        None => {
            state.notify(if forward { "Last column" } else { "First column" });
            return Ok(None);
        }
    };
    let col = match board.columns.get(state.focused_column) {
        Some(col) if !col.cards.is_empty() => col,
        Some(_) => {
            state.notify("No cards to move");
            return Ok(None);
        }
        None => unreachable!("focused_column always refers to a valid column index"),
    };
    // The filter guard in handle_card_action guarantees selected_card is a valid
    // visible index; the clamp is a safety net against any future invariant break.
    debug_assert!(state.selected_card < col.cards.len(), "selected_card out of range");
    let card_idx = state.selected_card.min(col.cards.len() - 1);
    let from = state.focused_column;

    if board.columns[to].is_over_wip_limit() {
        state.mode = Mode::Confirm {
            prompt: "Column over WIP limit, move anyway?",
            on_confirm: ConfirmTarget::WipLimitMove { from_col: from, card_idx, to_col: to },
        };
        return Ok(None);
    }

    let card_id = board.columns[from].cards[card_idx].id.clone();
    let card_title = board.columns[from].cards[card_idx].title.clone();
    let from_name = board.columns[from].name.clone();
    let to_name = board.columns[to].name.clone();
    board.move_card(from, card_idx, to);
    board.columns[to].sort_cards();
    state.focused_column = to;
    state.clamp_selection_filtered(board);
    save_board(kando_dir, board)?;
    append_activity(kando_dir, "move", &card_id, &card_title,
        &[("from", &from_name), ("to", &to_name)]);
    state.notify("Card moved");
    Ok(Some(format!("Move card #{card_id} \"{card_title}\" from {from_name} to {to_name}")))
}

/// Main TUI application loop.
pub fn run(terminal: &mut DefaultTerminal, start_dir: &std::path::Path, nerd_font_flag: bool) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(start_dir)?;
    let mut board = load_board(&kando_dir)?;
    let mut state = AppState::new();

    // CLI --nerd-font flag overrides config; otherwise use board config value
    state.nerd_font = nerd_font_flag || board.nerd_font;

    // Load per-user local preferences.
    match load_local_config(&kando_dir) {
        Ok(local_cfg) => {
            state.tutorial_shown = local_cfg.tutorial_shown;
        }
        Err(e) => {
            state.notify_error(format!("local.toml: {e}"));
        }
    }

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

    // Warn at startup if the 'archive' column is missing (reserved for archiving).
    if !board.columns.iter().any(|c| c.slug == "archive") {
        state.notify_error("Warning: no 'archive' column found — archiving features disabled");
    }

    state.clamp_selection(&board);

    // Show tutorial on first launch
    if !state.tutorial_shown {
        state.mode = Mode::Tutorial { scroll: 0 };
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
        terminal.draw(|f| crate::ui::render(f, &board, &mut state, now))?;

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
    let was_minor_mode = matches!(state.mode, Mode::Goto | Mode::Space | Mode::Column | Mode::ColMove | Mode::FilterMenu);
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
        | Action::ArchiveCard
        | Action::PipeCard
        | Action::EditCardExternal
        | Action::PickPriority
        | Action::MoveToColumn
        | Action::ToggleBlocker
        | Action::SetDueDate
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

        // Visibility toggles
        Action::ToggleHiddenColumns => {
            handle_visibility_toggle(board, state, action);
        }

        // Column mode actions
        Action::ToggleFocusedColumnHidden => {
            sync_message = handle_col_toggle_hidden(board, state, kando_dir)?;
        }
        Action::ColRenameSelected => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let slug = col.slug.clone();
                let current_name = col.name.clone();
                state.mode = Mode::Input {
                    prompt: "Rename column",
                    buf: TextBuffer::new(current_name),
                    on_confirm: InputTarget::ColRename(slug),
                    completion: None,
                };
            }
        }
        Action::ColAddBefore => {
            state.mode = Mode::Input {
                prompt: "New column name",
                buf: TextBuffer::empty(),
                on_confirm: InputTarget::ColAdd,
                completion: None,
            };
        }
        Action::ColRemoveSelected => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let slug = col.slug.clone();
                if slug == "archive" {
                    state.notify_error("The 'archive' column is reserved and cannot be removed");
                } else if board.columns.len() == 1 {
                    state.notify_error("Cannot remove the last column");
                } else if !col.cards.is_empty() {
                    state.notify_error(format!("Column has {} card(s); move them first", col.cards.len()));
                } else {
                    state.mode = Mode::Confirm {
                        prompt: "Delete column?",
                        on_confirm: ConfirmTarget::ColRemove(slug),
                    };
                }
            }
        }
        Action::ColSetWip => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let current = col.wip_limit.map(|n| n.to_string()).unwrap_or_else(|| "0".into());
                state.mode = Mode::Input {
                    prompt: "WIP limit (0=off)",
                    buf: TextBuffer::new(current),
                    on_confirm: InputTarget::WipLimit,
                    completion: None,
                };
            }
        }
        Action::EnterColMoveMode => {
            if board.columns.get(state.focused_column).is_some() {
                state.mode = Mode::ColMove;
            }
        }
        Action::ColMoveLeft | Action::ColMoveRight | Action::ColMoveFirst
        | Action::ColMoveLast | Action::ColMoveToPosition(_) => {
            let col_idx = state.focused_column;
            let len = board.columns.len();
            if col_idx >= len {
                state.mode = Mode::Normal;
                return Ok(());
            }
            let target_idx = match &action {
                Action::ColMoveLeft => {
                    if col_idx == 0 {
                        state.notify_error("Already leftmost column");
                        state.mode = Mode::Normal;
                        return Ok(());
                    }
                    col_idx - 1
                }
                Action::ColMoveRight => {
                    if col_idx + 1 >= len {
                        state.notify_error("Already rightmost column");
                        state.mode = Mode::Normal;
                        return Ok(());
                    }
                    col_idx + 1
                }
                Action::ColMoveFirst => 0,
                Action::ColMoveLast => len - 1,
                Action::ColMoveToPosition(n) => {
                    if *n >= 1 && *n <= len {
                        *n - 1
                    } else {
                        state.notify_error(format!("Invalid position: {n} (use 1–{len})"));
                        state.mode = Mode::Normal;
                        return Ok(());
                    }
                }
                _ => unreachable!(),
            };
            if target_idx == col_idx {
                let name = board.columns[col_idx].name.clone();
                state.notify(format!("{name} is already at that position"));
                state.mode = Mode::Normal;
                return Ok(());
            }
            let slug = board.columns[col_idx].slug.clone();
            let name = board.columns[col_idx].name.clone();
            let col = board.columns.remove(col_idx);
            board.columns.insert(target_idx, col);
            for (i, c) in board.columns.iter_mut().enumerate() {
                c.order = i as u32;
            }
            state.focused_column = board.columns.iter().position(|c| c.slug == slug).unwrap_or(0);
            state.clamp_selection(board);
            save_board(kando_dir, board)?;
            let new_pos = target_idx + 1;
            append_activity(kando_dir, "col-move", &slug, &name, &[("to", &new_pos.to_string())]);
            state.notify(format!("Moved {name} to position {new_pos}"));
            sync_message = Some(format!("Move column \"{name}\" to position {new_pos}"));
            state.mode = Mode::Normal;
        }

        // Sort
        Action::StartSort => {
            let items: Vec<(String, bool)> = ["priority", "created", "updated", "title"]
                .iter()
                .map(|s| (s.to_string(), false))
                .collect();
            state.mode = Mode::Picker {
                title: "sort by",
                items,
                selected: 0,
                target: PickerTarget::SortColumn,
            };
        }

        // Filter / tag filter
        Action::StartFilter | Action::StartTagFilter | Action::StartAssigneeFilter | Action::StartStalenessFilter | Action::StartOverdueFilter => {
            handle_filter_start(board, state, action);
        }

        // Find next/prev visible card
        Action::FindNext | Action::FindPrev => {
            if state.has_active_filter() {
                jump_to_visible_card(board, state, matches!(action, Action::FindNext));
            }
        }

        // Text input delegation
        Action::InputChar(_)
        | Action::InputBackspace
        | Action::InputLeft
        | Action::InputRight
        | Action::InputHome
        | Action::InputEnd
        | Action::InputDeleteWord
        | Action::InputCompleteForward
        | Action::InputCompleteBackward
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
        Action::EnterColumnMode => state.mode = Mode::Column,
        Action::EnterFilterMode => state.mode = Mode::FilterMenu,
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
        Action::ShowHelp => state.mode = Mode::Help { scroll: 0 },
        Action::ShowMetrics => state.mode = Mode::Metrics { scroll: 0 },
        Action::DismissTutorial => {
            state.mode = Mode::Normal;
            state.tutorial_shown = true;
            let mut local_cfg = load_local_config(kando_dir).unwrap_or_default();
            local_cfg.tutorial_shown = true;
            save_local_config(kando_dir, &local_cfg)?;
        }
        Action::Quit => {
            match &state.mode {
                Mode::Normal => state.should_quit = true,
                _ => state.mode = Mode::Normal,
            }
        }
        Action::ClearFilters => {
            if state.has_active_filter() {
                state.active_filter = None;
                state.active_tag_filters.clear();
                state.active_assignee_filters.clear();
                state.active_staleness_filters.clear();
                state.active_overdue_filter = false;
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
                        let visible = visible_card_indices(col, state, board);
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
                        let visible = visible_card_indices(col, state, board);
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
                let visible = visible_card_indices(col, state, board);
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
                        let next_visible = visible_card_indices(&board.columns[next], state, board);
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
                let visible = visible_card_indices(col, state, board);
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
                        let prev_visible = visible_card_indices(&board.columns[prev], state, board);
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
                let visible = visible_card_indices(col, state, board);
                if let Some(&first) = visible.first() {
                    state.selected_card = first;
                } else {
                    state.notify("No visible cards");
                }
            }
        }
        Action::JumpToLastCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state, board);
                if let Some(&last) = visible.last() {
                    state.selected_card = last;
                } else {
                    state.notify("No visible cards");
                }
            }
        }
        Action::JumpToBacklog => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "backlog" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.clamp_selection_filtered(board);
            } else {
                state.notify("No 'backlog' column");
            }
        }
        Action::JumpToDone => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "done" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.clamp_selection_filtered(board);
            } else {
                state.notify("No 'done' column");
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
        | Action::DetailScrollDown | Action::DetailScrollUp | Action::DetailNextCard | Action::DetailPrevCard
        | Action::PipeCard) {
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
            let visible = visible_card_indices(col, state, board);
            if !visible.contains(&state.selected_card) {
                state.notify("No visible card selected");
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
                completion: None,
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
        Action::ArchiveCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            let archive_idx = match board.columns.iter().position(|c| c.slug == "archive") {
                Some(i) => i,
                None => {
                    state.notify_error("No 'archive' column found — create one first");
                    return Ok(sync_message);
                }
            };
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if board.columns.get(col_idx).and_then(|c| c.cards.get(card_idx)).is_none() {
                state.notify_error("No card selected");
                return Ok(sync_message);
            }
            if col_idx == archive_idx {
                state.notify_error("Card is already in the archive");
                return Ok(sync_message);
            }
            let card = board.columns[col_idx].cards.remove(card_idx);
            let title = card.title.clone();
            let id = card.id.clone();
            let from_col_name = board.columns[col_idx].name.clone();
            let archive_name = board.columns[archive_idx].name.clone();
            board.columns[archive_idx].cards.push(card);
            board.columns[archive_idx].sort_cards();
            save_board(kando_dir, board)?;
            append_activity(kando_dir, "archive", &id, &title,
                &[("from", &from_col_name), ("to", &archive_name)]);
            state.clamp_selection_filtered(board);
            state.notify(format!("Archived: {title}"));
            sync_message = Some("Archive card".into());
        }
        Action::PipeCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if state.selected_card_ref(board).is_some() {
                state.mode = Mode::Input {
                    prompt: "Pipe to command",
                    buf: TextBuffer::empty(),
                    on_confirm: InputTarget::PipeCommand,
                    completion: None,
                };
            } else {
                state.notify_error("No card selected");
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
                    let card_title = board.columns[col_idx].cards[card_idx].title.clone();
                    let col_name = board.columns[col_idx].name.clone();
                    board.columns[col_idx].cards[card_idx].touch();
                    board.columns[col_idx].sort_cards();
                    save_board(kando_dir, board)?;
                    append_activity(kando_dir, "edit", &card_id, &card_title,
                        &[("column", &col_name)]);
                    sync_message = Some(format!("Edit card #{card_id} \"{card_title}\""));
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
        Action::SetDueDate => {
            state.mode = Mode::Normal;
            if state.selected_card_ref(board).is_some() {
                let items: Vec<(String, bool)> = ["tomorrow", "+3 days", "+1 week", "+2 weeks", "+1 month", "custom", "clear"]
                    .iter()
                    .map(|s| (s.to_string(), false))
                    .collect();
                state.mode = Mode::Picker {
                    title: "due date",
                    items,
                    selected: 0,
                    target: PickerTarget::DueDate,
                };
            } else {
                state.notify("No card selected");
            }
        }
        Action::ToggleBlocker => {
            if was_minor_mode { state.mode = Mode::Normal; }
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                let card_id = card.id.clone();
                let card_title = card.title.clone();
                card.blocked = !card.blocked;
                card.touch();
                let blocked = card.blocked;
                let msg = if blocked { "Card blocked" } else { "Blocker removed" };
                board.columns[col_idx].sort_cards();
                let col_name = board.columns[col_idx].name.clone();
                save_board(kando_dir, board)?;
                let blocked_str = if blocked { "true" } else { "false" };
                let action_str = if blocked { "Block" } else { "Unblock" };
                append_activity(kando_dir, "blocker", &card_id, &card_title,
                    &[("column", &col_name), ("blocked", blocked_str)]);
                sync_message = Some(format!("{action_str} #{card_id} \"{card_title}\""));
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
                    completion: None,
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
                    completion: None,
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
                Mode::CardDetail { scroll } | Mode::Metrics { scroll } | Mode::Help { scroll } | Mode::Tutorial { scroll } => {
                    *scroll = scroll.saturating_add(1);
                }
                _ => {}
            }
        }
        Action::DetailScrollUp => {
            match &mut state.mode {
                Mode::CardDetail { scroll } | Mode::Metrics { scroll } | Mode::Help { scroll } | Mode::Tutorial { scroll } => {
                    *scroll = scroll.saturating_sub(1);
                }
                _ => {}
            }
        }
        Action::DetailNextCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state, board);
                if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                    if pos + 1 < visible.len() {
                        state.selected_card = visible[pos + 1];
                        state.mode = Mode::CardDetail { scroll: 0 };
                    } else {
                        state.notify("Last card");
                    }
                }
            }
        }
        Action::DetailPrevCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                let visible = visible_card_indices(col, state, board);
                if let Some(pos) = visible.iter().position(|&i| i == state.selected_card) {
                    if pos > 0 {
                        state.selected_card = visible[pos - 1];
                        state.mode = Mode::CardDetail { scroll: 0 };
                    } else {
                        state.notify("First card");
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    Ok(sync_message)
}

// ---------------------------------------------------------------------------
// Handler: Visibility toggles
// ---------------------------------------------------------------------------

fn handle_visibility_toggle(board: &Board, state: &mut AppState, action: Action) {
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
// Handler: Column hide (c-mode h key)
// ---------------------------------------------------------------------------

fn handle_col_toggle_hidden(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let col_idx = state.focused_column;
    if col_idx >= board.columns.len() {
        return Ok(None);
    }
    let currently_hidden = board.columns[col_idx].hidden;
    if !currently_hidden {
        // Guard: don't hide the last visible column.
        let visible_count = board.columns.iter().filter(|c| !c.hidden).count();
        if visible_count <= 1 {
            state.notify_error("Cannot hide the last visible column");
            return Ok(None);
        }
    }
    let slug = board.columns[col_idx].slug.clone();
    let name = board.columns[col_idx].name.clone();
    let new_hidden = !currently_hidden;
    board.columns[col_idx].hidden = new_hidden;
    save_board(kando_dir, board)?;
    let action_name = if new_hidden { "col-hide" } else { "col-show" };
    append_activity(kando_dir, action_name, &slug, &name, &[]);
    state.mode = Mode::Normal;
    if new_hidden && !state.show_hidden_columns {
        // If hidden columns aren't shown, move focus to first visible column.
        if let Some(idx) = board.columns.iter().position(|c| !c.hidden) {
            state.focused_column = idx;
            state.clamp_selection_filtered(board);
        }
    }
    let label = if new_hidden { "hidden" } else { "visible" };
    state.notify(format!("{name}: {label}"));
    Ok(Some(if new_hidden { "Hide column" } else { "Show column" }.into()))
}

// ---------------------------------------------------------------------------
// Handler: Filter / tag filter start
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Helper: apply a due date to the selected card
// ---------------------------------------------------------------------------

fn apply_due_date(
    board: &mut Board,
    state: &mut AppState,
    due: Option<chrono::NaiveDate>,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<()> {
    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        let card_id = card.id.clone();
        let card_title = card.title.clone();
        card.due = due;
        card.touch();
        let col_name = board.columns[col_idx].name.clone();
        board.columns[col_idx].sort_cards();
        save_board(kando_dir, board)?;
        let date_str = due.map(|d| d.format("%Y-%m-%d").to_string()).unwrap_or_else(|| "none".into());
        append_activity(kando_dir, "due", &card_id, &card_title, &[("due", &date_str), ("column", &col_name)]);
        if due.is_some() {
            state.notify(format!("Due: {date_str}"));
        } else {
            state.notify("Due date cleared");
        }
    }
    Ok(())
}

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
                let mut items: Vec<(String, bool)> = tags
                    .iter()
                    .map(|(tag, _count)| {
                        let active = state.active_tag_filters.contains(tag);
                        (tag.clone(), active)
                    })
                    .collect();
                items.sort_by(|a, b| a.0.cmp(&b.0));
                state.mode = Mode::Picker {
                    title: "tag",
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
                let mut items: Vec<(String, bool)> = assignees
                    .iter()
                    .map(|(name, _count)| {
                        let active = state.active_assignee_filters.contains(name);
                        (name.clone(), active)
                    })
                    .collect();
                items.sort_by(|a, b| a.0.cmp(&b.0));
                state.mode = Mode::Picker {
                    title: "assignee",
                    items,
                    selected: 0,
                    target: PickerTarget::AssigneeFilter,
                };
            }
        }
        Action::StartStalenessFilter => {
            let labels: [(&str, &str); 4] = [
                ("new", "Created today"),
                ("normal", "Not stale"),
                ("stale", "Needs attention"),
                ("very stale", "Very stale"),
            ];
            let items: Vec<(String, bool)> = labels
                .iter()
                .map(|(label, _)| {
                    let active = state.active_staleness_filters.contains(&label.to_string());
                    (label.to_string(), active)
                })
                .collect();
            state.mode = Mode::Picker {
                title: "staleness",
                items,
                selected: 0,
                target: PickerTarget::StalenessFilter,
            };
        }
        Action::StartOverdueFilter => {
            state.active_overdue_filter = !state.active_overdue_filter;
            state.clamp_selection_filtered(board);
            state.mode = Mode::Normal;
            state.notify(if state.active_overdue_filter {
                "Overdue filter on"
            } else {
                "Overdue filter off"
            });
        }
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Tab-completion helpers for comma-separated input (tags / assignees)
// ---------------------------------------------------------------------------

/// Extract the current token from a comma-separated input string.
/// Returns (token_text_trimmed, byte_offset_of_token_start_including_leading_space).
fn current_csv_token(input: &str) -> (&str, usize) {
    let start = input.rfind(',').map(|i| i + 1).unwrap_or(0);
    let token = &input[start..];
    let trimmed = token.trim_start();
    let trim_offset = token.len() - trimmed.len();
    (trimmed, start + trim_offset)
}

/// Collect the already-entered values (segments before the current token).
fn entered_values(input: &str) -> Vec<String> {
    match input.rfind(',') {
        Some(pos) => input[..pos]
            .split(',')
            .map(|s| s.trim().to_lowercase())
            .filter(|s| !s.is_empty())
            .collect(),
        None => Vec::new(),
    }
}

/// Get completion candidates for the given input target.
fn completion_candidates(target: &InputTarget, board: &Board) -> Vec<String> {
    match target {
        InputTarget::EditTags => board.all_tags().into_iter().map(|(s, _)| s).collect(),
        InputTarget::EditAssignees => board.all_assignees().into_iter().map(|(s, _)| s).collect(),
        _ => Vec::new(),
    }
}

/// Filter candidates by prefix and exclude already-entered values.
fn filtered_candidates(all: &[String], prefix: &str, already: &[String]) -> Vec<String> {
    let prefix_lower = prefix.to_lowercase();
    all.iter()
        .filter(|c| c.to_lowercase().starts_with(&prefix_lower))
        .filter(|c| !already.contains(&c.to_lowercase()))
        .cloned()
        .collect()
}

/// Cycle tab-completion in the current input buffer.
fn cycle_input_completion(state: &mut AppState, board: &Board, forward: bool) {
    let Mode::Input { buf, on_confirm, completion, .. } = &mut state.mode else {
        return;
    };

    // Only complete for tags and assignees
    if !matches!(on_confirm, InputTarget::EditTags | InputTarget::EditAssignees) {
        return;
    }

    let all_candidates = completion_candidates(on_confirm, board);
    if all_candidates.is_empty() {
        return;
    }

    // On first Tab: compute candidates from current token. On subsequent
    // Tabs: reuse the stored candidate list so the index stays consistent.
    let (candidates, replace_start) = if let Some(cs) = completion.as_ref() {
        (cs.candidates.clone(), current_csv_token(&buf.input).1)
    } else {
        let (token, start) = current_csv_token(&buf.input);
        let already = entered_values(&buf.input);
        (filtered_candidates(&all_candidates, token, &already), start)
    };

    if candidates.is_empty() {
        return;
    }

    let index = if let Some(cs) = completion.as_ref() {
        if forward {
            (cs.index + 1) % candidates.len()
        } else {
            (cs.index + candidates.len() - 1) % candidates.len()
        }
    } else {
        0
    };

    // Replace current token with the selected candidate
    let candidate = &candidates[index];
    buf.input.truncate(replace_start);
    buf.input.push_str(candidate);
    buf.cursor = buf.input.chars().count();

    *completion = Some(CompletionState {
        candidates,
        index,
    });

    // Ghost text is hidden while actively cycling completions
    state.ghost_text = None;
}

/// Compute and update ghost text for the current input state.
fn update_ghost_text(state: &mut AppState, board: &Board) {
    state.ghost_text = compute_ghost_text(state, board);
}

/// Compute and update the completion hint popup for the current input state.
fn update_completion_hint(state: &mut AppState, board: &Board) {
    state.completion_hint = compute_completion_hint(state, board);
}

/// Compute the list of matching completion candidates for the popup.
fn compute_completion_hint(state: &AppState, board: &Board) -> Option<CompletionHint> {
    let Mode::Input { buf, on_confirm, completion, .. } = &state.mode else {
        return None;
    };

    if !matches!(on_confirm, InputTarget::EditTags | InputTarget::EditAssignees) {
        return None;
    }

    let all_candidates = completion_candidates(on_confirm, board);
    if all_candidates.is_empty() {
        return None;
    }

    let (token, _) = current_csv_token(&buf.input);
    let already = entered_values(&buf.input);
    let mut candidates = filtered_candidates(&all_candidates, token, &already);

    if candidates.is_empty() {
        return None;
    }

    candidates.sort_by_key(|c| c.to_lowercase());

    let selected = completion.as_ref().and_then(|cs| {
        let current = cs.candidates.get(cs.index)?;
        candidates.iter().position(|c| c == current)
    });

    Some(CompletionHint { candidates, selected })
}

/// Compute ghost text: the suffix of the first matching candidate shown dimmed after cursor.
fn compute_ghost_text(state: &AppState, board: &Board) -> Option<String> {
    let Mode::Input { buf, on_confirm, completion, .. } = &state.mode else {
        return None;
    };

    if !matches!(on_confirm, InputTarget::EditTags | InputTarget::EditAssignees) {
        return None;
    }

    // Don't show ghost text while actively cycling completions
    if completion.is_some() {
        return None;
    }

    // Only show ghost text when cursor is at the end
    if buf.cursor < buf.input.chars().count() {
        return None;
    }

    let all_candidates = completion_candidates(on_confirm, board);
    let (token, _) = current_csv_token(&buf.input);

    if token.is_empty() {
        return None;
    }

    let already = entered_values(&buf.input);
    let candidates = filtered_candidates(&all_candidates, token, &already);
    let first = candidates.first()?;

    // Return the suffix after what the user typed
    Some(first.get(token.len()..)?.to_string())
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
                Mode::Input { buf, completion, .. } => {
                    buf.insert(c);
                    *completion = None;
                }
                Mode::Filter { buf } => buf.insert(c),
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
            update_ghost_text(state, board);
            update_completion_hint(state, board);
        }
        Action::InputBackspace => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, completion, .. } => {
                    buf.backspace();
                    *completion = None;
                }
                Mode::Filter { buf } => buf.backspace(),
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
            update_ghost_text(state, board);
            update_completion_hint(state, board);
        }
        Action::InputLeft => {
            if let Mode::Input { buf, .. } | Mode::Filter { buf } = &mut state.mode {
                buf.move_left();
            }
            update_ghost_text(state, board);
        }
        Action::InputRight => {
            if let Mode::Input { buf, .. } | Mode::Filter { buf } = &mut state.mode {
                buf.move_right();
            }
            update_ghost_text(state, board);
        }
        Action::InputHome => {
            if let Mode::Input { buf, .. } | Mode::Filter { buf } = &mut state.mode {
                buf.home();
            }
            update_ghost_text(state, board);
        }
        Action::InputEnd => {
            if let Mode::Input { buf, .. } | Mode::Filter { buf } = &mut state.mode {
                buf.end();
            }
            update_ghost_text(state, board);
        }
        Action::InputDeleteWord => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, completion, .. } => {
                    buf.delete_word();
                    *completion = None;
                }
                Mode::Filter { buf } => buf.delete_word(),
                _ => {}
            }
            if is_filter { sync_filter(state); state.clamp_selection_filtered(board); }
            update_ghost_text(state, board);
            update_completion_hint(state, board);
        }
        Action::InputCompleteForward | Action::InputCompleteBackward => {
            let forward = matches!(action, Action::InputCompleteForward);
            cycle_input_completion(state, board, forward);
            update_completion_hint(state, board);
        }
        Action::InputConfirm => {
            state.ghost_text = None;
            state.completion_hint = None;
            sync_message = handle_input_confirm(board, state, kando_dir)?;
        }
        Action::InputCancel => {
            state.ghost_text = None;
            state.completion_hint = None;
            match &state.mode {
                Mode::Filter { .. } => {
                    state.active_filter = None;
                }
                Mode::Picker { .. } => {
                    // Just close — no side effects
                }
                _ => {}
            }
            state.mode = Mode::Normal;
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
                let col_name = board.columns.get(state.focused_column)
                    .map(|c| c.name.clone())
                    .unwrap_or_default();
                let card = Card::new(id.clone(), title.clone());
                if let Some(col) = board.columns.get_mut(state.focused_column) {
                    col.cards.push(card);
                    col.sort_cards();
                }
                save_board(kando_dir, board)?;
                append_activity(kando_dir, "create", &id, &title,
                    &[("column", &col_name)]);
                sync_message = Some(format!("Create card #{id} \"{title}\" in {col_name}"));
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
            let (card_id, card_title, col_name) = board.columns
                .get(state.focused_column)
                .and_then(|col| col.cards.get(state.selected_card).map(|card| {
                    (card.id.clone(), card.title.clone(), col.name.clone())
                }))
                .unwrap_or_default();
            if let Some(col) = board.columns.get_mut(state.focused_column) {
                if let Some(card) = col.cards.get_mut(state.selected_card) {
                    card.tags = tags;
                    card.touch();
                }
                col.sort_cards();
            }
            save_board(kando_dir, board)?;
            append_activity(kando_dir, "tags", &card_id, &card_title,
                &[("column", &col_name)]);
            sync_message = Some(format!("Update tags on #{card_id} \"{card_title}\""));
            state.clamp_selection_filtered(board);
            state.notify("Tags updated");
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::WipLimit,
            ..
        } => {
            let limit_str = buf.input.trim();
            let limit: u32 = match limit_str.parse() {
                Ok(n) => n,
                Err(_) => {
                    state.notify_error(format!("WIP limit must be a number, got: {limit_str}"));
                    return Ok(None);
                }
            };
            let col_idx = state.focused_column;
            if let Some(col) = board.columns.get_mut(col_idx) {
                col.wip_limit = if limit == 0 { None } else { Some(limit) };
                let col_name = col.name.clone();
                save_board(kando_dir, board)?;
                if limit == 0 {
                    state.notify(format!("WIP limit removed: {col_name}"));
                } else {
                    state.notify(format!("WIP limit: {col_name} = {limit}"));
                }
                sync_message = Some("Set WIP limit".into());
            }
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::ColRename(old_slug),
            ..
        } => {
            use crate::board::{slug_for_rename, slug_to_name};
            use crate::board::storage::rename_column_dir;
            let new_name = buf.input.trim().to_string();
            if new_name.is_empty() {
                state.notify_error("Column name cannot be empty");
            } else {
                let new_slug = match slug_for_rename(&new_name) {
                    Ok(s) => s,
                    Err(e) => {
                        state.notify_error(e);
                        return Ok(None);
                    }
                };
                let col_idx = match board.columns.iter().position(|c| c.slug == old_slug) {
                    Some(i) => i,
                    None => {
                        state.notify_error("Column no longer exists");
                        return Ok(None);
                    }
                };
                if new_slug == old_slug {
                    let old_name = board.columns[col_idx].name.clone();
                    state.notify(format!("{old_name} unchanged"));
                } else if board.columns.iter().enumerate().any(|(i, c)| i != col_idx && c.slug == new_slug) {
                    state.notify_error(format!("A column with slug '{new_slug}' already exists"));
                } else {
                    let derived_name = slug_to_name(&new_slug);
                    let old_name = board.columns[col_idx].name.clone();
                    let old_auto_close = board.policies.auto_close_target.clone();
                    board.columns[col_idx].slug = new_slug.clone();
                    board.columns[col_idx].name = derived_name.clone();
                    if board.policies.auto_close_target == old_slug {
                        board.policies.auto_close_target = new_slug.clone();
                    }
                    rename_column_dir(kando_dir, &old_slug, &new_slug).map_err(|e| {
                        color_eyre::eyre::eyre!("Could not rename column directory: {e}")
                    })?;
                    if let Err(e) = save_board(kando_dir, board) {
                        let _ = rename_column_dir(kando_dir, &new_slug, &old_slug);
                        board.columns[col_idx].slug = old_slug.clone();
                        board.columns[col_idx].name = old_name.clone();
                        board.policies.auto_close_target = old_auto_close;
                        return Err(e.into());
                    }
                    append_activity(kando_dir, "col-rename", &new_slug, &derived_name, &[("from", &old_slug)]);
                    state.notify(format!("Renamed: {old_name} → {derived_name}"));
                    sync_message = Some(format!("Rename column \"{old_name}\" to \"{derived_name}\""));
                }
            }
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::ColAdd,
            ..
        } => {
            use crate::board::{generate_slug, slug_to_name, normalize_column_orders};
            let name = buf.input.trim().to_string();
            if name.is_empty() {
                state.notify_error("Column name cannot be empty");
            } else {
                let slug = generate_slug(&name, &board.columns);
                let insert_order = board.columns
                    .get(state.focused_column)
                    .map(|c| c.order)
                    .unwrap_or(u32::try_from(board.columns.len()).unwrap_or(u32::MAX));
                for col in board.columns.iter_mut() {
                    if col.order >= insert_order {
                        col.order += 1;
                    }
                }
                let derived_name = slug_to_name(&slug);
                board.columns.push(Column {
                    slug: slug.clone(),
                    name: derived_name.clone(),
                    order: insert_order,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![],
                });
                normalize_column_orders(&mut board.columns);
                save_board(kando_dir, board)?;
                append_activity(kando_dir, "col-add", &slug, &derived_name, &[]);
                if let Some(idx) = board.columns.iter().position(|c| c.slug == slug) {
                    state.focused_column = idx;
                    state.clamp_selection(board);
                }
                state.notify(format!("Added column: {derived_name}"));
                sync_message = Some(format!("Add column \"{derived_name}\""));
            }
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
            let (card_id, card_title, col_name) = board.columns
                .get(state.focused_column)
                .and_then(|col| col.cards.get(state.selected_card).map(|card| {
                    (card.id.clone(), card.title.clone(), col.name.clone())
                }))
                .unwrap_or_default();
            if let Some(col) = board.columns.get_mut(state.focused_column) {
                if let Some(card) = col.cards.get_mut(state.selected_card) {
                    card.assignees = assignees;
                    card.touch();
                }
                col.sort_cards();
            }
            save_board(kando_dir, board)?;
            append_activity(kando_dir, "assignees", &card_id, &card_title,
                &[("column", &col_name)]);
            sync_message = Some(format!("Update assignees on #{card_id} \"{card_title}\""));
            state.clamp_selection_filtered(board);
            state.notify("Assignees updated");
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::DueDate,
            ..
        } => {
            let input = buf.input.trim().to_string();
            match chrono::NaiveDate::parse_from_str(&input, "%Y-%m-%d") {
                Ok(date) => {
                    apply_due_date(board, state, Some(date), kando_dir)?;
                    sync_message = Some("Set due date".into());
                }
                Err(_) => {
                    state.notify_error("Invalid date format. Use YYYY-MM-DD");
                }
            }
        }
        Mode::Input {
            buf,
            on_confirm: InputTarget::PipeCommand,
            ..
        } => {
            let cmd = buf.input.trim().to_string();
            if !cmd.is_empty() {
                if let Some(card) = state.selected_card_ref(board) {
                    let col_slug = board.columns[state.focused_column].slug.clone();
                    let col_name = board.columns[state.focused_column].name.clone();
                    let card_id = card.id.clone();
                    let card_title = card.title.clone();
                    let card_tags = card.tags.join(",");
                    let card_assignees = card.assignees.join(",");
                    let card_priority = card.priority.to_string();
                    let card_path = kando_dir.join("columns").join(&col_slug)
                        .join(format!("{card_id}.md"));
                    let contents = std::fs::read_to_string(&card_path).unwrap_or_default();

                    let mut child = match std::process::Command::new("sh")
                        .arg("-c")
                        .arg(&cmd)
                        .env("KANDO_CARD_ID", &card_id)
                        .env("KANDO_CARD_TITLE", &card_title)
                        .env("KANDO_CARD_TAGS", &card_tags)
                        .env("KANDO_CARD_ASSIGNEES", &card_assignees)
                        .env("KANDO_CARD_PRIORITY", &card_priority)
                        .env("KANDO_CARD_COLUMN", &col_name)
                        .stdin(std::process::Stdio::piped())
                        .stdout(std::process::Stdio::piped())
                        .stderr(std::process::Stdio::piped())
                        .spawn() {
                        Ok(c) => c,
                        Err(e) => {
                            state.notify_error(format!("Failed to spawn command: {e}"));
                            return Ok(None);
                        }
                    };

                    // Write stdin on a separate thread to avoid deadlocks
                    let mut stdin = child.stdin.take().expect("stdin was piped");
                    let write_handle = std::thread::spawn(move || {
                        let _ = std::io::Write::write_all(&mut stdin, contents.as_bytes());
                    });

                    match child.wait_with_output() {
                        Ok(output) => {
                            let _ = write_handle.join();
                            if output.status.success() {
                                let first_line = String::from_utf8_lossy(&output.stdout)
                                    .lines()
                                    .find(|l| !l.is_empty())
                                    .unwrap_or("Pipe complete")
                                    .to_string();
                                state.notify(first_line);
                            } else {
                                let stderr_line = String::from_utf8_lossy(&output.stderr)
                                    .lines()
                                    .next()
                                    .map(|s| s.to_string());
                                let msg = stderr_line.unwrap_or_else(|| {
                                    format!("Command exited with {}", output.status)
                                });
                                state.notify_error(msg);
                            }
                        }
                        Err(e) => {
                            let _ = write_handle.join();
                            state.notify_error(format!("Command failed: {e}"));
                        }
                    }
                } else {
                    state.notify_error("No card selected");
                }
            }
        }
        Mode::Filter { buf } => {
            if buf.input.trim().is_empty() {
                state.active_filter = None;
            } else {
                state.active_filter = Some(buf.input);
            }
            state.clamp_selection_filtered(board);
            // Auto-jump to first visible card after filter is confirmed
            if state.has_active_filter() {
                jump_to_visible_card(board, state, true);
            }
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
                PickerTarget::StalenessFilter => {
                    if let Some((label, _)) = items.get(selected) {
                        let label = label.clone();
                        if state.active_staleness_filters.contains(&label) {
                            state.active_staleness_filters.retain(|l| *l != label);
                        } else {
                            state.active_staleness_filters.push(label);
                        }
                    }
                    for (label, active) in items.iter_mut() {
                        *active = state.active_staleness_filters.contains(label);
                    }
                    state.clamp_selection_filtered(board);
                    state.mode = Mode::Picker { title, items, selected, target: PickerTarget::StalenessFilter };
                }
                PickerTarget::Priority => {
                    use crate::board::Priority;
                    if let Some(priority) = Priority::ALL.get(selected) {
                        let col_idx = state.focused_column;
                        let card_idx = state.selected_card;
                        if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                            let card_id = card.id.clone();
                            let card_title = card.title.clone();
                            card.priority = *priority;
                            card.touch();
                            let new_priority = card.priority.as_str().to_string();
                            let priority_str = format!("Priority: {new_priority}");
                            board.columns[col_idx].sort_cards();
                            let col_name = board.columns[col_idx].name.clone();
                            save_board(kando_dir, board)?;
                            append_activity(kando_dir, "priority", &card_id, &card_title,
                                &[("column", &col_name), ("priority", &new_priority)]);
                            sync_message = Some(format!("Update priority of #{card_id} \"{card_title}\" to {new_priority}"));
                            state.clamp_selection_filtered(board);
                            state.notify(priority_str);
                        }
                    }
                }
                PickerTarget::SortColumn => {
                    if let Some((field, _)) = items.get(selected) {
                        let col_idx = state.focused_column;
                        if let Some(col) = board.columns.get_mut(col_idx) {
                            match field.as_str() {
                                "priority" => {
                                    col.sort_cards();
                                    state.notify("Sorted by priority");
                                }
                                "created" => {
                                    col.cards.sort_by(|a, b| b.created.cmp(&a.created));
                                    state.notify("Sorted by created");
                                }
                                "updated" => {
                                    col.cards.sort_by(|a, b| b.updated.cmp(&a.updated));
                                    state.notify("Sorted by updated");
                                }
                                "title" => {
                                    col.cards.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
                                    state.notify("Sorted by title");
                                }
                                _ => {}
                            }
                            state.clamp_selection_filtered(board);
                            save_board(kando_dir, board)?;
                            sync_message = Some("Sort column".into());
                        }
                    }
                }
                PickerTarget::DueDate => {
                    if let Some((choice, _)) = items.get(selected) {
                        let today = chrono::Utc::now().date_naive();
                        match choice.as_str() {
                            "custom" => {
                                state.mode = Mode::Input {
                                    prompt: "Due date (YYYY-MM-DD)",
                                    buf: TextBuffer::empty(),
                                    on_confirm: InputTarget::DueDate,
                                    completion: None,
                                };
                            }
                            "clear" => {
                                apply_due_date(board, state, None, kando_dir)?;
                                sync_message = Some("Clear due date".into());
                            }
                            _ => {
                                let date = match choice.as_str() {
                                    "tomorrow" => today + chrono::Days::new(1),
                                    "+3 days" => today + chrono::Days::new(3),
                                    "+1 week" => today + chrono::Days::new(7),
                                    "+2 weeks" => today + chrono::Days::new(14),
                                    // checked_add_months returns None when target day doesn't exist
                    // (e.g., Jan 31 + 1 month = Feb 31). Fall back to +30 days.
                    "+1 month" => today.checked_add_months(chrono::Months::new(1))
                                        .unwrap_or(today + chrono::Days::new(30)),
                                    _ => today,
                                };
                                apply_due_date(board, state, Some(date), kando_dir)?;
                                sync_message = Some("Set due date".into());
                            }
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
                            let card_id = board.columns[from].cards.get(card_idx)
                                .map(|c| c.id.clone()).unwrap_or_default();
                            let card_title = board.columns[from].cards.get(card_idx)
                                .map(|c| c.title.clone()).unwrap_or_default();
                            let from_name = board.columns[from].name.clone();
                            let to_name = col_name.clone();
                            board.move_card(from, card_idx, to);
                            board.columns[to].sort_cards();
                            state.focused_column = to;
                            state.clamp_selection_filtered(board);
                            save_board(kando_dir, board)?;
                            append_activity(kando_dir, "move", &card_id, &card_title,
                                &[("from", &from_name), ("to", &to_name)]);
                            sync_message = Some(format!("Move card #{card_id} \"{card_title}\" from {from_name} to {to_name}"));
                            state.notify(format!("Moved to {col_name}"));
                        }
                    }
                }
            }
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
            let target_col_name = board.columns[target_col].name.clone();
            append_activity(kando_dir, "restore", &entry.id, &entry.title,
                &[("column", &target_col_name)]);
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
                    target_col_name,
                ));
            } else {
                state.notify(format!("Restored: {}", entry.title));
            }
            Ok(Some(format!("Restore card #{} \"{}\" to {}", entry.id, entry.title, target_col_name)))
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
                        let col_name = board.columns[col_idx].name.clone();
                        let card_title = board.columns[col_idx].cards[card_idx].title.clone();
                        // Trash the file BEFORE removing from board (save_board deletes orphans)
                        match trash_card(kando_dir, &col_slug, &id, &card_title) {
                            Ok(entry) => {
                                board.columns[col_idx].cards.remove(card_idx);
                                save_board(kando_dir, board)?;
                                append_activity(kando_dir, "delete", &id, &card_title,
                                    &[("column", &col_name)]);
                                sync_message = Some(format!("Delete card #{id} \"{card_title}\" from {col_name}"));
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
                    let card_id = board.columns[from].cards.get(ci)
                        .map(|c| c.id.clone()).unwrap_or_default();
                    let card_title = board.columns[from].cards.get(ci)
                        .map(|c| c.title.clone()).unwrap_or_default();
                    let from_name = board.columns[from].name.clone();
                    let to_name = board.columns[to].name.clone();
                    board.move_card(from, ci, to);
                    board.columns[to].sort_cards();
                    state.focused_column = to;
                    state.clamp_selection_filtered(board);
                    save_board(kando_dir, board)?;
                    append_activity(kando_dir, "move", &card_id, &card_title,
                        &[("from", &from_name), ("to", &to_name)]);
                    sync_message = Some(format!("Move card #{card_id} \"{card_title}\" from {from_name} to {to_name}"));
                    state.notify("Card moved");
                }
                Mode::Confirm {
                    on_confirm: ConfirmTarget::ColRemove(slug),
                    ..
                } => {
                    use crate::board::normalize_column_orders;
                    use crate::board::storage::remove_column_dir;
                    let slug = slug.clone();
                    let col_idx = match board.columns.iter().position(|c| c.slug == slug) {
                        Some(i) => i,
                        None => {
                            state.notify_error("Column no longer exists");
                            state.mode = Mode::Normal;
                            return Ok(None);
                        }
                    };
                    let name = board.columns[col_idx].name.clone();
                    let focused_slug = board.columns.get(state.focused_column).map(|c| c.slug.clone());
                    board.columns.remove(col_idx);
                    normalize_column_orders(&mut board.columns);
                    save_board(kando_dir, board)?;
                    remove_column_dir(kando_dir, &slug)?;
                    append_activity(kando_dir, "col-remove", &slug, &name, &[]);
                    state.focused_column = focused_slug
                        .and_then(|s| board.columns.iter().position(|c| c.slug == s))
                        .unwrap_or(0);
                    state.clamp_selection(board);
                    state.notify(format!("Removed column: {name}"));
                    sync_message = Some(format!("Remove column \"{name}\""));
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
    use crate::board::{Board, Card, Column, Policies};

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
    // Visibility toggle tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_toggle_hidden_columns() {
        let board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        assert!(!state.show_hidden_columns);
        handle_visibility_toggle(&board, &mut state, Action::ToggleHiddenColumns);
        assert!(state.show_hidden_columns);
        assert_eq!(state.notification.as_deref(), Some("Showing hidden columns"));
        handle_visibility_toggle(&board, &mut state, Action::ToggleHiddenColumns);
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
        handle_visibility_toggle(&board, &mut state, Action::ToggleHiddenColumns);
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
            completion: None,
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
            completion: None,
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
            completion: None,
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
            title: "tag",
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
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Create card #1 \"My task\" in Backlog"));
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
            completion: None,
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
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(sync.as_deref(), Some("Update tags on #001 \"Test\""));
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
        assert_eq!(sync.as_deref(), Some("Delete card #001 \"Doomed\" from Backlog"));
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
        assert_eq!(sync.as_deref(), Some("Move card #001 \"Card\" from Backlog to In Progress"));
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
        assert_eq!(sync.as_deref(), Some("Block #001 \"Test\""));
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
        assert_eq!(sync.as_deref(), Some("Restore card #001 \"Undone\" to Backlog"));

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
    fn test_help_scroll() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Help { scroll: 0 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::DetailScrollDown, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Help { scroll } = state.mode { assert_eq!(scroll, 1); } else { panic!("expected Mode::Help"); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Help { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Help"); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Help { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Help"); }
    }

    #[test]
    fn test_tutorial_scroll() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Tutorial { scroll: 0 };
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_card_action(&mut board, &mut state, Action::DetailScrollDown, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Tutorial { scroll } = state.mode { assert_eq!(scroll, 1); } else { panic!("expected Mode::Tutorial"); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Tutorial { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Tutorial"); }
        handle_card_action(&mut board, &mut state, Action::DetailScrollUp, &mut terminal, kando_dir, false).unwrap();
        if let Mode::Tutorial { scroll } = state.mode { assert_eq!(scroll, 0); } else { panic!("expected Mode::Tutorial"); }
    }

    #[test]
    fn test_close_help_panel() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.mode = Mode::Help { scroll: 3 };
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

    // ── TextBuffer tests ──

    #[test]
    fn textbuffer_new_cursor_at_end() {
        let buf = TextBuffer::new("hello".into());
        assert_eq!(buf.cursor, 5);
        assert_eq!(buf.input, "hello");
    }

    #[test]
    fn textbuffer_empty_cursor_at_zero() {
        let buf = TextBuffer::empty();
        assert_eq!(buf.cursor, 0);
        assert_eq!(buf.input, "");
    }

    #[test]
    fn textbuffer_insert_at_end() {
        let mut buf = TextBuffer::new("hel".into());
        buf.insert('l');
        buf.insert('o');
        assert_eq!(buf.input, "hello");
        assert_eq!(buf.cursor, 5);
    }

    #[test]
    fn textbuffer_insert_in_middle() {
        let mut buf = TextBuffer::new("hllo".into());
        buf.cursor = 1; // after 'h'
        buf.insert('e');
        assert_eq!(buf.input, "hello");
        assert_eq!(buf.cursor, 2);
    }

    #[test]
    fn textbuffer_insert_at_start() {
        let mut buf = TextBuffer::new("ello".into());
        buf.cursor = 0;
        buf.insert('h');
        assert_eq!(buf.input, "hello");
        assert_eq!(buf.cursor, 1);
    }

    #[test]
    fn textbuffer_backspace_at_end() {
        let mut buf = TextBuffer::new("hello".into());
        buf.backspace();
        assert_eq!(buf.input, "hell");
        assert_eq!(buf.cursor, 4);
    }

    #[test]
    fn textbuffer_backspace_in_middle() {
        let mut buf = TextBuffer::new("hello".into());
        buf.cursor = 2; // after 'he'
        buf.backspace();
        assert_eq!(buf.input, "hllo");
        assert_eq!(buf.cursor, 1);
    }

    #[test]
    fn textbuffer_backspace_at_start_does_nothing() {
        let mut buf = TextBuffer::new("hello".into());
        buf.cursor = 0;
        buf.backspace();
        assert_eq!(buf.input, "hello");
        assert_eq!(buf.cursor, 0);
    }

    #[test]
    fn textbuffer_delete_word_removes_last_word() {
        let mut buf = TextBuffer::new("hello world".into());
        buf.delete_word();
        assert_eq!(buf.input, "hello ");
        assert_eq!(buf.cursor, 6);
    }

    #[test]
    fn textbuffer_delete_word_single_word() {
        let mut buf = TextBuffer::new("hello".into());
        buf.delete_word();
        assert_eq!(buf.input, "");
        assert_eq!(buf.cursor, 0);
    }

    #[test]
    fn textbuffer_delete_word_trailing_spaces() {
        let mut buf = TextBuffer::new("hello   ".into());
        buf.delete_word();
        assert_eq!(buf.input, "");
        assert_eq!(buf.cursor, 0);
    }

    #[test]
    fn textbuffer_move_left_right() {
        let mut buf = TextBuffer::new("abc".into());
        assert_eq!(buf.cursor, 3);
        buf.move_left();
        assert_eq!(buf.cursor, 2);
        buf.move_left();
        assert_eq!(buf.cursor, 1);
        buf.move_right();
        assert_eq!(buf.cursor, 2);
    }

    #[test]
    fn textbuffer_move_left_at_start_stays() {
        let mut buf = TextBuffer::new("abc".into());
        buf.cursor = 0;
        buf.move_left();
        assert_eq!(buf.cursor, 0);
    }

    #[test]
    fn textbuffer_move_right_at_end_stays() {
        let mut buf = TextBuffer::new("abc".into());
        buf.move_right();
        assert_eq!(buf.cursor, 3);
    }

    #[test]
    fn textbuffer_home_end() {
        let mut buf = TextBuffer::new("hello".into());
        buf.home();
        assert_eq!(buf.cursor, 0);
        buf.end();
        assert_eq!(buf.cursor, 5);
    }

    #[test]
    fn textbuffer_unicode_insert() {
        let mut buf = TextBuffer::new("café".into());
        assert_eq!(buf.cursor, 4); // 4 chars, not 5 bytes
        buf.insert('!');
        assert_eq!(buf.input, "café!");
        assert_eq!(buf.cursor, 5);
    }

    #[test]
    fn textbuffer_unicode_insert_middle() {
        let mut buf = TextBuffer::new("café".into());
        buf.cursor = 2; // after 'ca'
        buf.insert('r');
        assert_eq!(buf.input, "carfé");
        assert_eq!(buf.cursor, 3);
    }

    #[test]
    fn textbuffer_unicode_backspace() {
        let mut buf = TextBuffer::new("café".into());
        buf.backspace(); // should remove 'é' (multi-byte)
        assert_eq!(buf.input, "caf");
        assert_eq!(buf.cursor, 3);
    }

    #[test]
    fn textbuffer_emoji_operations() {
        let mut buf = TextBuffer::new("a🎉b".into());
        assert_eq!(buf.cursor, 3); // 3 chars
        buf.move_left();
        assert_eq!(buf.cursor, 2); // after emoji
        buf.move_left();
        assert_eq!(buf.cursor, 1); // after 'a'
        buf.insert('X');
        assert_eq!(buf.input, "aX🎉b");
        assert_eq!(buf.cursor, 2);
    }

    #[test]
    fn textbuffer_emoji_backspace() {
        let mut buf = TextBuffer::new("a🎉b".into());
        buf.cursor = 2; // after emoji
        buf.backspace(); // should remove emoji
        assert_eq!(buf.input, "ab");
        assert_eq!(buf.cursor, 1);
    }

    #[test]
    fn textbuffer_home_end_with_unicode() {
        let buf_str = "héllo 🌍";
        let mut buf = TextBuffer::new(buf_str.into());
        let char_count = buf_str.chars().count();
        assert_eq!(buf.cursor, char_count);
        buf.home();
        assert_eq!(buf.cursor, 0);
        buf.end();
        assert_eq!(buf.cursor, char_count);
    }

    // -----------------------------------------------------------------------
    // AppState helper method tests
    // -----------------------------------------------------------------------

    #[test]
    fn selected_card_ref_valid() {
        let board = test_board(&[("A", &["c1", "c2"])]);
        let state = AppState::new();
        assert!(state.selected_card_ref(&board).is_some());
    }

    #[test]
    fn selected_card_ref_invalid_column() {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.focused_column = 99;
        assert!(state.selected_card_ref(&board).is_none());
    }

    #[test]
    fn selected_card_ref_invalid_card() {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.selected_card = 99;
        assert!(state.selected_card_ref(&board).is_none());
    }

    #[test]
    fn selected_card_metadata_with_card() {
        let mut board = test_board(&[("A", &["c1"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];
        board.columns[0].cards[0].assignees = vec!["alice".into()];
        let state = AppState::new();
        let (tags, assignees) = state.selected_card_metadata(&board);
        assert_eq!(tags, vec!["bug"]);
        assert_eq!(assignees, vec!["alice"]);
    }

    #[test]
    fn selected_card_metadata_no_card() {
        let board = test_board(&[("A", &[])]);
        let state = AppState::new();
        let (tags, assignees) = state.selected_card_metadata(&board);
        assert!(tags.is_empty());
        assert!(assignees.is_empty());
    }

    #[test]
    fn notify_sets_info_level() {
        let mut state = AppState::new();
        state.notify("hello");
        assert_eq!(state.notification, Some("hello".into()));
        assert_eq!(state.notification_level, NotificationLevel::Info);
        assert!(state.notification_expires.is_some());
    }

    #[test]
    fn notify_error_sets_error_level() {
        let mut state = AppState::new();
        state.notify_error("oops");
        assert_eq!(state.notification, Some("oops".into()));
        assert_eq!(state.notification_level, NotificationLevel::Error);
    }

    #[test]
    fn tick_notification_clears_expired() {
        let mut state = AppState::new();
        state.notification = Some("msg".into());
        state.notification_expires = Some(Instant::now() - Duration::from_secs(1));
        state.tick_notification();
        assert!(state.notification.is_none());
    }

    #[test]
    fn tick_notification_keeps_unexpired() {
        let mut state = AppState::new();
        state.notify("msg");
        state.tick_notification();
        assert!(state.notification.is_some());
    }

    #[test]
    fn has_active_filter_none() {
        let state = AppState::new();
        assert!(!state.has_active_filter());
    }

    #[test]
    fn has_active_filter_text_only() {
        let mut state = AppState::new();
        state.active_filter = Some("search".into());
        assert!(state.has_active_filter());
    }

    #[test]
    fn has_active_filter_tags_only() {
        let mut state = AppState::new();
        state.active_tag_filters = vec!["bug".into()];
        assert!(state.has_active_filter());
    }

    #[test]
    fn has_active_filter_assignees_only() {
        let mut state = AppState::new();
        state.active_assignee_filters = vec!["alice".into()];
        assert!(state.has_active_filter());
    }

    #[test]
    fn has_active_filter_staleness_only() {
        let mut state = AppState::new();
        state.active_staleness_filters = vec!["stale".into()];
        assert!(state.has_active_filter());
    }

    // ── clamp_selection tests ──

    #[test]
    fn clamp_selection_empty_column() {
        let board = test_board(&[("A", &[])]);
        let mut state = AppState::new();
        state.selected_card = 5;
        state.clamp_selection(&board);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn clamp_selection_out_of_bounds() {
        let board = test_board(&[("A", &["c1", "c2"])]);
        let mut state = AppState::new();
        state.selected_card = 99;
        state.clamp_selection(&board);
        assert_eq!(state.selected_card, 1); // cards.len() - 1
    }

    #[test]
    fn clamp_selection_within_bounds_unchanged() {
        let board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 1;
        state.clamp_selection(&board);
        assert_eq!(state.selected_card, 1);
    }

    // ── next_visible_column tests ──

    #[test]
    fn next_visible_column_forward() {
        let board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        assert_eq!(next_visible_column(&board, 0, true, false), Some(1));
    }

    #[test]
    fn next_visible_column_backward() {
        let board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        assert_eq!(next_visible_column(&board, 2, false, false), Some(1));
    }

    #[test]
    fn next_visible_column_skips_hidden() {
        let mut board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        board.columns[1].hidden = true;
        assert_eq!(next_visible_column(&board, 0, true, false), Some(2));
    }

    #[test]
    fn next_visible_column_shows_hidden_when_flag_set() {
        let mut board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        board.columns[1].hidden = true;
        assert_eq!(next_visible_column(&board, 0, true, true), Some(1));
    }

    #[test]
    fn next_visible_column_at_edge_returns_none() {
        let board = test_board(&[("A", &[]), ("B", &[])]);
        assert_eq!(next_visible_column(&board, 1, true, false), None);
    }

    #[test]
    fn next_visible_column_all_hidden_returns_none() {
        let mut board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        board.columns[1].hidden = true;
        board.columns[2].hidden = true;
        assert_eq!(next_visible_column(&board, 0, true, false), None);
    }

    // ── visible_card_indices tests ──

    #[test]
    fn visible_card_indices_no_filter() {
        let board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let state = AppState::new();
        let indices = visible_card_indices(&board.columns[0], &state, &board);
        assert_eq!(indices, vec![0, 1, 2]);
    }

    #[test]
    fn visible_card_indices_text_filter() {
        let board = test_board(&[("A", &["apple", "banana", "avocado"])]);
        let mut state = AppState::new();
        state.active_filter = Some("a".into()); // matches apple and avocado (and banana has 'a' too)
        let indices = visible_card_indices(&board.columns[0], &state, &board);
        // Fuzzy match: "a" matches all three titles
        assert!(!indices.is_empty());
    }

    #[test]
    fn visible_card_indices_tag_filter() {
        let mut board = test_board(&[("A", &["c1", "c2"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];
        let mut state = AppState::new();
        state.active_tag_filters = vec!["bug".into()];
        let indices = visible_card_indices(&board.columns[0], &state, &board);
        assert_eq!(indices, vec![0]); // only first card has "bug" tag
    }

    // ── clamp_selection_filtered tests ──

    #[test]
    fn clamp_selection_filtered_no_filter_delegates() {
        let board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 99;
        state.clamp_selection_filtered(&board);
        assert_eq!(state.selected_card, 2); // clamped to last
    }

    #[test]
    fn clamp_selection_filtered_snaps_to_visible() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];
        board.columns[0].cards[2].tags = vec!["bug".into()];
        let mut state = AppState::new();
        state.active_tag_filters = vec!["bug".into()];
        state.selected_card = 1; // card[1] is not visible under tag filter
        state.clamp_selection_filtered(&board);
        // Should snap to nearest visible: 0 or 2
        assert!(state.selected_card == 0 || state.selected_card == 2);
    }

    #[test]
    fn clamp_selection_filtered_no_visible_cards() {
        let board = test_board(&[("A", &["c1", "c2"])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["nonexistent".into()];
        state.selected_card = 1;
        state.clamp_selection_filtered(&board);
        assert_eq!(state.selected_card, 0);
    }

    // ── process_action edge case tests ──

    #[test]
    fn clear_filters_clears_all() {
        let mut state = AppState::new();
        state.active_filter = Some("search".into());
        state.active_tag_filters = vec!["bug".into()];
        state.active_assignee_filters = vec!["alice".into()];
        state.active_staleness_filters = vec!["stale".into()];
        assert!(state.has_active_filter());
        // Simulate the ClearFilters action effect
        state.active_filter = None;
        state.active_tag_filters.clear();
        state.active_assignee_filters.clear();
        state.active_staleness_filters.clear();
        assert!(!state.has_active_filter());
    }

    // -----------------------------------------------------------------------
    // Feature #10: Silent action failure notifications
    // -----------------------------------------------------------------------

    fn test_terminal() -> ratatui::Terminal<ratatui::backend::TestBackend> {
        ratatui::Terminal::new(ratatui::backend::TestBackend::new(80, 24)).unwrap()
    }

    fn fake_dir() -> &'static std::path::Path {
        std::path::Path::new("/tmp/fake")
    }

    // ── Filter guard ──

    #[test]
    fn filter_guard_notifies_no_visible_card_selected() {
        let mut board = test_board(&[("A", &["c1", "c2"])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["missing-tag".into()];
        state.selected_card = 0;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::ToggleBlocker, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("No visible card selected"));
    }

    #[test]
    fn filter_guard_does_not_block_new_card() {
        let mut board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["missing-tag".into()];
        state.selected_card = 0;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::NewCard, &mut terminal, fake_dir(), false).unwrap();
        // Guard exempts NewCard — mode should advance to input prompt
        assert!(matches!(state.mode, Mode::Input { on_confirm: InputTarget::NewCardTitle, .. }));
    }

    #[test]
    fn filter_guard_does_not_block_detail_nav_selected_not_in_visible_set() {
        // DetailNextCard/DetailPrevCard are exempt from the filter guard.
        // When selected_card is not in the visible set, they perform a silent no-op
        // (no notification, no state change) — distinct from the "last/first card" case.
        let mut board = test_board(&[("A", &["c1", "c2"])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["missing-tag".into()]; // nothing visible
        state.selected_card = 0;
        state.mode = Mode::CardDetail { scroll: 0 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailNextCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.notification.is_none());
        assert_eq!(state.selected_card, 0); // unchanged — no-op
        handle_card_action(&mut board, &mut state, Action::DetailPrevCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.notification.is_none());
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn filter_guard_passes_when_selected_card_is_visible() {
        let mut board = test_board(&[("A", &["c1"])]);
        board.columns[0].cards[0].tags = vec!["special".into()];
        let mut state = AppState::new();
        state.active_tag_filters = vec!["special".into()];
        state.selected_card = 0; // card 0 is visible (matches the tag filter)
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::OpenCardDetail, &mut terminal, fake_dir(), false).unwrap();
        assert!(matches!(state.mode, Mode::CardDetail { .. }));
        assert!(state.notification.is_none());
    }

    // ── try_move_card boundary and empty-column ──

    #[test]
    fn move_card_forward_at_last_column_notifies_last_column() {
        let mut board = test_board(&[("A", &["c1"]), ("B", &["c2"])]);
        let mut state = AppState::new();
        state.focused_column = 1;
        state.selected_card = 0;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::MoveCardNextColumn, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Last column"));
    }

    #[test]
    fn move_card_backward_at_first_column_notifies_first_column() {
        let mut board = test_board(&[("A", &["c1"]), ("B", &["c2"])]);
        let mut state = AppState::new();
        state.focused_column = 0;
        state.selected_card = 0;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::MoveCardPrevColumn, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("First column"));
    }

    #[test]
    fn move_card_from_empty_column_notifies_no_cards_to_move() {
        // Column A is empty but has a neighbour — boundary check passes, empty check fires.
        let mut board = test_board(&[("A", &[]), ("B", &["c1"])]);
        let mut state = AppState::new();
        state.focused_column = 0;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::MoveCardNextColumn, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("No cards to move"));
    }

    #[test]
    fn move_card_from_empty_last_column_notifies_last_column() {
        // Column B is empty AND the last column — boundary check fires first, not empty check.
        let mut board = test_board(&[("A", &["c1"]), ("B", &[])]);
        let mut state = AppState::new();
        state.focused_column = 1;
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::MoveCardNextColumn, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Last column"));
    }

    // ── Detail view navigation edges ──

    #[test]
    fn detail_next_card_at_last_card_notifies_last_card() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 2;
        state.mode = Mode::CardDetail { scroll: 0 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailNextCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Last card"));
        assert_eq!(state.selected_card, 2);
    }

    #[test]
    fn detail_prev_card_at_first_card_notifies_first_card() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 0;
        state.mode = Mode::CardDetail { scroll: 0 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailPrevCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("First card"));
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn detail_next_card_navigates_when_not_at_last() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 0;
        state.mode = Mode::CardDetail { scroll: 5 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailNextCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.selected_card, 1);
        assert!(matches!(state.mode, Mode::CardDetail { scroll: 0 }));
        assert!(state.notification.is_none());
    }

    #[test]
    fn detail_prev_card_navigates_when_not_at_first() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        let mut state = AppState::new();
        state.selected_card = 2;
        state.mode = Mode::CardDetail { scroll: 5 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailPrevCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.selected_card, 1);
        assert!(matches!(state.mode, Mode::CardDetail { scroll: 0 }));
        assert!(state.notification.is_none());
    }

    #[test]
    fn detail_next_card_at_last_visible_with_filter_notifies() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        board.columns[0].cards[0].tags = vec!["keep".into()];
        board.columns[0].cards[1].tags = vec!["keep".into()];
        // card 2 has no "keep" tag → visible = [0, 1]; card 1 is last visible
        let mut state = AppState::new();
        state.active_tag_filters = vec!["keep".into()];
        state.selected_card = 1;
        state.mode = Mode::CardDetail { scroll: 0 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailNextCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Last card"));
        assert_eq!(state.selected_card, 1);
    }

    #[test]
    fn detail_prev_card_at_first_visible_with_filter_notifies() {
        let mut board = test_board(&[("A", &["c1", "c2", "c3"])]);
        board.columns[0].cards[1].tags = vec!["keep".into()];
        board.columns[0].cards[2].tags = vec!["keep".into()];
        // card 0 has no "keep" tag → visible = [1, 2]; card 1 is first visible
        let mut state = AppState::new();
        state.active_tag_filters = vec!["keep".into()];
        state.selected_card = 1;
        state.mode = Mode::CardDetail { scroll: 0 };
        let mut terminal = test_terminal();
        handle_card_action(&mut board, &mut state, Action::DetailPrevCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("First card"));
        assert_eq!(state.selected_card, 1);
    }

    // ── Goto failures ──

    fn assert_jump_notifies_no_visible(action: Action) {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        state.active_tag_filters = vec!["missing-tag".into()];
        state.selected_card = 0;
        handle_goto(&board, &mut state, action);
        assert_eq!(state.notification.as_deref(), Some("No visible cards"));
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn jump_to_first_card_no_visible_cards_notifies() {
        assert_jump_notifies_no_visible(Action::JumpToFirstCard);
    }

    #[test]
    fn jump_to_last_card_no_visible_cards_notifies() {
        assert_jump_notifies_no_visible(Action::JumpToLastCard);
    }

    #[test]
    fn jump_to_backlog_missing_column_notifies() {
        let board = test_board(&[("Todo", &[]), ("Done", &[])]);
        let mut state = AppState::new();
        handle_goto(&board, &mut state, Action::JumpToBacklog);
        assert_eq!(state.notification.as_deref(), Some("No 'backlog' column"));
        assert_eq!(state.focused_column, 0);
    }

    #[test]
    fn jump_to_done_missing_column_notifies() {
        let board = test_board(&[("Todo", &[]), ("Backlog", &[])]);
        let mut state = AppState::new();
        handle_goto(&board, &mut state, Action::JumpToDone);
        assert_eq!(state.notification.as_deref(), Some("No 'done' column"));
        assert_eq!(state.focused_column, 0);
    }

    #[test]
    fn jump_to_backlog_hidden_notifies_when_show_hidden_off() {
        let mut board = test_board(&[("Backlog", &[])]);
        board.columns[0].slug = "backlog".into();
        board.columns[0].hidden = true;
        let mut state = AppState::new();
        state.show_hidden_columns = false;
        handle_goto(&board, &mut state, Action::JumpToBacklog);
        assert_eq!(state.notification.as_deref(), Some("No 'backlog' column"));
    }

    #[test]
    fn jump_to_done_hidden_notifies_when_show_hidden_off() {
        let mut board = test_board(&[("Done", &[])]);
        board.columns[0].slug = "done".into();
        board.columns[0].hidden = true;
        let mut state = AppState::new();
        state.show_hidden_columns = false;
        handle_goto(&board, &mut state, Action::JumpToDone);
        assert_eq!(state.notification.as_deref(), Some("No 'done' column"));
    }

    // -----------------------------------------------------------------------
    // handle_col_hide tests
    // -----------------------------------------------------------------------

    fn setup_col_hide() -> (tempfile::TempDir, std::path::PathBuf, Board, AppState) {
        let dir = tempfile::tempdir().unwrap();
        crate::board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = crate::board::storage::load_board(&kando_dir).unwrap();
        let state = AppState::new();
        (dir, kando_dir, board, state)
    }

    #[test]
    fn handle_col_hide_marks_column_hidden() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        // Board has 4 columns; hiding first is allowed.
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        assert!(board.columns[0].hidden);
    }

    #[test]
    fn handle_col_hide_last_visible_column_blocked() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        // Hide all columns except the first.
        board.columns[1].hidden = true;
        board.columns[2].hidden = true;
        board.columns[3].hidden = true;
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        // Column must remain visible.
        assert!(!board.columns[0].hidden);
        assert!(state.notification.as_deref().unwrap().contains("Cannot hide"));
    }

    #[test]
    fn handle_col_hide_two_visible_allows_hide() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        // Leave exactly 2 visible — hiding one should succeed.
        board.columns[2].hidden = true;
        board.columns[3].hidden = true;
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        assert!(board.columns[0].hidden, "should be allowed to hide when 2 visible remain");
    }

    #[test]
    fn handle_col_hide_focus_moves_to_first_visible_when_hidden_not_shown() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        // Focus is on col 0; show_hidden_columns is false (default).
        state.focused_column = 0;
        state.show_hidden_columns = false;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        // Focus should have moved off the now-hidden column 0.
        let focused = state.focused_column;
        assert!(!board.columns[focused].hidden, "new focus should be on a visible column");
    }

    #[test]
    fn handle_col_hide_mode_resets_to_normal() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        state.mode = Mode::Column;
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn handle_col_hide_activity_logged() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        let log = std::fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        assert!(log.contains("\"action\":\"col-hide\""));
    }

    #[test]
    fn handle_col_hide_board_persisted() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        let slug = board.columns[0].slug.clone();
        state.focused_column = 0;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        let reloaded = crate::board::storage::load_board(&kando_dir).unwrap();
        let col = reloaded.columns.iter().find(|c| c.slug == slug).unwrap();
        assert!(col.hidden, "hidden flag should be persisted to disk");
    }

    // -----------------------------------------------------------------------
    // Archive card tests
    // -----------------------------------------------------------------------

    #[test]
    fn archive_card_moves_to_archive_column() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        // Board already has an archive column from init_board
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        assert!(board.columns[archive_idx].cards.is_empty(), "precondition: archive should start empty");
        board.columns[0].cards.push(Card::new("001".into(), "My task".into()));
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        let sync = handle_card_action(&mut board, &mut state, Action::ArchiveCard, &mut terminal, &kando_dir, false).unwrap();
        assert!(sync.is_some());
        assert!(board.columns[0].cards.is_empty());
        assert_eq!(board.columns[archive_idx].cards.len(), 1);
        assert_eq!(board.columns[archive_idx].cards[0].title, "My task");
        assert!(state.notification.as_deref().unwrap().contains("Archived"));
    }

    #[test]
    fn archive_card_no_archive_column() {
        let mut board = test_board(&[("Todo", &["c1"])]);
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        handle_card_action(&mut board, &mut state, Action::ArchiveCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("No 'archive' column"));
    }

    #[test]
    fn archive_card_already_in_archive() {
        let mut board = test_board(&[("Todo", &[]), ("Archive", &["c1"])]);
        board.columns[1].slug = "archive".into();
        let mut state = AppState::new();
        state.focused_column = 1;
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        handle_card_action(&mut board, &mut state, Action::ArchiveCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("already in the archive"));
    }

    #[test]
    fn archive_card_empty_column() {
        let mut board = test_board(&[("Todo", &[]), ("Archive", &[])]);
        board.columns[1].slug = "archive".into();
        let mut state = AppState::new();
        let backend = ratatui::backend::TestBackend::new(80, 24);
        let mut terminal = ratatui::Terminal::new(backend).unwrap();
        handle_card_action(&mut board, &mut state, Action::ArchiveCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("No card selected"));
    }

    // -----------------------------------------------------------------------
    // Sort picker tests
    // -----------------------------------------------------------------------

    #[test]
    fn start_sort_opens_picker() {
        // Note: process_action takes DefaultTerminal and can't be called in tests.
        // We test the sort picker confirmation handler instead (sort_by_title).
        // This test verifies the expected Picker shape.
        let mut state = AppState::new();
        let items: Vec<(String, bool)> = ["priority", "created", "updated", "title"]
            .iter()
            .map(|s| (s.to_string(), false))
            .collect();
        state.mode = Mode::Picker {
            title: "sort by",
            items,
            selected: 0,
            target: PickerTarget::SortColumn,
        };
        assert!(matches!(state.mode, Mode::Picker { target: PickerTarget::SortColumn, .. }));
        if let Mode::Picker { items, .. } = &state.mode {
            assert_eq!(items.len(), 4);
            assert_eq!(items[0].0, "priority");
            assert_eq!(items[3].0, "title");
        }
    }

    #[test]
    fn sort_by_title() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].cards = vec![
            Card::new("001".into(), "Zebra".into()),
            Card::new("002".into(), "Apple".into()),
            Card::new("003".into(), "Mango".into()),
        ];
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Picker {
            title: "sort by",
            items: vec![("title".to_string(), false)],
            selected: 0,
            target: PickerTarget::SortColumn,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_some());
        assert_eq!(board.columns[0].cards[0].title, "Apple");
        assert_eq!(board.columns[0].cards[1].title, "Mango");
        assert_eq!(board.columns[0].cards[2].title, "Zebra");
    }

    #[test]
    fn sort_empty_column_no_panic() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        assert!(board.columns[0].cards.is_empty(), "precondition: column should start empty");
        let mut state = AppState::new();
        state.mode = Mode::Picker {
            title: "sort by",
            items: vec![("priority".to_string(), false)],
            selected: 0,
            target: PickerTarget::SortColumn,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_some());
        assert!(board.columns[0].cards.is_empty(), "column should still be empty after sort");
    }

    // -----------------------------------------------------------------------
    // WIP limit tests
    // -----------------------------------------------------------------------

    #[test]
    fn col_set_wip_opens_input() {
        let board = test_board(&[("A", &["c1"])]);
        let mut state = AppState::new();
        // Simulate process_action for ColSetWip
        if let Some(col) = board.columns.get(state.focused_column) {
            let current = col.wip_limit.map(|n| n.to_string()).unwrap_or_else(|| "0".into());
            state.mode = Mode::Input {
                prompt: "WIP limit (0=off)",
                buf: TextBuffer::new(current),
                on_confirm: InputTarget::WipLimit,
                completion: None,
            };
        }
        assert!(matches!(state.mode, Mode::Input { on_confirm: InputTarget::WipLimit, .. }));
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "0");
        }
    }

    #[test]
    fn wip_limit_set_valid() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "WIP limit (0=off)",
            buf: TextBuffer::new("3".into()),
            on_confirm: InputTarget::WipLimit,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(board.columns[0].wip_limit, Some(3));
        assert!(state.notification.as_deref().unwrap().contains("WIP limit"));
        assert!(state.notification.as_deref().unwrap().contains("3"));
    }

    #[test]
    fn wip_limit_clear_with_zero() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        board.columns[0].wip_limit = Some(5);
        crate::board::storage::save_board(&kando_dir, &board).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "WIP limit (0=off)",
            buf: TextBuffer::new("0".into()),
            on_confirm: InputTarget::WipLimit,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(board.columns[0].wip_limit, None);
        assert!(state.notification.as_deref().unwrap().contains("removed"));
    }

    #[test]
    fn wip_limit_invalid_input() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "WIP limit (0=off)",
            buf: TextBuffer::new("abc".into()),
            on_confirm: InputTarget::WipLimit,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("must be a number"));
        assert!(matches!(state.mode, Mode::Normal));
    }

    // -----------------------------------------------------------------------
    // Find next / prev (n/N) tests
    // -----------------------------------------------------------------------

    #[test]
    fn jump_to_visible_card_forward() {
        let board = test_board(&[("A", &["alpha-match", "alpha-nope"]), ("B", &["beta-match", "beta-nope"])]);
        let mut state = AppState::new();
        state.active_filter = Some("match".into());
        // Currently at (0, 0) = "alpha-match" which matches. Forward should skip "alpha-nope"
        // and jump to "beta-match" at (1, 0).
        jump_to_visible_card(&board, &mut state, true);
        assert_eq!(state.focused_column, 1);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn jump_to_visible_card_backward() {
        let board = test_board(&[("A", &["alpha-match", "alpha-nope"]), ("B", &["beta-match", "beta-nope"])]);
        let mut state = AppState::new();
        state.active_filter = Some("match".into());
        state.focused_column = 1;
        state.selected_card = 0;
        // Currently at "beta-match". Backward should wrap to "alpha-match".
        jump_to_visible_card(&board, &mut state, false);
        assert_eq!(state.focused_column, 0);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn jump_to_visible_card_wraps_forward() {
        let board = test_board(&[("A", &["alpha-match"]), ("B", &["beta-nope"])]);
        let mut state = AppState::new();
        state.active_filter = Some("alpha-match".into());
        // Only "alpha-match" matches. Forward from it should wrap back to itself.
        jump_to_visible_card(&board, &mut state, true);
        assert_eq!(state.focused_column, 0);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn jump_to_visible_card_no_matches() {
        let board = test_board(&[("A", &["hello"]), ("B", &["world"])]);
        let mut state = AppState::new();
        state.active_filter = Some("zzzznotfound".into());
        jump_to_visible_card(&board, &mut state, true);
        assert_eq!(state.notification.as_deref(), Some("No matches"));
    }

    #[test]
    fn jump_to_visible_card_skips_hidden_columns() {
        let mut board = test_board(&[("A", &["alpha-match"]), ("B", &["beta-match"]), ("C", &["gamma-match"])]);
        board.columns[1].hidden = true;
        let mut state = AppState::new();
        state.active_filter = Some("match".into());
        // At "alpha-match", forward should skip hidden "B" and land on "gamma-match".
        jump_to_visible_card(&board, &mut state, true);
        assert_eq!(state.focused_column, 2);
        assert_eq!(state.selected_card, 0);
    }

    // -----------------------------------------------------------------------
    // Column rename / add / remove action dispatch tests
    // -----------------------------------------------------------------------

    #[test]
    fn col_rename_selected_enters_input_mode() {
        let board = test_board(&[("Backlog", &[])]);
        let mut state = AppState::new();
        // Simulate ColRenameSelected dispatch
        if let Some(col) = board.columns.get(state.focused_column) {
            let slug = col.slug.clone();
            let current_name = col.name.clone();
            state.mode = Mode::Input {
                prompt: "Rename column",
                buf: TextBuffer::new(current_name),
                on_confirm: InputTarget::ColRename(slug),
                completion: None,
            };
        }
        assert!(matches!(state.mode, Mode::Input { on_confirm: InputTarget::ColRename(_), .. }));
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "Backlog");
        }
    }

    #[test]
    fn col_rename_confirm_changes_name() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let old_slug = board.columns[0].slug.clone();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Rename column",
            buf: TextBuffer::new("Todo Items".into()),
            on_confirm: InputTarget::ColRename(old_slug),
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_some());
        assert_eq!(board.columns[0].name, "Todo Items");
        assert!(state.notification.as_deref().unwrap().contains("Renamed"));
    }

    #[test]
    fn col_rename_empty_name_errors() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let old_slug = board.columns[0].slug.clone();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Rename column",
            buf: TextBuffer::new("  ".into()),
            on_confirm: InputTarget::ColRename(old_slug),
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("cannot be empty"));
    }

    #[test]
    fn col_add_before_enters_input_mode() {
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "New column name",
            buf: TextBuffer::empty(),
            on_confirm: InputTarget::ColAdd,
            completion: None,
        };
        assert!(matches!(state.mode, Mode::Input { on_confirm: InputTarget::ColAdd, .. }));
    }

    #[test]
    fn col_add_confirm_creates_column() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let original_len = board.columns.len();
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "New column name",
            buf: TextBuffer::new("Review".into()),
            on_confirm: InputTarget::ColAdd,
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_some());
        assert_eq!(board.columns.len(), original_len + 1);
        // New column inserted at position 0 (before focused column which is backlog)
        assert_eq!(board.columns[0].name, "Review");
    }

    #[test]
    fn col_remove_archive_blocked() {
        let mut board = test_board(&[("Archive", &[])]);
        board.columns[0].slug = "archive".into();
        let mut state = AppState::new();
        // Simulate ColRemoveSelected
        let col = &board.columns[0];
        let slug = col.slug.clone();
        if slug == "archive" {
            state.notify_error("The 'archive' column is reserved and cannot be removed");
        }
        assert!(state.notification.as_deref().unwrap().contains("reserved"));
    }

    #[test]
    fn col_remove_last_column_blocked() {
        let board = test_board(&[("Only", &[])]);
        let mut state = AppState::new();
        if board.columns.len() == 1 {
            state.notify_error("Cannot remove the last column");
        }
        assert!(state.notification.as_deref().unwrap().contains("last column"));
    }

    #[test]
    fn col_remove_with_cards_blocked() {
        let board = test_board(&[("Todo", &["card1"]), ("Done", &[])]);
        let mut state = AppState::new();
        let col = &board.columns[0];
        if !col.cards.is_empty() {
            state.notify_error(format!("Column has {} card(s); move them first", col.cards.len()));
        }
        assert!(state.notification.as_deref().unwrap().contains("1 card(s)"));
    }

    #[test]
    fn col_remove_confirm_removes_column() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let original_len = board.columns.len();
        // Find the "in-progress" column (empty by default) to remove
        let ip_idx = board.columns.iter().position(|c| c.slug == "in-progress").unwrap();
        let mut state = AppState::new();
        state.focused_column = ip_idx;
        state.mode = Mode::Confirm {
            prompt: "Delete column?",
            on_confirm: ConfirmTarget::ColRemove("in-progress".into()),
        };
        let sync = handle_confirm(&mut board, &mut state, Action::Confirm, &kando_dir).unwrap();
        assert!(sync.is_some());
        assert_eq!(board.columns.len(), original_len - 1);
        assert!(!board.columns.iter().any(|c| c.slug == "in-progress"));
    }

    // -----------------------------------------------------------------------
    // Column move tests
    // -----------------------------------------------------------------------

    #[test]
    fn col_move_left_at_leftmost_errors() {
        let _board = test_board(&[("A", &[]), ("B", &[])]);
        let mut state = AppState::new();
        state.focused_column = 0;
        state.notify_error("Already leftmost column");
        assert!(state.notification.as_deref().unwrap().contains("leftmost"));
    }

    #[test]
    fn col_move_right_at_rightmost_errors() {
        let _board = test_board(&[("A", &[]), ("B", &[])]);
        let mut state = AppState::new();
        state.focused_column = 1;
        state.notify_error("Already rightmost column");
        assert!(state.notification.as_deref().unwrap().contains("rightmost"));
    }

    #[test]
    fn col_move_to_position_same_notifies() {
        let board = test_board(&[("A", &[]), ("B", &[]), ("C", &[])]);
        let mut state = AppState::new();
        state.focused_column = 1;
        let target = 2usize - 1;
        if target == state.focused_column {
            state.notify(format!("{} is already at that position", board.columns[1].name));
        }
        assert!(state.notification.as_deref().unwrap().contains("already at that position"));
    }

    // -----------------------------------------------------------------------
    // Keymap binding tests for new features
    // -----------------------------------------------------------------------

    #[test]
    fn normal_s_maps_to_start_sort() {
        use crate::input::keymap::map_key;
        use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
        let action = map_key(KeyEvent::new(KeyCode::Char('s'), KeyModifiers::NONE), &Mode::Normal);
        assert_eq!(action, Action::StartSort);
    }

    #[test]
    fn normal_n_maps_to_find_next() {
        use crate::input::keymap::map_key;
        use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
        let action = map_key(KeyEvent::new(KeyCode::Char('n'), KeyModifiers::NONE), &Mode::Normal);
        assert_eq!(action, Action::FindNext);
    }

    #[test]
    fn normal_shift_n_maps_to_find_prev() {
        use crate::input::keymap::map_key;
        use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
        let action = map_key(KeyEvent::new(KeyCode::Char('N'), KeyModifiers::SHIFT), &Mode::Normal);
        assert_eq!(action, Action::FindPrev);
    }

    #[test]
    fn space_x_maps_to_archive_card() {
        use crate::input::keymap::map_key;
        use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
        let action = map_key(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE), &Mode::Space);
        assert_eq!(action, Action::ArchiveCard);
    }

    #[test]
    fn column_w_maps_to_col_set_wip() {
        use crate::input::keymap::map_key;
        use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
        let action = map_key(KeyEvent::new(KeyCode::Char('w'), KeyModifiers::NONE), &Mode::Column);
        assert_eq!(action, Action::ColSetWip);
    }

    // -----------------------------------------------------------------------
    // ColRenameSelected / ColAddBefore / ColRemoveSelected action dispatch
    // -----------------------------------------------------------------------

    #[test]
    fn handle_col_hide_focus_unchanged_when_show_hidden_true() {
        let (_dir, kando_dir, mut board, mut state) = setup_col_hide();
        // When hidden columns are shown, focus should stay on the now-hidden column.
        state.focused_column = 0;
        state.show_hidden_columns = true;
        handle_col_toggle_hidden(&mut board, &mut state, &kando_dir).unwrap();
        assert!(board.columns[0].hidden);
        assert_eq!(state.focused_column, 0, "focus should remain when hidden cols are shown");
    }

    // ── Tab-completion tests ──

    #[test]
    fn current_csv_token_empty() {
        let (token, start) = current_csv_token("");
        assert_eq!(token, "");
        assert_eq!(start, 0);
    }

    #[test]
    fn current_csv_token_single_word() {
        let (token, start) = current_csv_token("bug");
        assert_eq!(token, "bug");
        assert_eq!(start, 0);
    }

    #[test]
    fn current_csv_token_after_comma_with_space() {
        let (token, start) = current_csv_token("bug, ui");
        assert_eq!(token, "ui");
        assert_eq!(start, 5);
    }

    #[test]
    fn current_csv_token_trailing_comma() {
        let (token, start) = current_csv_token("bug,");
        assert_eq!(token, "");
        assert_eq!(start, 4);
    }

    #[test]
    fn entered_values_empty() {
        assert!(entered_values("").is_empty());
    }

    #[test]
    fn entered_values_single_token_returns_empty() {
        // Single token = the current one being typed, nothing "entered" yet
        assert!(entered_values("bug").is_empty());
    }

    #[test]
    fn entered_values_multiple_lowercased() {
        let vals = entered_values("Bug, UI, feature");
        assert_eq!(vals, vec!["bug", "ui"]);
    }

    #[test]
    fn entered_values_trailing_comma() {
        let vals = entered_values("bug,");
        assert_eq!(vals, vec!["bug"]);
    }

    #[test]
    fn filtered_candidates_prefix_match_case_insensitive() {
        let all = vec!["Bug".into(), "build".into(), "ui".into()];
        let result = filtered_candidates(&all, "bu", &[]);
        assert_eq!(result, vec!["Bug", "build"]);
    }

    #[test]
    fn filtered_candidates_excludes_already_entered() {
        let all = vec!["Bug".into(), "ui".into()];
        let already = vec!["bug".into()];
        let result = filtered_candidates(&all, "bu", &already);
        assert!(result.is_empty());
    }

    #[test]
    fn filtered_candidates_no_match() {
        let all = vec!["x".into()];
        let result = filtered_candidates(&all, "z", &[]);
        assert!(result.is_empty());
    }

    #[test]
    fn cycle_completion_forward_and_wrap() {
        let mut board = test_board(&[("Backlog", &["A", "B"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];
        board.columns[0].cards[1].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags (comma-separated)",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        // First Tab: picks first candidate
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert_eq!(buf.input, "bug");
            assert_eq!(completion.as_ref().unwrap().index, 0);
        } else {
            panic!("expected Input mode");
        }

        // Second Tab: cycles to next
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert_eq!(buf.input, "build");
            assert_eq!(completion.as_ref().unwrap().index, 1);
        } else {
            panic!("expected Input mode");
        }

        // Third Tab: wraps to first
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "bug");
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn cycle_completion_backward_wraps() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        // First Tab forward
        cycle_input_completion(&mut state, &board, true);
        // Then backward wraps to last
        cycle_input_completion(&mut state, &board, false);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert_eq!(buf.input, "build");
            assert_eq!(completion.as_ref().unwrap().index, 1);
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn cycle_completion_noop_for_non_tag_target() {
        let board = test_board(&[("Backlog", &["A"])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "New card",
            buf: TextBuffer::new("test".into()),
            on_confirm: InputTarget::NewCardTitle,
            completion: None,
        };
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { completion, .. } = &state.mode {
            assert!(completion.is_none());
        }
    }

    #[test]
    fn cycle_completion_excludes_already_entered() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bug, b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, .. } = &state.mode {
            // "bug" is already entered, so only "build" should be offered
            assert_eq!(buf.input, "bug, build");
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn ghost_text_shows_suffix() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["feature".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("fea".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        let ghost = compute_ghost_text(&state, &board);
        assert_eq!(ghost.as_deref(), Some("ture"));
    }

    #[test]
    fn ghost_text_none_when_cursor_not_at_end() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["feature".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer { input: "fea".into(), cursor: 1 },
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        assert!(compute_ghost_text(&state, &board).is_none());
    }

    #[test]
    fn ghost_text_none_during_active_completion() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: Some(CompletionState {
                candidates: vec!["bug".into()],
                index: 0,
            }),
        };

        assert!(compute_ghost_text(&state, &board).is_none());
    }

    #[test]
    fn ghost_text_none_for_empty_token() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bug, ".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        assert!(compute_ghost_text(&state, &board).is_none());
    }

    // -----------------------------------------------------------------------
    // compute_completion_hint tests
    // -----------------------------------------------------------------------

    #[test]
    fn completion_hint_none_in_normal_mode() {
        let board = test_board(&[("Backlog", &["A"])]);
        let state = AppState::new(); // Mode::Normal
        assert!(compute_completion_hint(&state, &board).is_none());
    }

    #[test]
    fn completion_hint_none_for_non_tag_target() {
        let board = test_board(&[("Backlog", &["A"])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Title",
            buf: TextBuffer::new("hello".into()),
            on_confirm: InputTarget::NewCardTitle,
            completion: None,
        };
        assert!(compute_completion_hint(&state, &board).is_none());
    }

    #[test]
    fn completion_hint_none_when_no_tags_on_board() {
        let board = test_board(&[("Backlog", &["A"])]);
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };
        assert!(compute_completion_hint(&state, &board).is_none());
    }

    #[test]
    fn completion_hint_none_when_no_prefix_match() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("z".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };
        assert!(compute_completion_hint(&state, &board).is_none());
    }

    #[test]
    fn completion_hint_returns_matching_candidates() {
        let mut board = test_board(&[("Backlog", &["A", "B"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];
        board.columns[0].cards[1].tags = vec!["feature".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates, vec!["bug", "build"]);
        assert_eq!(hint.selected, None);
    }

    #[test]
    fn completion_hint_selected_tracks_cycling_candidate() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into(), "batch".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("build".into()),
            on_confirm: InputTarget::EditTags,
            completion: Some(CompletionState {
                candidates: vec!["bug".into(), "build".into(), "batch".into()],
                index: 1, // cycling on "build"
            }),
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        // "build" should be found in the fresh candidate list
        assert!(hint.candidates.contains(&"build".to_string()), "expected 'build' in candidates");
        let pos = hint.candidates.iter().position(|c| c == "build").unwrap();
        assert_eq!(hint.selected, Some(pos));
    }

    #[test]
    fn completion_hint_selected_none_when_cycled_candidate_no_longer_matches() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bui".into()),
            on_confirm: InputTarget::EditTags,
            completion: Some(CompletionState {
                candidates: vec!["bug".into(), "build".into()],
                index: 0, // cycling on "bug"
            }),
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        // "bug" doesn't match prefix "bui", so fresh list is ["build"]
        assert_eq!(hint.candidates, vec!["build"]);
        assert_eq!(hint.selected, None); // "bug" not found in fresh list
    }

    #[test]
    fn completion_hint_excludes_already_entered() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bug, b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates, vec!["build"]);
    }

    #[test]
    fn completion_hint_all_values_entered_returns_none() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bug, b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        // "bug" is already entered, and there are no other tags starting with "b"
        assert!(compute_completion_hint(&state, &board).is_none());
    }

    #[test]
    fn completion_hint_works_for_assignees() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].assignees = vec!["alice".into(), "bob".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Assignees",
            buf: TextBuffer::new("a".into()),
            on_confirm: InputTarget::EditAssignees,
            completion: None,
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates, vec!["alice"]);
    }

    #[test]
    fn completion_hint_empty_token_shows_all_remaining() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "feature".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates.len(), 2);
    }

    #[test]
    fn completion_hint_stale_index_oob_gives_none_selected() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: Some(CompletionState {
                // Index 5 is out of bounds for the stale candidates list,
                // so get() returns None and selected becomes None.
                candidates: vec!["bug".into()],
                index: 5,
            }),
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.selected, None);
    }

    #[test]
    fn completion_hint_empty_stale_candidates_gives_none_selected() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: Some(CompletionState {
                candidates: vec![],
                index: 0,
            }),
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates, vec!["bug"]);
        assert_eq!(hint.selected, None);
    }

    #[test]
    fn cycle_completion_works_for_assignees() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].assignees = vec!["alice".into(), "adam".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Assignees",
            buf: TextBuffer::new("a".into()),
            on_confirm: InputTarget::EditAssignees,
            completion: None,
        };

        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert!(completion.is_some());
            assert_eq!(buf.input, "adam");
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn cycle_completion_noop_when_board_has_no_tags() {
        let board = test_board(&[("Backlog", &["A"])]);

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert_eq!(buf.input, "b");
            assert!(completion.is_none());
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn cycle_completion_noop_when_all_candidates_entered() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("bug, b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert_eq!(buf.input, "bug, b");
            assert!(completion.is_none());
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn ghost_text_works_for_assignees() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].assignees = vec!["alice".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Assignees",
            buf: TextBuffer::new("al".into()),
            on_confirm: InputTarget::EditAssignees,
            completion: None,
        };

        let ghost = compute_ghost_text(&state, &board);
        assert_eq!(ghost.as_deref(), Some("ice"));
    }

    #[test]
    fn ghost_text_none_when_board_has_no_tags() {
        let board = test_board(&[("Backlog", &["A"])]);

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        assert!(compute_ghost_text(&state, &board).is_none());
    }

    #[test]
    fn ghost_text_none_for_non_tag_target() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Title",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::NewCardTitle,
            completion: None,
        };

        assert!(compute_ghost_text(&state, &board).is_none());
    }

    #[test]
    fn typing_after_tab_cycle_resets_completion() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        // Tab-cycle to first candidate
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { completion, .. } = &state.mode {
            assert!(completion.is_some());
        } else {
            panic!("expected Input mode");
        }

        // Type a character — completion should be reset
        let kando_dir = std::path::Path::new("/tmp/fake");
        handle_input(&mut board, &mut state, Action::InputChar('x'), kando_dir).unwrap();
        if let Mode::Input { buf, completion, .. } = &state.mode {
            assert!(completion.is_none());
            assert!(buf.input.ends_with('x'));
        } else {
            panic!("expected Input mode");
        }
    }

    #[test]
    fn current_csv_token_multiple_commas() {
        let (token, start) = current_csv_token("a, b, c");
        assert_eq!(token, "c");
        assert_eq!(start, 6);
    }

    #[test]
    fn completion_hint_candidates_sorted_alphabetically() {
        let mut board = test_board(&[("Backlog", &["A", "B", "C"])]);
        // "zebra" appears 3 times, "alpha" once — by-count would put zebra first
        board.columns[0].cards[0].tags = vec!["zebra".into()];
        board.columns[0].cards[1].tags = vec!["zebra".into()];
        board.columns[0].cards[2].tags = vec!["zebra".into(), "alpha".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        let hint = compute_completion_hint(&state, &board).unwrap();
        assert_eq!(hint.candidates, vec!["alpha", "zebra"]);
    }

    #[test]
    fn cycle_completion_multi_token_replaces_only_last_segment() {
        let mut board = test_board(&[("Backlog", &["A"])]);
        board.columns[0].cards[0].tags = vec!["bug".into(), "build".into(), "feature".into()];

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Tags",
            buf: TextBuffer::new("feature, b".into()),
            on_confirm: InputTarget::EditTags,
            completion: None,
        };

        // First Tab: replaces "b" with first candidate ("bug", alphabetical)
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "feature, bug");
        } else {
            panic!("expected Input mode");
        }

        // Second Tab: replaces entire last segment with next candidate
        cycle_input_completion(&mut state, &board, true);
        if let Mode::Input { buf, .. } = &state.mode {
            assert_eq!(buf.input, "feature, build");
        } else {
            panic!("expected Input mode");
        }
    }

    // ── Pipe card tests ──

    #[test]
    fn pipe_card_enters_input_mode() {
        let board = test_board(&[("Todo", &["task1"])]);
        let mut state = AppState::new();
        let mut terminal = test_terminal();
        handle_card_action(&mut board.clone(), &mut state, Action::PipeCard, &mut terminal, fake_dir(), false).unwrap();
        match state.mode {
            Mode::Input { prompt, on_confirm: InputTarget::PipeCommand, .. } => {
                assert_eq!(prompt, "Pipe to command");
            }
            _ => panic!("Expected Input mode with PipeCommand target"),
        }
    }

    #[test]
    fn pipe_card_no_card_selected() {
        let board = test_board(&[("Empty", &[])]);
        let mut state = AppState::new();
        let mut terminal = test_terminal();
        handle_card_action(&mut board.clone(), &mut state, Action::PipeCard, &mut terminal, fake_dir(), false).unwrap();
        assert_eq!(state.notification.as_deref(), Some("No card selected"));
        assert_eq!(state.notification_level, NotificationLevel::Error);
    }

    #[test]
    fn pipe_card_does_not_clear_undo_state() {
        let board = test_board(&[("Todo", &["task1"])]);
        let mut state = AppState::new();
        state.last_delete = Some(TrashEntry {
            id: "001".into(),
            title: "deleted".into(),
            from_column: "todo".into(),
            deleted: Utc::now().to_rfc3339(),
        });
        let mut terminal = test_terminal();
        handle_card_action(&mut board.clone(), &mut state, Action::PipeCard, &mut terminal, fake_dir(), false).unwrap();
        assert!(state.last_delete.is_some());
    }

    #[test]
    fn pipe_command_success_shows_stdout_first_line() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        // Create a card so we have a file on disk
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("echo hello".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_none());
        assert_eq!(state.notification.as_deref(), Some("hello"));
        assert_eq!(state.notification_level, NotificationLevel::Info);
        assert!(matches!(state.mode, Mode::Normal));
    }

    #[test]
    fn pipe_command_empty_stdout_shows_pipe_complete() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("true".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Pipe complete"));
    }

    #[test]
    fn pipe_command_skips_empty_lines() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("printf '\\n\\nactual output\\n'".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(state.notification.as_deref(), Some("actual output"));
    }

    #[test]
    fn pipe_command_empty_input_does_nothing() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("   ".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(state.notification.is_none());
    }

    #[test]
    fn pipe_command_card_not_found() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        // No cards in the column
        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("echo test".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(state.notification.as_deref(), Some("No card selected"));
        assert_eq!(state.notification_level, NotificationLevel::Error);
    }

    #[test]
    fn pipe_command_nonzero_exit_shows_stderr() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("echo 'error msg' >&2; exit 1".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(state.notification.as_deref(), Some("error msg"));
        assert_eq!(state.notification_level, NotificationLevel::Error);
    }

    #[test]
    fn pipe_command_nonzero_exit_empty_stderr() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("exit 42".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(state.notification.as_deref().unwrap().contains("Command exited with"));
        assert_eq!(state.notification_level, NotificationLevel::Error);
    }

    #[test]
    fn pipe_command_receives_card_contents_on_stdin() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        // `head -1` outputs the first line of stdin
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("head -1".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        // Card files start with "---" (TOML frontmatter delimiter)
        assert_eq!(state.notification.as_deref(), Some("---"));
    }

    #[test]
    fn pipe_command_sets_env_vars() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let mut card = Card::new("1".into(), "Test Card".into());
        card.tags = vec!["bug".into(), "ui".into()];
        card.assignees = vec!["alice".into()];
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("printenv KANDO_CARD_TITLE".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert_eq!(state.notification.as_deref(), Some("Test Card"));
    }

    #[test]
    fn pipe_command_returns_no_sync_message() {
        let (_dir, kando_dir) = setup_kando_dir();
        let mut board = crate::board::storage::load_board(&kando_dir).unwrap();
        let card = Card::new("1".into(), "Test Card".into());
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        let mut state = AppState::new();
        state.mode = Mode::Input {
            prompt: "Pipe to command",
            buf: TextBuffer::new("echo ok".into()),
            on_confirm: InputTarget::PipeCommand,
            completion: None,
        };
        let sync = handle_input_confirm(&mut board, &mut state, &kando_dir).unwrap();
        assert!(sync.is_none());
    }
}
