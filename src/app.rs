use std::time::{Duration, Instant};

use chrono::Utc;
use crossterm::event::{self, Event};
use ratatui::DefaultTerminal;

use crate::board::age::run_auto_close;
use crate::board::storage::{find_kando_dir, load_board, save_board};
use crate::board::sync::{self, SyncState};
use crate::board::{Board, Card};
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
    Tutorial,
    Help,
}

#[derive(Debug, Clone)]
pub enum InputTarget {
    NewCardTitle,
    EditTags,
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
    pub notification: Option<String>,
    pub notification_level: NotificationLevel,
    pub notification_expires: Option<Instant>,
    pub should_quit: bool,
    pub sync_state: Option<SyncState>,
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
            notification: None,
            notification_level: NotificationLevel::Info,
            notification_expires: None,
            should_quit: false,
            sync_state: None,
        }
    }

    /// Get a reference to the currently selected card.
    pub fn selected_card_ref<'a>(&self, board: &'a Board) -> Option<&'a Card> {
        board
            .columns
            .get(self.focused_column)
            .and_then(|col| col.cards.get(self.selected_card))
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
    let closed = run_auto_close(board, Utc::now());
    if !closed.is_empty() {
        save_board(kando_dir, board)?;
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, kando_dir, "Auto-close stale cards");
        }
        state.notify(format!(
            "{} card{} auto-closed to Archive",
            closed.len(),
            if closed.len() == 1 { "" } else { "s" }
        ));
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
    state.clamp_selection(board);
    save_board(kando_dir, board)?;
    state.notify("Card moved");
    Ok(Some("Move card".into()))
}

/// Main TUI application loop.
pub fn run(terminal: &mut DefaultTerminal, start_dir: &std::path::Path) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(start_dir)?;
    let mut board = load_board(&kando_dir)?;
    let mut state = AppState::new();

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
    let was_minor_mode = matches!(state.mode, Mode::Goto | Mode::Space | Mode::View);
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
        Action::StartFilter | Action::StartTagFilter => {
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
        Action::EnterCommandMode => {
            state.mode = Mode::Command { cmd: crate::command::CommandState::new() };
        }

        // Board-level actions
        Action::ReloadBoard => {
            state.mode = Mode::Normal;
            *board = load_board(kando_dir)?;
            state.clamp_selection(board);
            state.notify("Board reloaded");
        }
        Action::ShowHelp => state.mode = Mode::Help,
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
            if state.active_filter.is_some() || !state.active_tag_filters.is_empty() {
                state.active_filter = None;
                state.active_tag_filters.clear();
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
                state.clamp_selection(board);
            }
        }
        Action::FocusNextColumn => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = next_visible_column(board, state.focused_column, true, state.show_hidden_columns) {
                state.focused_column = col;
                state.clamp_selection(board);
            }
        }
        Action::SelectPrevCard => {
            match &mut state.mode {
                Mode::Picker { selected, .. } => {
                    if *selected > 0 { *selected -= 1; }
                }
                _ => {
                    if was_minor_mode { state.mode = Mode::Normal; }
                    if state.selected_card > 0 { state.selected_card -= 1; }
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
                        if state.selected_card + 1 < col.cards.len() {
                            state.selected_card += 1;
                        }
                    }
                }
            }
        }
        Action::CycleNextCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if let Some(col) = board.columns.get(state.focused_column) {
                if state.selected_card + 1 < col.cards.len() {
                    state.selected_card += 1;
                } else {
                    let mut from = state.focused_column;
                    while let Some(next) = next_visible_column(board, from, true, state.show_hidden_columns) {
                        if !board.columns[next].cards.is_empty() {
                            state.focused_column = next;
                            state.selected_card = 0;
                            break;
                        }
                        from = next;
                    }
                }
            }
        }
        Action::CyclePrevCard => {
            if was_minor_mode { state.mode = Mode::Normal; }
            if state.selected_card > 0 {
                state.selected_card -= 1;
            } else {
                let mut from = state.focused_column;
                while let Some(prev) = next_visible_column(board, from, false, state.show_hidden_columns) {
                    if !board.columns[prev].cards.is_empty() {
                        state.focused_column = prev;
                        state.selected_card = board.columns[prev].cards.len() - 1;
                        break;
                    }
                    from = prev;
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
                state.selected_card = 0;
                state.clamp_selection(board);
            }
        }
        Action::JumpToFirstCard => {
            state.selected_card = 0;
        }
        Action::JumpToLastCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                state.selected_card = col.cards.len().saturating_sub(1);
            }
        }
        Action::JumpToBacklog => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "backlog" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.selected_card = 0;
                state.clamp_selection(board);
            }
        }
        Action::JumpToDone => {
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "done" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.selected_card = 0;
                state.clamp_selection(board);
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

                crossterm::terminal::disable_raw_mode()?;
                crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen)?;

                let _ = std::process::Command::new(&editor).arg(&card_path).status();

                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::EnterAlternateScreen,
                )?;
                crossterm::terminal::enable_raw_mode()?;

                terminal.clear()?;

                *board = load_board(kando_dir)?;
                if let Some((col_idx, card_idx)) = board.find_card(&card_id) {
                    board.columns[col_idx].cards[card_idx].touch();
                    board.columns[col_idx].sort_cards();
                    save_board(kando_dir, board)?;
                    sync_message = Some("Edit card".into());
                }
                state.clamp_selection(board);
                state.notify("Card updated");
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
                state.clamp_selection(board);
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
                state.clamp_selection(board);
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
        Action::OpenCardDetail => {
            if state.selected_card_ref(board).is_some() {
                state.mode = Mode::CardDetail { scroll: 0 };
            }
        }
        Action::ClosePanel => {
            state.mode = Mode::Normal;
        }
        Action::DetailScrollDown => {
            if let Mode::CardDetail { scroll } = &mut state.mode {
                *scroll = scroll.saturating_add(1);
            }
        }
        Action::DetailScrollUp => {
            if let Mode::CardDetail { scroll } = &mut state.mode {
                *scroll = scroll.saturating_sub(1);
            }
        }
        Action::DetailNextCard => {
            if let Some(col) = board.columns.get(state.focused_column) {
                if state.selected_card + 1 < col.cards.len() {
                    state.selected_card += 1;
                    state.mode = Mode::CardDetail { scroll: 0 };
                }
            }
        }
        Action::DetailPrevCard => {
            if state.selected_card > 0 {
                state.selected_card -= 1;
                state.mode = Mode::CardDetail { scroll: 0 };
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
                            state.clamp_selection(board);
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
            if is_filter { sync_filter(state); }
        }
        Action::InputBackspace => {
            let is_filter = matches!(state.mode, Mode::Filter { .. });
            match &mut state.mode {
                Mode::Input { buf, .. } | Mode::Filter { buf } => buf.backspace(),
                Mode::Command { cmd } => { cmd.buf.backspace(); crate::command::clear_completion(cmd); }
                _ => {}
            }
            if is_filter { sync_filter(state); }
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
            if is_filter { sync_filter(state); }
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
            // Extract card tags before mutably borrowing state.mode
            let card_tags: Vec<String> = state
                .selected_card_ref(board)
                .map(|c| c.tags.clone())
                .unwrap_or_default();
            if let Mode::Command { cmd } = &mut state.mode {
                crate::command::cycle_completion(cmd, board, &card_tags, forward);
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
            state.clamp_selection(board);
            state.notify("Tags updated");
        }
        Mode::Filter { buf } => {
            if buf.input.trim().is_empty() {
                state.active_filter = None;
            } else {
                state.active_filter = Some(buf.input);
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
                    state.mode = Mode::Picker { title, items, selected, target: PickerTarget::TagFilter };
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
                            state.clamp_selection(board);
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
                            state.clamp_selection(board);
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
                        let card_path = kando_dir
                            .join("columns")
                            .join(&board.columns[col_idx].slug)
                            .join(format!("{id}.md"));
                        let file_err = match std::fs::remove_file(&card_path) {
                            Ok(()) => None,
                            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                            Err(e) => Some(e),
                        };
                        board.columns[col_idx].cards.remove(card_idx);
                        save_board(kando_dir, board)?;
                        sync_message = Some("Delete card".into());
                        state.clamp_selection(board);
                        if let Some(e) = file_err {
                            state.notify(format!("Deleted (file removal failed: {e})"));
                        } else {
                            state.notify("Card deleted");
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
                    state.clamp_selection(board);
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
        assert_eq!(state.notification.as_deref(), Some("Card deleted"));
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
}
