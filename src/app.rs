use std::path::PathBuf;
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

/// Current interaction mode.
#[derive(Debug, Clone)]
pub enum Mode {
    Normal,
    Goto,
    Space,
    View,
    Input {
        prompt: &'static str,
        input: String,
        cursor: usize,
        on_confirm: InputTarget,
    },
    Confirm {
        prompt: &'static str,
        on_confirm: ConfirmTarget,
    },
    Filter {
        input: String,
        cursor: usize,
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

/// Global application state.
pub struct AppState {
    pub mode: Mode,
    pub focused_column: usize,
    pub selected_card: usize,
    pub show_hidden_columns: bool,
    pub active_filter: Option<String>,
    pub active_tag_filters: Vec<String>,
    pub notification: Option<String>,
    pub notification_expires: Option<Instant>,
    pub should_quit: bool,
    pub kando_dir: PathBuf,
    pub sync_state: Option<SyncState>,
}

impl AppState {
    pub fn new(kando_dir: PathBuf) -> Self {
        Self {
            mode: Mode::Normal,
            focused_column: 0,
            selected_card: 0,
            show_hidden_columns: false,
            active_filter: None,
            active_tag_filters: Vec::new(),
            notification: None,
            notification_expires: None,
            should_quit: false,
            kando_dir,
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
        self.notification_expires = Some(Instant::now() + Duration::from_secs(3));
    }

    /// Clear expired notifications.
    pub fn tick_notification(&mut self) {
        if let Some(expires) = self.notification_expires {
            if Instant::now() >= expires {
                self.notification = None;
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

    /// Count visible (non-hidden) columns.
    #[allow(dead_code)]
    fn visible_column_count(&self, board: &Board) -> usize {
        board
            .columns
            .iter()
            .filter(|c| !c.hidden || self.show_hidden_columns)
            .count()
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
    let mut state = AppState::new(kando_dir.clone());

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
    let closed = run_auto_close(&mut board, Utc::now());
    if !closed.is_empty() {
        save_board(&kando_dir, &board)?;
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, &kando_dir, "Auto-close stale cards");
        }
        state.notify(format!(
            "{} card{} auto-closed to Archive",
            closed.len(),
            if closed.len() == 1 { "" } else { "s" }
        ));
    }

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
            let closed = run_auto_close(&mut board, Utc::now());
            if !closed.is_empty() {
                save_board(&kando_dir, &board)?;
                if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, &kando_dir, "Auto-close stale cards");
        }
                state.notify(format!(
                    "{} card{} auto-closed to Archive",
                    closed.len(),
                    if closed.len() == 1 { "" } else { "s" }
                ));
            }
            last_auto_close = Instant::now();
        }

        // Periodic sync pull
        if state.sync_state.is_some() && last_sync.elapsed() >= sync_interval {
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
        terminal.draw(|f| crate::ui::render(f, &board, &state))?;

        // Handle input
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                let action = map_key(key, &state.mode);
                process_action(&mut board, &mut state, action, terminal)?;

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
) -> color_eyre::Result<()> {
    // For minor modes, return to normal after processing (unless entering a new mode)
    let was_minor_mode = matches!(state.mode, Mode::Goto | Mode::Space | Mode::View);
    let kando_dir = state.kando_dir.clone();
    let mut sync_message: Option<String> = None;

    match action {
        Action::None => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
        }

        // Navigation
        Action::FocusPrevColumn => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            if let Some(col) = next_visible_column(board, state.focused_column, false, state.show_hidden_columns) {
                state.focused_column = col;
                state.clamp_selection(board);
            }
        }
        Action::FocusNextColumn => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            if let Some(col) = next_visible_column(board, state.focused_column, true, state.show_hidden_columns) {
                state.focused_column = col;
                state.clamp_selection(board);
            }
        }
        Action::SelectPrevCard => {
            match &mut state.mode {
                Mode::Picker { selected, .. } => {
                    if *selected > 0 {
                        *selected -= 1;
                    }
                }
                _ => {
                    if was_minor_mode {
                        state.mode = Mode::Normal;
                    }
                    if state.selected_card > 0 {
                        state.selected_card -= 1;
                    }
                }
            }
        }
        Action::SelectNextCard => {
            match &mut state.mode {
                Mode::Picker { selected, items, .. } => {
                    if *selected + 1 < items.len() {
                        *selected += 1;
                    }
                }
                _ => {
                    if was_minor_mode {
                        state.mode = Mode::Normal;
                    }
                    if let Some(col) = board.columns.get(state.focused_column) {
                        if state.selected_card + 1 < col.cards.len() {
                            state.selected_card += 1;
                        }
                    }
                }
            }
        }

        Action::CycleNextCard => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            if let Some(col) = board.columns.get(state.focused_column) {
                if state.selected_card + 1 < col.cards.len() {
                    state.selected_card += 1;
                } else {
                    // Jump to next visible non-empty column
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
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            if state.selected_card > 0 {
                state.selected_card -= 1;
            } else {
                // Jump to previous visible non-empty column
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

        // Goto
        Action::JumpToColumn(idx) => {
            state.mode = Mode::Normal;
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
            state.mode = Mode::Normal;
            state.selected_card = 0;
        }
        Action::JumpToLastCard => {
            state.mode = Mode::Normal;
            if let Some(col) = board.columns.get(state.focused_column) {
                state.selected_card = col.cards.len().saturating_sub(1);
            }
        }
        Action::JumpToBacklog => {
            state.mode = Mode::Normal;
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "backlog" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.selected_card = 0;
                state.clamp_selection(board);
            }
        }
        Action::JumpToDone => {
            state.mode = Mode::Normal;
            if let Some(idx) = board.columns.iter().position(|c| {
                c.slug == "done" && (state.show_hidden_columns || !c.hidden)
            }) {
                state.focused_column = idx;
                state.selected_card = 0;
                state.clamp_selection(board);
            }
        }

        // Card movement
        Action::MoveCardPrevColumn => {
            if let Some(msg) = try_move_card(board, state, false, &kando_dir)? {
                sync_message = Some(msg);
            }
        }
        Action::MoveCardNextColumn => {
            if let Some(msg) = try_move_card(board, state, true, &kando_dir)? {
                sync_message = Some(msg);
            }
        }
        // Card actions
        Action::NewCard => {
            state.mode = Mode::Input {
                prompt: "New card",
                input: String::new(),
                cursor: 0,
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
            // Collect what we need before borrowing board mutably
            let card_info = state.selected_card_ref(board).map(|c| (c.id.clone(), board.columns[state.focused_column].slug.clone()));
            if let Some((card_id, col_slug)) = card_info {
                let card_path = kando_dir.join("columns").join(&col_slug).join(format!("{card_id}.md"));

                save_board(&kando_dir, board)?;

                let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vi".to_string());

                crossterm::terminal::disable_raw_mode()?;
                crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen)?;

                let _ = std::process::Command::new(&editor).arg(&card_path).status();

                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::EnterAlternateScreen,
                )?;
                crossterm::terminal::enable_raw_mode()?;

                // Force ratatui to fully repaint on the next draw()
                terminal.clear()?;

                *board = load_board(&kando_dir)?;
                if let Some((col_idx, card_idx)) = board.find_card(&card_id) {
                    board.columns[col_idx].cards[card_idx].touch();
                    board.columns[col_idx].sort_cards();
                    save_board(&kando_dir, board)?;
                    sync_message = Some("Edit card".into());
                }
                state.clamp_selection(board);
                state.notify("Card updated");
            }
        }
        Action::CyclePriority => {
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                card.priority = card.priority.next();
                card.touch();
                let priority_str = format!("Priority: {}", card.priority);
                board.columns[col_idx].sort_cards();
                save_board(&kando_dir, board)?;
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
            if was_minor_mode {
                state.mode = Mode::Normal;
            }
            let col_idx = state.focused_column;
            let card_idx = state.selected_card;
            if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
                card.blocked = !card.blocked;
                card.touch();
                let msg = if card.blocked { "Card blocked" } else { "Blocker removed" };
                board.columns[col_idx].sort_cards();
                save_board(&kando_dir, board)?;
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
                    input: current.clone(),
                    cursor: current.len(),
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

        // View toggles
        Action::ToggleHiddenColumns => {
            state.mode = Mode::Normal;
            state.show_hidden_columns = !state.show_hidden_columns;
            // If focused column is now hidden, move to nearest visible column
            if !state.show_hidden_columns {
                if let Some(col) = board.columns.get(state.focused_column) {
                    if col.hidden {
                        // Find first visible column
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
        Action::ToggleCollapseColumn => {
            state.mode = Mode::Normal;
            // TODO: implement column collapsing
            state.notify("Column collapse not yet implemented");
        }
        Action::ToggleCollapseAll => {
            state.mode = Mode::Normal;
            state.notify("Collapse all not yet implemented");
        }
        Action::ToggleWipDisplay => {
            state.mode = Mode::Normal;
            state.notify("WIP display toggle not yet implemented");
        }
        Action::CenterOnCard => {
            state.mode = Mode::Normal;
            // Scroll is handled automatically
        }

        // Filter
        Action::StartFilter => {
            state.mode = Mode::Filter {
                input: String::new(),
                cursor: 0,
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
        Action::StartSearch => {
            state.mode = Mode::Filter {
                input: String::new(),
                cursor: 0,
            };
        }

        // Input handling
        Action::InputChar(c) => {
            match &mut state.mode {
                Mode::Input { input, cursor, .. } => {
                    input.insert(*cursor, c);
                    *cursor += 1;
                }
                Mode::Filter { input, cursor } => {
                    input.insert(*cursor, c);
                    *cursor += 1;
                    // Live filter update
                    state.active_filter = Some(input.clone());
                }
                _ => {}
            }
        }
        Action::InputBackspace => {
            match &mut state.mode {
                Mode::Input { input, cursor, .. } => {
                    if *cursor > 0 {
                        input.remove(*cursor - 1);
                        *cursor -= 1;
                    }
                }
                Mode::Filter { input, cursor } => {
                    if *cursor > 0 {
                        input.remove(*cursor - 1);
                        *cursor -= 1;
                        state.active_filter = if input.is_empty() {
                            None
                        } else {
                            Some(input.clone())
                        };
                    }
                }
                _ => {}
            }
        }
        Action::InputLeft => {
            match &mut state.mode {
                Mode::Input { cursor, .. } | Mode::Filter { cursor, .. } => {
                    if *cursor > 0 {
                        *cursor -= 1;
                    }
                }
                _ => {}
            }
        }
        Action::InputRight => {
            match &mut state.mode {
                Mode::Input { input, cursor, .. } | Mode::Filter { input, cursor } => {
                    if *cursor < input.len() {
                        *cursor += 1;
                    }
                }
                _ => {}
            }
        }
        Action::InputHome => {
            match &mut state.mode {
                Mode::Input { cursor, .. } | Mode::Filter { cursor, .. } => {
                    *cursor = 0;
                }
                _ => {}
            }
        }
        Action::InputEnd => {
            match &mut state.mode {
                Mode::Input { input, cursor, .. } | Mode::Filter { input, cursor } => {
                    *cursor = input.len();
                }
                _ => {}
            }
        }
        Action::InputDeleteWord => {
            match &mut state.mode {
                Mode::Input { input, cursor, .. } => {
                    // Delete backward to previous word boundary
                    let start = input[..*cursor]
                        .trim_end()
                        .rfind(|c: char| c.is_whitespace())
                        .map(|i| i + 1)
                        .unwrap_or(0);
                    input.drain(start..*cursor);
                    *cursor = start;
                }
                Mode::Filter { input, cursor } => {
                    let start = input[..*cursor]
                        .trim_end()
                        .rfind(|c: char| c.is_whitespace())
                        .map(|i| i + 1)
                        .unwrap_or(0);
                    input.drain(start..*cursor);
                    *cursor = start;
                    state.active_filter = if input.is_empty() {
                        None
                    } else {
                        Some(input.clone())
                    };
                }
                _ => {}
            }
        }
        Action::InputConfirm => {
            match state.mode.clone() {
                Mode::Input {
                    input,
                    on_confirm: InputTarget::NewCardTitle,
                    ..
                } => {
                    let title = input.trim().to_string();
                    if !title.is_empty() {
                        let id = board.next_card_id();
                        let card = Card::new(id, title);
                        // Add to focused column
                        if let Some(col) = board.columns.get_mut(state.focused_column) {
                            col.cards.push(card);
                            col.sort_cards();
                        }
                        save_board(&kando_dir, board)?;
                        sync_message = Some("Create card".into());
                        state.notify("Card created");
                    }
                    state.mode = Mode::Normal;
                }
                Mode::Input {
                    input,
                    on_confirm: InputTarget::EditTags,
                    ..
                } => {
                    let tags: Vec<String> = input
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
                    save_board(&kando_dir, board)?;
                    sync_message = Some("Update tags".into());
                    state.clamp_selection(board);
                    state.notify("Tags updated");
                    state.mode = Mode::Normal;
                }
                Mode::Filter { input, .. } => {
                    if input.trim().is_empty() {
                        state.active_filter = None;
                    } else {
                        state.active_filter = Some(input);
                    }
                    state.mode = Mode::Normal;
                }
                Mode::Picker { items, selected, target, .. } => {
                    match target {
                        PickerTarget::TagFilter => {
                            let sel = selected;
                            if let Some((tag, _)) = items.get(sel) {
                                let tag = tag.clone();
                                if state.active_tag_filters.contains(&tag) {
                                    state.active_tag_filters.retain(|t| *t != tag);
                                } else {
                                    state.active_tag_filters.push(tag);
                                }
                            }
                            // Rebuild picker items with updated active states
                            if let Mode::Picker { items, .. } = &mut state.mode {
                                for (tag, active) in items.iter_mut() {
                                    *active = state.active_tag_filters.contains(tag);
                                }
                            }
                            // Stay in picker mode
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
                                    save_board(&kando_dir, board)?;
                                    sync_message = Some("Change priority".into());
                                    state.clamp_selection(board);
                                    state.notify(priority_str);
                                }
                            }
                            state.mode = Mode::Normal;
                        }
                        PickerTarget::MoveToColumn => {
                            if let Some((col_name, _)) = items.get(selected) {
                                // Find target column index by name (skipping current)
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
                                    save_board(&kando_dir, board)?;
                                    sync_message = Some("Move card".into());
                                    state.notify(format!("Moved to {col_name}"));
                                }
                            }
                            state.mode = Mode::Normal;
                        }
                    }
                }
                _ => {}
            }
        }
        Action::InputCancel => {
            match &state.mode {
                Mode::Filter { .. } => {
                    state.active_filter = None;
                }
                Mode::Picker { .. } => {
                    // Keep any active filters, just close the picker
                }
                _ => {}
            }
            state.mode = Mode::Normal;
        }

        // Confirmation
        Action::Confirm => {
            match &state.mode {
                Mode::Confirm {
                    on_confirm: ConfirmTarget::DeleteCard(id),
                    ..
                } => {
                    let id = id.clone();
                    if let Some((col_idx, card_idx)) = board.find_card(&id) {
                        // Remove the card file
                        let card_path = kando_dir
                            .join("columns")
                            .join(&board.columns[col_idx].slug)
                            .join(format!("{id}.md"));
                        let _ = std::fs::remove_file(card_path);
                        board.columns[col_idx].cards.remove(card_idx);
                        save_board(&kando_dir, board)?;
                        sync_message = Some("Delete card".into());
                        state.clamp_selection(board);
                        state.notify("Card deleted");
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
                    save_board(&kando_dir, board)?;
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

        // Mode entry
        Action::EnterGotoMode => {
            state.mode = Mode::Goto;
        }
        Action::EnterSpaceMode => {
            state.mode = Mode::Space;
        }
        Action::EnterViewMode => {
            state.mode = Mode::View;
        }

        // Board
        Action::ReloadBoard => {
            state.mode = Mode::Normal;
            *board = load_board(&kando_dir)?;
            state.clamp_selection(board);
            state.notify("Board reloaded");
        }
        Action::ShowHelp => {
            state.mode = Mode::Help;
        }
        Action::DismissTutorial => {
            state.mode = Mode::Normal;
            board.tutorial_shown = true;
            save_board(&kando_dir, board)?;
        }
        Action::Quit => {
            match &state.mode {
                Mode::Normal => state.should_quit = true,
                Mode::CardDetail { .. } => state.mode = Mode::Normal,
                _ => state.mode = Mode::Normal,
            }
        }
        Action::ClearFilters => {
            if state.active_filter.is_some() || !state.active_tag_filters.is_empty() {
                state.active_filter = None;
                state.active_tag_filters.clear();
                state.notify("Filters cleared");
            }
            // Otherwise no-op — Esc does nothing in Normal mode
        }
    }

    // Sync to remote if any mutation was saved
    if let Some(msg) = sync_message {
        if let Some(ref mut sync_state) = state.sync_state {
            sync::commit_and_push(sync_state, &kando_dir, &msg);
        }
    }

    Ok(())
}
