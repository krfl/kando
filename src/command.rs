//! Command mode — `:` commands for parameterized and bulk operations.
//!
//! Commands do things that keybindings can't: named arguments, bulk actions,
//! and runtime configuration changes.

use crate::app::{AppState, TextBuffer};
use crate::board::storage::{save_board, load_trash, restore_card, load_board};
use crate::board::Board;

// ---------------------------------------------------------------------------
// Command state & tab completion
// ---------------------------------------------------------------------------

/// Command metadata for the palette overlay.
pub struct CommandInfo {
    pub name: &'static str,
    pub description: &'static str,
}

/// All recognized commands with descriptions (sorted alphabetically).
/// Aliases (mv, pri, q) are intentionally omitted — they still work but
/// are not shown in the palette or offered as completions.
pub const COMMANDS: &[CommandInfo] = &[
    CommandInfo { name: "assign",   description: "Assign card" },
    CommandInfo { name: "col",      description: "Column visibility" },
    CommandInfo { name: "count",    description: "Card counts" },
    CommandInfo { name: "find",     description: "Find card" },
    CommandInfo { name: "move",     description: "Move card to column" },
    CommandInfo { name: "priority", description: "Set priority" },
    CommandInfo { name: "quit",     description: "Quit" },
    CommandInfo { name: "reload",   description: "Reload board" },
    CommandInfo { name: "rename",   description: "Rename card" },
    CommandInfo { name: "restore",  description: "Restore deleted card" },
    CommandInfo { name: "sort",     description: "Sort column" },
    CommandInfo { name: "tag",      description: "Add tag" },
    CommandInfo { name: "unassign", description: "Unassign card" },
    CommandInfo { name: "untag",    description: "Remove tag" },
    CommandInfo { name: "wip",      description: "Set WIP limit" },
];

/// All recognized command names (for completion), derived from COMMANDS.
const COMMAND_NAMES: &[&str] = &[
    "assign", "col", "count", "find", "move", "priority",
    "quit", "reload", "rename", "restore", "sort", "tag", "unassign", "untag", "wip",
];

const PRIORITY_NAMES: &[&str] = &["low", "normal", "high", "urgent"];
const SORT_FIELDS: &[&str] = &["priority", "created", "updated", "title"];
const COL_SUBCMDS: &[&str] = &["hide", "show"];

/// State for command mode: text buffer + optional completion.
#[derive(Debug, Clone)]
pub struct CommandState {
    pub buf: TextBuffer,
    pub completion: Option<CompletionState>,
}

/// Active tab-completion state.
#[derive(Debug, Clone)]
pub struct CompletionState {
    pub candidates: Vec<String>,
    pub index: usize,
    /// Byte offset in `buf.input` where the current token starts.
    pub replace_start: usize,
}

impl CommandState {
    pub fn new() -> Self {
        Self {
            buf: TextBuffer::empty(),
            completion: None,
        }
    }
}

/// Compute the ghost suffix for the current input (shown as dimmed text after cursor).
/// Returns the suffix to append, or None if no match.
pub fn compute_ghost(input: &str, board: &Board, card_tags: &[String], card_assignees: &[String], trash_ids: &[(String, String)]) -> Option<String> {
    let (token, candidates) = current_token_and_candidates(input, board, card_tags, card_assignees, trash_ids);
    if token.is_empty() {
        // After "command " (space following a known command): show first candidate.
        // But not when input is empty/blank — no ghost on bare `:` prompt.
        if input.trim_start().contains(' ') {
            return candidates.into_iter().next();
        }
        return None;
    }
    let token_lower = token.to_lowercase();
    candidates
        .into_iter()
        .find(|c| c.to_lowercase().starts_with(&token_lower) && c.to_lowercase() != token_lower)
        .map(|c| {
            // Return the suffix after what the user typed
            c[token.len()..].to_string()
        })
}

/// Compute all matching candidates for the current token.
fn compute_candidates_for_token(input: &str, board: &Board, card_tags: &[String], card_assignees: &[String], trash_ids: &[(String, String)]) -> Vec<String> {
    let (token, candidates) = current_token_and_candidates(input, board, card_tags, card_assignees, trash_ids);
    let token_lower = token.to_lowercase();
    if token.is_empty() {
        return candidates;
    }
    candidates
        .into_iter()
        .filter(|c| c.to_lowercase().starts_with(&token_lower))
        .collect()
}

/// Get the current token being typed and the candidate list for its position.
fn current_token_and_candidates(input: &str, board: &Board, card_tags: &[String], card_assignees: &[String], trash_ids: &[(String, String)]) -> (String, Vec<String>) {
    let trimmed = input.trim_start();

    // Split into tokens
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    let ends_with_space = trimmed.ends_with(' ');

    if parts.is_empty() {
        // Empty input — complete command names
        return (String::new(), COMMAND_NAMES.iter().map(|s| s.to_string()).collect());
    }

    if parts.len() == 1 && !ends_with_space {
        // Typing the command name
        return (parts[0].to_string(), COMMAND_NAMES.iter().map(|s| s.to_string()).collect());
    }

    // Command name is known; determine argument candidates
    let cmd = parts[0];
    let arg_idx = if ends_with_space { parts.len() } else { parts.len() - 1 };
    let current_token = if ends_with_space { "" } else { parts.last().unwrap_or(&"") };

    let candidates = match cmd {
        "move" | "mv" => column_names(board),
        "tag" => board_tags(board),
        "untag" => card_tags.to_vec(),
        "assign" => board_assignees(board),
        "unassign" => card_assignees.to_vec(),
        "priority" | "pri" => PRIORITY_NAMES.iter().map(|s| s.to_string()).collect(),
        "sort" => SORT_FIELDS.iter().map(|s| s.to_string()).collect(),
        "col" => {
            if arg_idx <= 1 {
                column_names(board)
            } else {
                COL_SUBCMDS.iter().map(|s| s.to_string()).collect()
            }
        }
        "wip" => {
            if arg_idx <= 1 {
                column_names(board)
            } else {
                vec![] // WIP limit is a number — no completion
            }
        }
        "restore" => trash_ids.iter().map(|(id, _)| id.clone()).collect(),
        _ => vec![], // find, rename, reload, count — no arg completion
    };

    (current_token.to_string(), candidates)
}

/// Get the byte offset where the current token starts in the input.
fn current_token_start(input: &str) -> usize {
    let trimmed_start = input.len() - input.trim_start().len();
    let trimmed = input.trim_start();

    if trimmed.ends_with(' ') || trimmed.is_empty() {
        return input.len();
    }

    // Find the start of the last whitespace-delimited token
    match trimmed.rfind(char::is_whitespace) {
        Some(pos) => {
            // Advance past the whitespace character (may be multi-byte)
            let ws_char = trimmed[pos..].chars().next().expect("rfind returned valid pos");
            trimmed_start + pos + ws_char.len_utf8()
        }
        None => trimmed_start,
    }
}

fn column_names(board: &Board) -> Vec<String> {
    board.columns.iter().map(|c| c.slug.clone()).collect()
}

fn board_tags(board: &Board) -> Vec<String> {
    board.all_tags().into_iter().map(|(tag, _)| tag).collect()
}

fn board_assignees(board: &Board) -> Vec<String> {
    board.all_assignees().into_iter().map(|(name, _)| name).collect()
}

/// Compute the palette items (name, description) for the command overlay.
///
/// Returns `(title, items)` where `title` is the palette heading (e.g. "commands",
/// "columns") and `items` is the filtered list of `(name, description)` pairs.
pub fn palette_items(input: &str, board: &Board, card_tags: &[String], card_assignees: &[String], trash_ids: &[(String, String)]) -> (&'static str, Vec<(String, String)>) {
    let trimmed = input.trim_start();
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    let ends_with_space = trimmed.ends_with(' ');

    // Phase 1: Completing the command name
    if parts.is_empty() || (parts.len() == 1 && !ends_with_space) {
        let query = parts.first().copied().unwrap_or("");
        let query_lower = query.to_lowercase();
        let items: Vec<(String, String)> = COMMANDS
            .iter()
            .filter(|c| {
                if query_lower.is_empty() {
                    true
                } else {
                    c.name.to_lowercase().contains(&query_lower)
                        || c.description.to_lowercase().contains(&query_lower)
                }
            })
            .map(|c| (c.name.to_string(), c.description.to_string()))
            .collect();
        return ("commands", items);
    }

    // Phase 2: Completing arguments
    let cmd = parts[0];
    let arg_idx = if ends_with_space { parts.len() } else { parts.len() - 1 };
    let current_token = if ends_with_space { "" } else { parts.last().unwrap_or(&"") };
    let token_lower = current_token.to_lowercase();

    let (title, raw_items): (&str, Vec<(String, String)>) = match cmd {
        "move" | "mv" => ("columns", column_names(board).into_iter().map(|n| (n, String::new())).collect()),
        "tag" => ("tags", board_tags(board).into_iter().map(|t| (t, String::new())).collect()),
        "untag" => ("tags", card_tags.iter().map(|t| (t.clone(), String::new())).collect()),
        "assign" => ("assignees", board_assignees(board).into_iter().map(|n| (n, String::new())).collect()),
        "unassign" => ("assignees", card_assignees.iter().map(|a| (a.clone(), String::new())).collect()),
        "priority" | "pri" => ("priorities", PRIORITY_NAMES.iter().map(|s| (s.to_string(), String::new())).collect()),
        "sort" => ("sort", SORT_FIELDS.iter().map(|s| (s.to_string(), String::new())).collect()),
        "col" => {
            if arg_idx <= 1 {
                ("columns", column_names(board).into_iter().map(|n| (n, String::new())).collect())
            } else {
                ("action", COL_SUBCMDS.iter().map(|s| (s.to_string(), String::new())).collect())
            }
        }
        "wip" => {
            if arg_idx <= 1 {
                ("columns", column_names(board).into_iter().map(|n| (n, String::new())).collect())
            } else {
                return ("", vec![]); // WIP limit is a number
            }
        }
        "restore" => ("trash", trash_ids.iter().map(|(id, title)| (id.clone(), title.clone())).collect()),
        _ => return ("", vec![]), // find, rename, reload, count, q, quit — no arg completion
    };

    // Filter by current token (substring match)
    let items = if token_lower.is_empty() {
        raw_items
    } else {
        raw_items
            .into_iter()
            .filter(|(name, _)| name.to_lowercase().contains(&token_lower))
            .collect()
    };

    (title, items)
}

/// Get the current token being typed (public, for palette rendering).
pub fn current_token(input: &str) -> String {
    let trimmed = input.trim_start();
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    if trimmed.ends_with(' ') || parts.is_empty() {
        String::new()
    } else {
        parts.last().unwrap_or(&"").to_string()
    }
}

/// Handle Tab/Shift-Tab: cycle through completions.
pub fn cycle_completion(cmd: &mut CommandState, board: &Board, card_tags: &[String], card_assignees: &[String], trash_ids: &[(String, String)], forward: bool) {
    if let Some(ref mut comp) = cmd.completion {
        // Already cycling — advance index
        if comp.candidates.is_empty() {
            return;
        }
        if forward {
            comp.index = (comp.index + 1) % comp.candidates.len();
        } else {
            comp.index = (comp.index + comp.candidates.len() - 1) % comp.candidates.len();
        }
        // Replace current token with the new candidate (no trailing space while cycling)
        let candidate = &comp.candidates[comp.index];
        let start = comp.replace_start;
        cmd.buf.input.truncate(start);
        cmd.buf.input.push_str(candidate);
        cmd.buf.cursor = cmd.buf.input.chars().count();
    } else {
        // First Tab press — compute candidates and accept first match
        let candidates = compute_candidates_for_token(&cmd.buf.input, board, card_tags, card_assignees, trash_ids);
        if candidates.is_empty() {
            return;
        }
        let start = current_token_start(&cmd.buf.input);
        let current = &cmd.buf.input[start..];

        // Skip the exact match so first Tab always changes the text
        let index = candidates
            .iter()
            .position(|c| c != current)
            .unwrap_or(0);

        // Apply candidate
        let candidate = &candidates[index];
        cmd.buf.input.truncate(start);
        cmd.buf.input.push_str(candidate);
        cmd.buf.cursor = cmd.buf.input.chars().count();

        if candidates.len() == 1 {
            // Only one match — auto-commit with trailing space
            cmd.buf.input.push(' ');
            cmd.buf.cursor = cmd.buf.input.chars().count();
        } else {
            cmd.completion = Some(CompletionState {
                candidates,
                index,
                replace_start: start,
            });
        }
    }
}

/// Clear completion state (called after any text edit).
pub fn clear_completion(cmd: &mut CommandState) {
    cmd.completion = None;
}

/// Parse and execute a command string. Returns an optional sync message.
pub fn execute_command(
    board: &mut Board,
    state: &mut AppState,
    input: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(None);
    }

    // Split into command name and rest-of-line arguments.
    // For :rename, everything after the command is the argument (not tokenized).
    let (cmd, rest) = match input.find(char::is_whitespace) {
        Some(pos) => (&input[..pos], input[pos..].trim_start()),
        None => (input, ""),
    };

    let result = match cmd {
        "q" | "quit" => { state.should_quit = true; Ok(None) }
        "move" | "mv" => cmd_move(board, state, rest, kando_dir),
        "tag" => cmd_tag(board, state, rest, kando_dir),
        "untag" => cmd_untag(board, state, rest, kando_dir),
        "assign" => cmd_assign(board, state, rest, kando_dir),
        "unassign" => cmd_unassign(board, state, rest, kando_dir),
        "priority" | "pri" => cmd_priority(board, state, rest, kando_dir),
        "rename" => cmd_rename(board, state, rest, kando_dir),
        "wip" => cmd_wip(board, state, rest, kando_dir),
        "sort" => cmd_sort(board, state, rest, kando_dir),
        "find" => cmd_find(board, state, rest),
        "col" => cmd_col(board, state, rest, kando_dir),
        "restore" => cmd_restore(board, state, rest, kando_dir),
        "reload" => cmd_reload(board, state, kando_dir),
        "count" => cmd_count(board, state),
        _ => {
            state.notify_error(format!("Unknown command: {cmd}"));
            Ok(None)
        }
    };

    // Mirror the keyboard handler: clear undo state on any successful card
    // mutation so that `u` doesn't silently restore across unrelated changes.
    // Commands that return Some(sync_msg) are the ones that mutate state.
    // `:restore` manages last_delete itself; `:reload` reloads from disk so
    // last_delete would be stale anyway.
    if matches!(result, Ok(Some(_))) && cmd != "restore" {
        state.last_delete = None;
        state.deleted_this_session = false;
    }

    result
}

// ---------------------------------------------------------------------------
// Column fuzzy matching
// ---------------------------------------------------------------------------

/// Find a column by fuzzy name match (case-insensitive).
/// Returns Ok(index) or Err(error message).
fn resolve_column(board: &Board, query: &str, exclude: Option<usize>) -> Result<usize, String> {
    if query.is_empty() {
        return Err("Column name required".into());
    }

    let query_lower = query.to_lowercase();

    // Exact match first (by slug or name)
    let mut matches: Vec<usize> = Vec::new();
    for (i, col) in board.columns.iter().enumerate() {
        if exclude == Some(i) {
            continue;
        }
        if col.slug.to_lowercase() == query_lower || col.name.to_lowercase() == query_lower {
            return Ok(i);
        }
    }

    // Prefix match
    for (i, col) in board.columns.iter().enumerate() {
        if exclude == Some(i) {
            continue;
        }
        if col.slug.to_lowercase().starts_with(&query_lower)
            || col.name.to_lowercase().starts_with(&query_lower)
        {
            matches.push(i);
        }
    }

    // Contains match (only if prefix didn't match)
    if matches.is_empty() {
        for (i, col) in board.columns.iter().enumerate() {
            if exclude == Some(i) {
                continue;
            }
            if col.slug.to_lowercase().contains(&query_lower)
                || col.name.to_lowercase().contains(&query_lower)
            {
                matches.push(i);
            }
        }
    }

    match matches.len() {
        0 => Err(format!("Unknown column: {query}")),
        1 => Ok(matches[0]),
        _ => {
            let names: Vec<&str> = matches.iter().map(|&i| board.columns[i].name.as_str()).collect();
            Err(format!("Ambiguous: {}", names.join(", ")))
        }
    }
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

/// :move <column> — Move selected card to column by name.
fn cmd_move(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let from = state.focused_column;
    let card_idx = state.selected_card;

    if board.columns.get(from).is_none_or(|c| c.cards.is_empty()) {
        state.notify_error("No card selected");
        return Ok(None);
    }

    match resolve_column(board, args, Some(from)) {
        Ok(to) => {
            let col_name = board.columns[to].name.clone();
            board.move_card(from, card_idx, to);
            board.columns[to].sort_cards();
            state.focused_column = to;
            state.clamp_selection(board);
            save_board(kando_dir, board)?;
            state.notify(format!("Moved to {col_name}"));
            Ok(Some("Move card".into()))
        }
        Err(e) => {
            state.notify_error(e);
            Ok(None)
        }
    }
}

/// :tag <name>[,name,...] — Add tags to selected card.
fn cmd_tag(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let names: Vec<String> = args
        .split(',')
        .map(|t| t.trim().trim_start_matches('@').to_lowercase())
        .filter(|t| !t.is_empty())
        .collect();
    if names.is_empty() {
        state.notify_error("Usage: tag <name>[,name,...]");
        return Ok(None);
    }

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        let mut added = Vec::new();
        for tag in names {
            if !card.tags.contains(&tag) {
                card.tags.push(tag.clone());
                added.push(tag);
            }
        }
        if added.is_empty() {
            state.notify("All tags already present");
            return Ok(None);
        }
        card.touch();
        board.columns[col_idx].sort_cards();
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        state.notify(format!("Tagged: {}", added.join(", ")));
        Ok(Some("Update tags".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :untag <name>[,name,...] — Remove tags from selected card.
fn cmd_untag(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let names: Vec<String> = args
        .split(',')
        .map(|t| t.trim().trim_start_matches('@').to_lowercase())
        .filter(|t| !t.is_empty())
        .collect();
    if names.is_empty() {
        state.notify_error("Usage: untag <name>[,name,...]");
        return Ok(None);
    }

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        if names.len() == 1 && names[0] == "*" {
            if card.tags.is_empty() {
                state.notify("No tags to remove");
                return Ok(None);
            }
            let all = card.tags.join(", ");
            card.tags.clear();
            card.touch();
            board.columns[col_idx].sort_cards();
            state.clamp_selection(board);
            save_board(kando_dir, board)?;
            state.notify(format!("Untagged: {all}"));
            return Ok(Some("Update tags".into()));
        }
        let mut removed = Vec::new();
        for tag in &names {
            if card.tags.contains(tag) {
                removed.push(tag.clone());
            }
        }
        if removed.is_empty() {
            state.notify_error(format!("Tags not found: {}", names.join(", ")));
            return Ok(None);
        }
        card.tags.retain(|t| !removed.contains(t));
        card.touch();
        board.columns[col_idx].sort_cards();
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        state.notify(format!("Untagged: {}", removed.join(", ")));
        Ok(Some("Update tags".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :assign <name>[,name,...] — Assign people to the selected card.
fn cmd_assign(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let names: Vec<String> = args
        .split(',')
        .map(|a| a.trim().trim_start_matches('@').to_lowercase())
        .filter(|a| !a.is_empty())
        .collect();
    if names.is_empty() {
        state.notify_error("Usage: assign <name>[,name,...]");
        return Ok(None);
    }

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        let mut added = Vec::new();
        for name in names {
            if !card.assignees.contains(&name) {
                card.assignees.push(name.clone());
                added.push(name);
            }
        }
        if added.is_empty() {
            state.notify("All already assigned");
            return Ok(None);
        }
        card.touch();
        board.columns[col_idx].sort_cards();
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        state.notify(format!("Assigned: {}", added.join(", ")));
        Ok(Some("Update assignees".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :unassign <name>[,name,...] — Remove assignees from the selected card.
fn cmd_unassign(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let names: Vec<String> = args
        .split(',')
        .map(|a| a.trim().trim_start_matches('@').to_lowercase())
        .filter(|a| !a.is_empty())
        .collect();
    if names.is_empty() {
        state.notify_error("Usage: unassign <name>[,name,...]");
        return Ok(None);
    }

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        if names.len() == 1 && names[0] == "*" {
            if card.assignees.is_empty() {
                state.notify("No assignees to remove");
                return Ok(None);
            }
            let all = card.assignees.join(", ");
            card.assignees.clear();
            card.touch();
            board.columns[col_idx].sort_cards();
            state.clamp_selection(board);
            save_board(kando_dir, board)?;
            state.notify(format!("Unassigned: {all}"));
            return Ok(Some("Update assignees".into()));
        }
        let mut removed = Vec::new();
        for name in &names {
            if card.assignees.contains(name) {
                removed.push(name.clone());
            }
        }
        if removed.is_empty() {
            state.notify_error(format!("Not assigned: {}", names.join(", ")));
            return Ok(None);
        }
        card.assignees.retain(|a| !removed.contains(a));
        card.touch();
        board.columns[col_idx].sort_cards();
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        state.notify(format!("Unassigned: {}", removed.join(", ")));
        Ok(Some("Update assignees".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :priority <level> — Set priority directly by name.
fn cmd_priority(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let level = args.trim();
    if level.is_empty() {
        state.notify_error("Usage: priority <low|normal|high|urgent>");
        return Ok(None);
    }

    let priority = match level.parse::<crate::board::Priority>() {
        Ok(p) => p,
        Err(e) => {
            state.notify_error(e);
            return Ok(None);
        }
    };

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        card.priority = priority;
        card.touch();
        let msg = format!("Priority: {priority}");
        board.columns[col_idx].sort_cards();
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        state.notify(msg);
        Ok(Some("Change priority".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :rename <new title> — Rename selected card inline.
fn cmd_rename(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let new_title = args.trim();
    if new_title.is_empty() {
        state.notify_error("Usage: rename <new title>");
        return Ok(None);
    }

    let col_idx = state.focused_column;
    let card_idx = state.selected_card;
    if let Some(card) = board.columns.get_mut(col_idx).and_then(|c| c.cards.get_mut(card_idx)) {
        card.title = new_title.to_string();
        card.touch();
        save_board(kando_dir, board)?;
        state.notify("Card renamed");
        Ok(Some("Rename card".into()))
    } else {
        state.notify_error("No card selected");
        Ok(None)
    }
}

/// :wip <column> <n> — Set WIP limit for a column. 0 disables.
/// The last whitespace-separated token is the limit; everything before it is the column name.
fn cmd_wip(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let args = args.trim();
    let last_space = args.rfind(char::is_whitespace);
    let (col_name, limit_str) = match last_space {
        Some(pos) => (args[..pos].trim(), args[pos..].trim()),
        None => {
            state.notify_error("Usage: wip <column> <limit>");
            return Ok(None);
        }
    };

    let col_idx = match resolve_column(board, col_name, None) {
        Ok(i) => i,
        Err(e) => {
            state.notify_error(e);
            return Ok(None);
        }
    };

    let limit: u32 = match limit_str.parse() {
        Ok(n) => n,
        Err(_) => {
            state.notify_error(format!("WIP limit must be a number, got: {limit_str}"));
            return Ok(None);
        }
    };

    let col = &mut board.columns[col_idx];
    col.wip_limit = if limit == 0 { None } else { Some(limit) };
    let col_name = col.name.clone();
    save_board(kando_dir, board)?;
    if limit == 0 {
        state.notify(format!("WIP limit removed: {col_name}"));
    } else {
        state.notify(format!("WIP limit: {col_name} = {limit}"));
    }
    Ok(Some("Set WIP limit".into()))
}

/// :sort [field] — Sort current column by field.
fn cmd_sort(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let field = args.trim();
    let col_idx = state.focused_column;

    if let Some(col) = board.columns.get_mut(col_idx) {
        match field {
            "" | "priority" => {
                col.sort_cards(); // default: priority then updated
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
            other => {
                state.notify_error(format!("Unknown sort field: {other} (use priority, created, updated, title)"));
                return Ok(None);
            }
        }
        state.clamp_selection(board);
        save_board(kando_dir, board)?;
        Ok(Some("Sort column".into()))
    } else {
        state.notify_error("No column focused");
        Ok(None)
    }
}

/// :find <text> — Jump to first matching card across all columns.
fn cmd_find(
    board: &Board,
    state: &mut AppState,
    args: &str,
) -> color_eyre::Result<Option<String>> {
    let query = args.trim();
    if query.is_empty() {
        state.notify_error("Usage: find <text>");
        return Ok(None);
    }

    let query_lower = query.to_lowercase();
    for (col_idx, col) in board.columns.iter().enumerate() {
        for (card_idx, card) in col.cards.iter().enumerate() {
            if card.title.to_lowercase().contains(&query_lower)
                || card.id == query
            {
                state.focused_column = col_idx;
                state.selected_card = card_idx;
                state.notify(format!("Found: {} in {}", card.title, col.name));
                return Ok(None);
            }
        }
    }

    state.notify_error(format!("No card matching: {query}"));
    Ok(None)
}

/// :col <name> [hide|show] — Jump to or toggle column visibility.
fn cmd_col(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let parts: Vec<&str> = args.split_whitespace().collect();
    if parts.is_empty() {
        state.notify_error("Usage: col <name> [hide|show]");
        return Ok(None);
    }

    let col_idx = match resolve_column(board, parts[0], None) {
        Ok(i) => i,
        Err(e) => {
            state.notify_error(e);
            return Ok(None);
        }
    };

    if parts.len() == 1 {
        // Jump to column
        state.focused_column = col_idx;
        state.clamp_selection(board);
        state.notify(format!("Jumped to {}", board.columns[col_idx].name));
        return Ok(None);
    }

    match parts[1] {
        "hide" => {
            board.columns[col_idx].hidden = true;
            let name = board.columns[col_idx].name.clone();
            save_board(kando_dir, board)?;
            state.notify(format!("Hidden: {name}"));
            Ok(Some("Hide column".into()))
        }
        "show" => {
            board.columns[col_idx].hidden = false;
            let name = board.columns[col_idx].name.clone();
            save_board(kando_dir, board)?;
            state.notify(format!("Visible: {name}"));
            Ok(Some("Show column".into()))
        }
        other => {
            state.notify_error(format!("Unknown subcommand: {other} (use hide or show)"));
            Ok(None)
        }
    }
}

/// :restore <id> — Restore a card from the trash.
fn cmd_restore(
    board: &mut Board,
    state: &mut AppState,
    args: &str,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    let card_id = args.trim();
    if card_id.is_empty() {
        state.notify_error("Usage: restore <card-id>");
        return Ok(None);
    }

    let entries = load_trash(kando_dir);
    let entry = match entries.iter().find(|e| e.id == card_id) {
        Some(e) => e,
        None => {
            state.notify_error(format!("Card {card_id} not found in trash"));
            return Ok(None);
        }
    };

    // Restore to original column if it exists, otherwise first column (backlog)
    let target_col = board.columns.iter().position(|c| c.slug == entry.from_column)
        .unwrap_or(0);
    let target_slug = board.columns[target_col].slug.clone();
    let title = entry.title.clone();

    restore_card(kando_dir, card_id, &target_slug)?;
    *board = load_board(kando_dir)?;
    if let Some((col_idx, card_idx)) = board.find_card(card_id) {
        state.focused_column = col_idx;
        state.selected_card = card_idx;
    } else {
        state.focused_column = target_col;
        state.clamp_selection(board);
    }
    // Clear undo state — if this card was the last deleted, `u` would try to
    // restore it again (file already gone) and produce a spurious error.
    if state.last_delete.as_ref().map(|e| e.id.as_str()) == Some(card_id) {
        state.last_delete = None;
    }
    state.notify(format!("Restored: {title}"));
    Ok(Some("Restore card".into()))
}

/// :reload — Reload board from disk.
fn cmd_reload(
    board: &mut Board,
    state: &mut AppState,
    kando_dir: &std::path::Path,
) -> color_eyre::Result<Option<String>> {
    *board = crate::board::storage::load_board(kando_dir)?;
    state.clamp_selection(board);
    state.notify("Board reloaded");
    Ok(None)
}

/// :count — Show card counts per column.
fn cmd_count(board: &Board, state: &mut AppState) -> color_eyre::Result<Option<String>> {
    let counts: Vec<String> = board
        .columns
        .iter()
        .map(|col| format!("{}: {}", col.name, col.cards.len()))
        .collect();
    state.notify(counts.join("  "));
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::{Board, Card, Column, Policies};

    fn test_board() -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 4,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            columns: vec![
                Column {
                    slug: "backlog".into(),
                    name: "Backlog".into(),
                    order: 0,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![
                        Card::new("001".into(), "First card".into()),
                        Card::new("002".into(), "Second card".into()),
                    ],
                },
                Column {
                    slug: "in-progress".into(),
                    name: "In Progress".into(),
                    order: 1,
                    wip_limit: Some(3),
                    hidden: false,
                    cards: vec![
                        Card::new("003".into(), "Active card".into()),
                    ],
                },
                Column {
                    slug: "done".into(),
                    name: "Done".into(),
                    order: 2,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![],
                },
            ],
        }
    }

    #[test]
    fn resolve_column_exact() {
        let board = test_board();
        assert_eq!(resolve_column(&board, "backlog", None).unwrap(), 0);
        assert_eq!(resolve_column(&board, "Backlog", None).unwrap(), 0);
        assert_eq!(resolve_column(&board, "in-progress", None).unwrap(), 1);
        assert_eq!(resolve_column(&board, "In Progress", None).unwrap(), 1);
    }

    #[test]
    fn resolve_column_prefix() {
        let board = test_board();
        assert_eq!(resolve_column(&board, "back", None).unwrap(), 0);
        assert_eq!(resolve_column(&board, "in", None).unwrap(), 1);
        assert_eq!(resolve_column(&board, "don", None).unwrap(), 2);
    }

    #[test]
    fn resolve_column_contains() {
        let board = test_board();
        assert_eq!(resolve_column(&board, "prog", None).unwrap(), 1);
    }

    #[test]
    fn resolve_column_excludes() {
        let board = test_board();
        // Excluding column 0 means "backlog" won't match
        assert!(resolve_column(&board, "backlog", Some(0)).is_err());
    }

    #[test]
    fn resolve_column_unknown() {
        let board = test_board();
        assert!(resolve_column(&board, "nonexistent", None).is_err());
    }

    #[test]
    fn resolve_column_empty() {
        let board = test_board();
        assert!(resolve_column(&board, "", None).is_err());
    }

    #[test]
    fn find_card_by_title() {
        let board = test_board();
        let mut state = AppState::new();
        cmd_find(&board, &mut state, "Active").unwrap();
        assert_eq!(state.focused_column, 1);
        assert_eq!(state.selected_card, 0);
    }

    #[test]
    fn find_card_by_id() {
        let board = test_board();
        let mut state = AppState::new();
        cmd_find(&board, &mut state, "002").unwrap();
        assert_eq!(state.focused_column, 0);
        assert_eq!(state.selected_card, 1);
    }

    #[test]
    fn find_card_not_found() {
        let board = test_board();
        let mut state = AppState::new();
        cmd_find(&board, &mut state, "nonexistent").unwrap();
        // Should show error notification
        assert!(state.notification.is_some());
        assert!(state.notification.as_ref().unwrap().contains("No card matching"));
    }

    #[test]
    fn resolve_column_ambiguous() {
        // Create a board with "Done" and "Doing" — "do" should be ambiguous
        let board = Board {
            name: "Test".into(),
            next_card_id: 1,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            columns: vec![
                Column {
                    slug: "doing".into(),
                    name: "Doing".into(),
                    order: 0,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![],
                },
                Column {
                    slug: "done".into(),
                    name: "Done".into(),
                    order: 1,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![],
                },
            ],
        };
        let err = resolve_column(&board, "do", None).unwrap_err();
        assert!(err.contains("Ambiguous"));
        assert!(err.contains("Doing"));
        assert!(err.contains("Done"));
    }

    #[test]
    fn unknown_command_notifies_error() {
        let mut board = test_board();
        let mut state = AppState::new();
        let dir = std::path::Path::new("/nonexistent");
        // This won't touch the filesystem since unknown commands return early
        let _ = execute_command(&mut board, &mut state, "foobar", dir);
        assert!(state.notification.as_ref().unwrap().contains("Unknown command: foobar"));
        assert_eq!(state.notification_level, crate::app::NotificationLevel::Error);
    }

    #[test]
    fn count_shows_all_columns() {
        let board = test_board();
        let mut state = AppState::new();
        cmd_count(&board, &mut state).unwrap();
        let notif = state.notification.unwrap();
        assert!(notif.contains("Backlog: 2"));
        assert!(notif.contains("In Progress: 1"));
        assert!(notif.contains("Done: 0"));
    }

    // -- Ghost completion tests -------------------------------------------

    #[test]
    fn ghost_command_name() {
        let board = test_board();
        let ghost = compute_ghost("mo", &board, &[], &[], &[]);
        assert_eq!(ghost, Some("ve".into()));
    }

    #[test]
    fn ghost_command_name_exact_no_ghost() {
        let board = test_board();
        // Exact match — no ghost (nothing left to complete)
        let ghost = compute_ghost("move", &board, &[], &[], &[]);
        assert_eq!(ghost, None);
    }

    #[test]
    fn ghost_column_name() {
        let board = test_board();
        let ghost = compute_ghost("move back", &board, &[], &[], &[]);
        assert_eq!(ghost, Some("log".into()));
    }

    #[test]
    fn ghost_priority() {
        let board = test_board();
        let ghost = compute_ghost("priority ur", &board, &[], &[], &[]);
        assert_eq!(ghost, Some("gent".into()));
    }

    #[test]
    fn ghost_sort_field() {
        let board = test_board();
        let ghost = compute_ghost("sort cr", &board, &[], &[], &[]);
        assert_eq!(ghost, Some("eated".into()));
    }

    #[test]
    fn ghost_no_match() {
        let board = test_board();
        let ghost = compute_ghost("xyz", &board, &[], &[], &[]);
        assert_eq!(ghost, None);
    }

    #[test]
    fn ghost_empty_input() {
        let board = test_board();
        let ghost = compute_ghost("", &board, &[], &[], &[]);
        assert_eq!(ghost, None);
    }

    #[test]
    fn ghost_after_space_shows_first_candidate() {
        let board = test_board();
        // After "move " we should see column completion
        let ghost = compute_ghost("move ", &board, &[], &[], &[]);
        // First column alphabetically: backlog
        assert_eq!(ghost, Some("backlog".into()));
    }

    #[test]
    fn cycle_completion_first_tab() {
        let board = test_board();
        let mut cmd = CommandState::new();
        cmd.buf = TextBuffer::new("mo".into());
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        // Only one match ("move") — accepts with trailing space, no cycling state
        assert_eq!(cmd.buf.input, "move ");
        assert!(cmd.completion.is_none());
    }

    #[test]
    fn cycle_completion_first_tab_multiple() {
        let board = test_board();
        let mut cmd = CommandState::new();
        // "re" matches ["reload", "rename"] — multiple candidates, first Tab picks first
        cmd.buf = TextBuffer::new("re".into());
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        assert_eq!(cmd.buf.input, "reload");
        assert!(cmd.completion.is_some());
    }

    #[test]
    fn cycle_completion_skips_exact_match() {
        let board = test_board();
        let mut cmd = CommandState::new();
        // "tag" exactly matches candidate "tag", but also matches "tag" only —
        // with aliases removed, "tag" is a single match → auto-commits with space
        // Use "un" which matches ["unassign", "untag"] for cycling test
        cmd.buf = TextBuffer::new("un".into());
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        assert_eq!(cmd.buf.input, "unassign");
        assert!(cmd.completion.is_some());
        // Tab again cycles to "untag"
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        assert_eq!(cmd.buf.input, "untag");
    }

    #[test]
    fn cycle_completion_tab_cycles() {
        let board = test_board();
        let mut cmd = CommandState::new();
        cmd.buf = TextBuffer::new("move ".into());
        // First Tab: selects first column
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        let first = cmd.buf.input.clone();
        // Second Tab: cycles to next
        cycle_completion(&mut cmd, &board, &[], &[], &[], true);
        let second = cmd.buf.input.clone();
        assert_ne!(first, second);
    }

    #[test]
    fn clear_completion_on_edit() {
        let mut cmd = CommandState::new();
        cmd.completion = Some(CompletionState {
            candidates: vec!["test".into()],
            index: 0,
            replace_start: 0,
        });
        clear_completion(&mut cmd);
        assert!(cmd.completion.is_none());
    }

    #[test]
    fn ghost_untag_uses_card_tags() {
        let board = test_board();
        let card_tags = vec!["bug".to_string(), "feature".to_string()];
        let ghost = compute_ghost("untag b", &board, &card_tags, &[], &[]);
        assert_eq!(ghost, Some("ug".into()));
    }

    #[test]
    fn ghost_col_subcmd() {
        let board = test_board();
        let ghost = compute_ghost("col backlog hi", &board, &[], &[], &[]);
        assert_eq!(ghost, Some("de".into()));
    }

    // -- Palette tests --------------------------------------------------------

    #[test]
    fn palette_empty_input_shows_all_commands() {
        let board = test_board();
        let (title, items) = palette_items("", &board, &[], &[], &[]);
        assert_eq!(title, "commands");
        assert_eq!(items.len(), COMMANDS.len());
    }

    #[test]
    fn palette_filters_commands_by_query() {
        let board = test_board();
        let (title, items) = palette_items("sor", &board, &[], &[], &[]);
        assert_eq!(title, "commands");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].0, "sort");
    }

    #[test]
    fn palette_after_command_space_shows_columns() {
        let board = test_board();
        let (title, items) = palette_items("move ", &board, &[], &[], &[]);
        assert_eq!(title, "columns");
        assert_eq!(items.len(), 3); // backlog, in-progress, done
    }

    #[test]
    fn palette_filters_argument_candidates() {
        let board = test_board();
        let (title, items) = palette_items("move back", &board, &[], &[], &[]);
        assert_eq!(title, "columns");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].0, "backlog");
    }

    #[test]
    fn palette_no_args_returns_empty() {
        let board = test_board();
        let (_, items) = palette_items("find ", &board, &[], &[], &[]);
        assert!(items.is_empty());
    }

    #[test]
    fn palette_fuzzy_matches_description() {
        let board = test_board();
        // "card" appears in descriptions of "move" and "find" and "rename"
        let (title, items) = palette_items("card", &board, &[], &[], &[]);
        assert_eq!(title, "commands");
        assert!(items.iter().any(|(n, _)| n == "find"));
        assert!(items.iter().any(|(n, _)| n == "rename"));
    }
}
