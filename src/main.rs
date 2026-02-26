mod app;
mod board;
mod command;
mod config;
mod input;
mod ui;

use std::env;
use std::path::Path;

use color_eyre::eyre::{bail, WrapErr};

use clap::{Parser, Subcommand};

use board::storage::{append_activity, find_kando_dir, init_board, load_board, save_board, trash_card, remove_column_dir, rename_column_dir};
use board::{Card, Priority, generate_slug, normalize_column_orders, slug_for_rename, slug_to_name};

#[derive(Parser)]
#[command(name = "kando", about = "A keyboard-first Kanban TUI")]
struct Cli {
    /// Use Nerd Font glyphs instead of ASCII icons
    #[arg(long, visible_alias = "nf", global = true)]
    nerd_font: bool,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Initialize a new .kando/ board in the current directory
    Init {
        /// Board name (defaults to current directory name)
        #[arg(short, long)]
        name: Option<String>,
        /// Git branch for team sync (enables git sync)
        #[arg(short, long)]
        branch: Option<String>,
    },
    /// Add a new card to the backlog
    Add {
        /// Card title
        title: String,
        /// Tags (comma-separated)
        #[arg(short, long, value_delimiter = ',')]
        tags: Vec<String>,
        /// Assignees (comma-separated)
        #[arg(short, long, value_delimiter = ',')]
        assignee: Vec<String>,
        /// Priority (low, normal, high, urgent)
        #[arg(short, long, default_value = "normal")]
        priority: Priority,
    },
    /// List all cards
    List {
        /// Filter by tag
        #[arg(short, long)]
        tag: Option<String>,
        /// Filter by column
        #[arg(short, long)]
        column: Option<String>,
    },
    /// Soft-delete (trash) a card by ID
    Delete {
        /// Card ID (e.g. 001)
        card_id: String,
    },
    /// Edit a card's fields, or open in $EDITOR when no flags are given
    Edit {
        /// Card ID (e.g. 001)
        card_id: String,
        /// New title
        #[arg(long)]
        title: Option<String>,
        /// New priority (low, normal, high, urgent)
        #[arg(long)]
        priority: Option<Priority>,
        /// Add tags (comma-separated)
        #[arg(long, value_delimiter = ',')]
        tag_add: Vec<String>,
        /// Remove tags (comma-separated)
        #[arg(long, value_delimiter = ',')]
        tag_remove: Vec<String>,
        /// Add assignees (comma-separated)
        #[arg(long, value_delimiter = ',')]
        assignee_add: Vec<String>,
        /// Remove assignees (comma-separated)
        #[arg(long, value_delimiter = ',')]
        assignee_remove: Vec<String>,
        /// Mark as blocked
        #[arg(long, conflicts_with = "unblocked")]
        blocked: bool,
        /// Clear blocked status
        #[arg(long)]
        unblocked: bool,
    },
    /// Move a card to a different column
    Move {
        /// Card ID (e.g. 001)
        card_id: String,
        /// Target column slug
        column: String,
    },
    /// List all tags with card counts
    Tags,
    /// Manually sync with remote (pull + push)
    Sync,
    /// Configure board settings
    Config {
        #[command(subcommand)]
        setting: ConfigSetting,
    },
    /// Show sync status and info
    SyncStatus,
    /// Check board setup and diagnose common issues
    Doctor,
    /// Show board metrics (throughput, cycle time, WIP)
    Metrics {
        /// Lookback window in weeks (default: full board lifetime)
        #[arg(short, long)]
        weeks: Option<u32>,
        /// Output in CSV format for piping to other tools
        #[arg(long)]
        csv: bool,
    },
    /// Manage trashed (soft-deleted) cards
    Trash {
        #[command(subcommand)]
        action: Option<TrashAction>,
    },
    /// Print the raw card file (frontmatter + body) to stdout
    Show {
        /// Card ID (e.g. 001)
        card_id: String,
    },
    /// Stream activity log to stdout (JSONL, one entry per line)
    Log,
    /// Manage archived cards
    Archive {
        #[command(subcommand)]
        action: Option<ArchiveAction>,
    },
    /// Manage board columns (add, remove, rename, reorder)
    Col {
        #[command(subcommand)]
        action: Option<ColAction>,
    },
}

#[derive(Subcommand)]
enum ArchiveAction {
    /// List all archived cards
    List,
    /// Full-text search archived cards (title + body, case-insensitive)
    Search {
        /// Search query (omit to list all archived cards)
        query: Option<String>,
    },
    /// Move a card from archive back to a column
    Restore {
        /// Card ID to restore
        card_id: String,
        /// Target column slug (default: done)
        #[arg(long, default_value = "done")]
        column: String,
    },
}

#[derive(Subcommand)]
enum TrashAction {
    /// Restore a trashed card back to its original column
    Restore {
        /// Card ID to restore
        card_id: String,
    },
    /// Permanently delete all trashed cards
    Purge,
}

#[derive(Subcommand)]
enum ColAction {
    /// List all columns with their position, slug, card count, and settings
    List,
    /// Add a new column (inserted before the first hidden column by default)
    Add {
        /// Column display name
        name: String,
        /// Insert after this column (slug or name)
        #[arg(long)]
        after: Option<String>,
    },
    /// Remove an empty column ("archive" is reserved and cannot be removed)
    Remove {
        /// Column to remove (slug or name)
        column: String,
        /// Move cards to this column before removing
        #[arg(long)]
        move_to: Option<String>,
    },
    /// Rename a column's display name (slug is unchanged)
    Rename {
        /// Column to rename (slug or name)
        column: String,
        /// New display name
        new_name: String,
    },
    /// Move a column to a new position
    Move {
        /// Column to move (slug or name)
        column: String,
        /// New position: a number (1-indexed), or left/right/first/last
        position: String,
    },
    /// Hide a column
    Hide {
        /// Column to hide (slug or name)
        column: String,
    },
    /// Unhide a hidden column
    Show {
        /// Column to show (slug or name)
        column: String,
    },
}

#[derive(Subcommand)]
enum ConfigSetting {
    /// Set WIP limit for a column
    Wip {
        /// Column slug (e.g. in-progress)
        column: String,
        /// WIP limit (0 to remove)
        limit: u32,
    },
    /// Set how many days of inactivity before a staleness indicator appears (0 to disable)
    StaleDays {
        /// Days of inactivity
        days: u32,
    },
    /// Set how many days of inactivity before a card is auto-closed (0 to disable)
    AutoCloseDays {
        /// Days of inactivity
        days: u32,
    },
    /// Set the column that auto-closed cards are moved to
    AutoCloseTarget {
        /// Column slug (e.g. archive)
        column: String,
    },
    /// Set how many days trashed cards are kept before purging (0 to keep forever)
    TrashPurgeDays {
        /// Days to keep trash
        days: u32,
    },
    /// Set how many days a completed card stays in done before being auto-archived (0 to disable)
    ArchiveAfterDays {
        /// Days in done before auto-archiving
        days: u32,
    },
    /// Enable or disable Nerd Font icons
    NerdFont {
        /// on or off
        #[arg(value_parser = ["on", "off"])]
        value: String,
    },
    /// Show all current board settings
    Show,
}

fn main() {
    // Install color_eyre for unexpected panics/errors (developer bugs).
    let _ = color_eyre::install();
    let cli = Cli::parse();
    let cwd = match env::current_dir() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error: cannot determine current directory: {e}");
            std::process::exit(1);
        }
    };

    let result = match cli.command {
        Some(Command::Init { name, branch }) => {
            let name = name.unwrap_or_else(|| {
                cwd.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("My Project")
                    .to_string()
            });
            cmd_init(&cwd, &name, branch)
        }
        Some(Command::Add {
            title,
            tags,
            assignee,
            priority,
        }) => cmd_add(&cwd, &title, tags, assignee, priority),
        Some(Command::List { tag, column }) => cmd_list(&cwd, tag.as_deref(), column.as_deref(), cli.nerd_font),
        Some(Command::Delete { card_id }) => cmd_delete(&cwd, &card_id),
        Some(Command::Edit {
            card_id,
            title,
            priority,
            tag_add,
            tag_remove,
            assignee_add,
            assignee_remove,
            blocked,
            unblocked,
        }) => cmd_edit(
            &cwd,
            &card_id,
            title.as_deref(),
            priority,
            tag_add,
            tag_remove,
            assignee_add,
            assignee_remove,
            blocked,
            unblocked,
        ),
        Some(Command::Move { card_id, column }) => cmd_move(&cwd, &card_id, &column),
        Some(Command::Tags) => cmd_tags(&cwd),
        Some(Command::Sync) => cmd_sync(&cwd),
        Some(Command::Config { setting }) => match setting {
            ConfigSetting::Wip { column, limit } => cmd_config_wip(&cwd, &column, limit),
            ConfigSetting::StaleDays { days } => cmd_config_stale_days(&cwd, days),
            ConfigSetting::AutoCloseDays { days } => cmd_config_auto_close_days(&cwd, days),
            ConfigSetting::AutoCloseTarget { column } => cmd_config_auto_close_target(&cwd, &column),
            ConfigSetting::TrashPurgeDays { days } => cmd_config_trash_purge_days(&cwd, days),
            ConfigSetting::ArchiveAfterDays { days } => cmd_config_archive_after_days(&cwd, days),
            ConfigSetting::NerdFont { value } => cmd_config_nerd_font(&cwd, &value),
            ConfigSetting::Show => cmd_config_show(&cwd),
        },
        Some(Command::SyncStatus) => cmd_sync_status(&cwd),
        Some(Command::Doctor) => cmd_doctor(&cwd),
        Some(Command::Metrics { weeks, csv }) => cmd_metrics(&cwd, weeks, csv),
        Some(Command::Trash { action }) => cmd_trash(&cwd, action),
        Some(Command::Show { card_id }) => cmd_show(&cwd, &card_id),
        Some(Command::Log) => cmd_log(&cwd),
        Some(Command::Archive { action }) => cmd_archive(&cwd, action),
        Some(Command::Col { action }) => cmd_col_cli(&cwd, action),
        None => cmd_tui(&cwd, cli.nerd_font),
    };

    if let Err(e) = result {
        print_user_error(&e);
        std::process::exit(1);
    }
}

/// Print a user-friendly error message, with actionable hints for known error types.
fn print_user_error(error: &color_eyre::Report) {
    // Walk the error chain looking for known types.
    if let Some(storage_err) = error.downcast_ref::<board::storage::StorageError>() {
        match storage_err {
            board::storage::StorageError::NotFound(_) => {
                eprintln!("error: no kando board found in this directory.");
                eprintln!("  Run `kando init` to create one.");
            }
            board::storage::StorageError::InvalidCard { path, reason } => {
                eprintln!(
                    "error: invalid card file: {}",
                    path.file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or(&path.to_string_lossy())
                );
                eprintln!("  {reason}");
            }
            board::storage::StorageError::InvalidSlug(slug) => {
                eprintln!("error: invalid column slug: {slug:?}");
                eprintln!("  Column slugs must match [a-z0-9][a-z0-9-]*");
            }
            board::storage::StorageError::TomlDe(e) => {
                eprintln!("error: config file has invalid TOML syntax.");
                eprintln!("  {e}");
                eprintln!("  Run `kando doctor` to diagnose.");
            }
            board::storage::StorageError::TomlSer(e) => {
                eprintln!("error: failed to save board config.");
                eprintln!("  {e}");
            }
            board::storage::StorageError::Io(e) => {
                eprintln!("error: could not read or write board files.");
                eprintln!("  {e}");
            }
        }
        return;
    }

    if let Some(sync_err) = error.downcast_ref::<board::sync::SyncError>() {
        match sync_err {
            board::sync::SyncError::NotGitRepo => {
                eprintln!("error: not a git repository.");
                eprintln!("  Run `git init` first, then `kando init --branch <name>`.");
            }
            board::sync::SyncError::NoRemote => {
                eprintln!("error: no git remote 'origin' configured.");
                eprintln!("  Run `git remote add origin <url>` to set one up.");
            }
            board::sync::SyncError::GitFailed(msg) => {
                eprintln!("error: git command failed.");
                eprintln!("  {msg}");
                eprintln!("  Run `kando doctor` to diagnose.");
            }
            board::sync::SyncError::Io(e) => {
                eprintln!("error: sync I/O failure.");
                eprintln!("  {e}");
            }
        }
        return;
    }

    // For eyre::eyre!() / bail!() messages, print the full error chain.
    // These are already human-readable strings like "Card '003' not found".
    eprintln!("error: {e:#}", e = error);
}

fn cmd_init(cwd: &Path, name: &str, branch: Option<String>) -> color_eyre::Result<()> {
    // If board already exists, only allow --branch to enable/update sync
    if cwd.join(".kando").exists() {
        if let Some(ref branch) = branch {
            let kando_dir = find_kando_dir(cwd)?;
            let sync_branch = ensure_git_repo(cwd, branch)?;
            if let Some(branch) = sync_branch {
                let mut board = load_board(&kando_dir)?;
                board.sync_branch = Some(branch.to_string());
                save_board(&kando_dir, &board)?;
                println!("Git sync enabled on branch: {branch}");
            }
            return Ok(());
        }
        bail!("Board already exists in this directory. To enable sync, run: kando init --branch <branch>");
    }

    // If --branch is provided, ensure we're in a git repo (or create one).
    let sync_branch = if let Some(ref branch) = branch {
        ensure_git_repo(cwd, branch)?
    } else {
        None
    };

    let kando_dir = init_board(cwd, name, sync_branch)?;
    println!(
        "Initialized Kando board '{}' in {}",
        name,
        kando_dir.display()
    );
    println!("\nCreated columns: Backlog, In Progress, Done, Archive");
    if let Some(branch) = sync_branch {
        println!("Git sync enabled on branch: {branch}");
    }
    println!("Run `kando` to open the board, or `kando add \"Card title\"` to add cards.");
    Ok(())
}

/// Ensure cwd is inside a git repo; prompt to create one if not.
/// Returns Some(branch) if sync should be enabled, None if user declined.
fn ensure_git_repo<'a>(cwd: &Path, branch: &'a str) -> color_eyre::Result<Option<&'a str>> {
    use board::sync::find_git_root;
    use std::io::{self, Write};
    use std::process::Command;

    if find_git_root(cwd).is_some() {
        return Ok(Some(branch));
    }

    print!("This directory is not a git repository. Initialize one? (y/n) ");
    io::stdout().flush()?;
    let mut answer = String::new();
    io::stdin().read_line(&mut answer)?;
    if answer.trim().eq_ignore_ascii_case("y") {
        let output = Command::new("git")
            .arg("init")
            .current_dir(cwd)
            .output()?;
        if !output.status.success() {
            bail!(
                "Failed to initialize git repository: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        println!("Initialized git repository.");
        Ok(Some(branch))
    } else {
        println!("Skipping git sync.");
        Ok(None)
    }
}

fn cmd_add(
    cwd: &Path,
    title: &str,
    tags: Vec<String>,
    assignees: Vec<String>,
    priority: Priority,
) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let id = board.next_card_id();
    let mut card = Card::new(id.clone(), title.to_string());
    card.priority = priority;
    card.tags = tags
        .into_iter()
        .map(|t| t.trim().to_lowercase())
        .filter(|t| !t.is_empty())
        .collect();
    card.assignees = assignees
        .into_iter()
        .map(|a| a.trim().to_lowercase())
        .filter(|a| !a.is_empty())
        .collect();

    // Add to first column (backlog)
    if let Some(col) = board.columns.first_mut() {
        col.cards.push(card);
    }

    save_board(&kando_dir, &board)?;
    println!("Created {id}: {title}");
    Ok(())
}

fn cmd_list(cwd: &Path, tag: Option<&str>, column: Option<&str>, nerd_font_flag: bool) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;
    let nerd_font = nerd_font_flag || board.nerd_font;
    let icons = ui::theme::icons(nerd_font);
    let now = chrono::Utc::now();

    for col in &board.columns {
        if let Some(filter_col) = column {
            if col.slug != filter_col {
                continue;
            }
        }

        let cards: Vec<&Card> = col
            .cards
            .iter()
            .filter(|card| {
                if let Some(filter_tag) = tag {
                    card.tags.iter().any(|t| t == filter_tag)
                } else {
                    true
                }
            })
            .collect();

        if cards.is_empty() && column.is_none() {
            continue;
        }

        println!("\n{} ({})", col.name, cards.len());
        println!("{}", "─".repeat(40));
        for card in &cards {
            let age = board::age::format_age(card.created, now);
            let tags = if card.tags.is_empty() {
                String::new()
            } else {
                format!(" [{}]", card.tags.join(", "))
            };
            let priority = icons.priority(card.priority).unwrap_or("");
            let blocked = if card.blocked { " [blocked]" } else { "" };
            println!(
                "  {} {:>5}  {}{}  {}{}",
                card.id, age, card.title, tags, priority, blocked
            );
        }
    }
    println!();
    Ok(())
}

fn cmd_move(cwd: &Path, card_id: &str, target: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let target_col = board
        .columns
        .iter()
        .position(|c| c.slug == target)
        .ok_or_else(|| color_eyre::eyre::eyre!("Column '{}' not found", target))?;

    let (from_col, card_idx) = board
        .find_card(card_id)
        .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found", card_id))?;

    if from_col == target_col {
        println!("Card is already in '{}'", target);
        return Ok(());
    }

    board.move_card(from_col, card_idx, target_col);
    save_board(&kando_dir, &board)?;
    println!("Moved {} to {}", card_id, board.columns[target_col].name);
    Ok(())
}

fn cmd_tags(cwd: &Path) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;
    let tags = board.all_tags();

    if tags.is_empty() {
        println!("No tags on the board.");
        return Ok(());
    }

    println!("\nTags:");
    println!("{}", "─".repeat(30));
    for (tag, count) in &tags {
        println!(
            "  {tag:<20} {count} card{}",
            if *count == 1 { "" } else { "s" }
        );
    }
    println!();
    Ok(())
}

fn cmd_sync(cwd: &Path) -> color_eyre::Result<()> {
    use board::sync;
    use std::io::{self, Write};

    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let branch = match board.sync_branch.as_deref() {
        Some(b) => b.to_string(),
        None => {
            print!("No sync branch configured. Enter a branch name to enable sync (or press Enter to cancel): ");
            io::stdout().flush()?;
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            let branch = input.trim().to_string();
            if branch.is_empty() {
                println!("Sync cancelled.");
                return Ok(());
            }
            let sync_branch = match ensure_git_repo(cwd, &branch)? {
                Some(b) => b.to_string(),
                None => return Ok(()),
            };
            board.sync_branch = Some(sync_branch.clone());
            save_board(&kando_dir, &board)?;
            println!("Git sync enabled on branch: {sync_branch}");
            sync_branch
        }
    };

    let mut sync_state = sync::init_shadow(&kando_dir, &branch)
        .wrap_err("Failed to initialize sync")?;

    // Pull
    println!("Pulling from remote...");
    let status = sync::pull(&mut sync_state, &kando_dir);
    match status {
        sync::SyncStatus::Updated => println!("Pulled changes from remote."),
        sync::SyncStatus::AlreadyUpToDate => println!("Already up to date."),
        sync::SyncStatus::Offline => println!("Could not reach remote (offline)."),
    }

    // Push
    println!("Pushing to remote...");
    sync::commit_and_push(&mut sync_state, &kando_dir, "Manual sync");
    if sync_state.online {
        println!("Pushed to remote.");
    } else {
        println!("Push failed (offline). Changes saved locally.");
    }

    Ok(())
}

fn cmd_config_wip(cwd: &Path, column: &str, limit: u32) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let col = board
        .columns
        .iter_mut()
        .find(|c| c.slug == column)
        .ok_or_else(|| color_eyre::eyre::eyre!("Column '{}' not found", column))?;

    if limit == 0 {
        col.wip_limit = None;
        println!("Removed WIP limit from '{}'", col.name);
    } else {
        col.wip_limit = Some(limit);
        println!("Set WIP limit for '{}' to {}", col.name, limit);
    }

    save_board(&kando_dir, &board)?;
    Ok(())
}

fn cmd_delete(cwd: &Path, card_id: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let (col_idx, card_idx) = board
        .find_card(card_id)
        .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found", card_id))?;

    let col_slug = board.columns[col_idx].slug.clone();
    let col_name = board.columns[col_idx].name.clone();
    let card_title = board.columns[col_idx].cards[card_idx].title.clone();

    // Trash the file BEFORE removing from board (save_board deletes orphaned files)
    trash_card(&kando_dir, &col_slug, card_id, &card_title)?;
    board.columns[col_idx].cards.remove(card_idx);
    save_board(&kando_dir, &board)?;
    append_activity(&kando_dir, "delete", card_id, &card_title, &[("column", &col_name)]);

    println!("Deleted {card_id} ({card_title}) from {col_name}");
    println!("Restore with: kando trash restore {card_id}");
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn cmd_edit(
    cwd: &Path,
    card_id: &str,
    title: Option<&str>,
    priority: Option<Priority>,
    tag_add: Vec<String>,
    tag_remove: Vec<String>,
    assignee_add: Vec<String>,
    assignee_remove: Vec<String>,
    blocked: bool,
    unblocked: bool,
) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let has_flags = title.is_some()
        || priority.is_some()
        || !tag_add.is_empty()
        || !tag_remove.is_empty()
        || !assignee_add.is_empty()
        || !assignee_remove.is_empty()
        || blocked
        || unblocked;

    let (col_idx, card_idx) = board
        .find_card(card_id)
        .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found", card_id))?;

    if has_flags {
        let card = &mut board.columns[col_idx].cards[card_idx];

        if let Some(t) = title {
            card.title = t.to_string();
        }
        if let Some(p) = priority {
            card.priority = p;
        }
        for tag in tag_add {
            let tag = tag.trim().to_lowercase();
            if !tag.is_empty() && !card.tags.contains(&tag) {
                card.tags.push(tag);
            }
        }
        for tag in &tag_remove {
            let tag = tag.trim().to_lowercase();
            card.tags.retain(|t| t != &tag);
        }
        for assignee in assignee_add {
            let a = assignee.trim().to_lowercase();
            if !a.is_empty() && !card.assignees.contains(&a) {
                card.assignees.push(a);
            }
        }
        for assignee in &assignee_remove {
            let assignee = assignee.trim().to_lowercase();
            card.assignees.retain(|a| a != &assignee);
        }
        if blocked {
            card.blocked = true;
        }
        if unblocked {
            card.blocked = false;
        }
        card.touch();

        let card_title = card.title.clone();
        let col_name = board.columns[col_idx].name.clone();
        board.columns[col_idx].sort_cards();
        save_board(&kando_dir, &board)?;
        append_activity(&kando_dir, "edit", card_id, &card_title, &[("column", &col_name)]);
        println!("Updated {card_id}: {card_title}");
    } else {
        // No flags — open in $EDITOR
        let col_slug = board.columns[col_idx].slug.clone();
        let card_path = kando_dir
            .join("columns")
            .join(&col_slug)
            .join(format!("{card_id}.md"));

        let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vi".to_string());
        let editor = editor.trim().to_string();
        if editor.is_empty() {
            color_eyre::eyre::bail!("$EDITOR is not set");
        }

        // Split $EDITOR on whitespace so "code --wait" works, and exec directly
        // (no shell interpolation) to avoid injection via crafted $EDITOR values.
        let mut editor_parts = editor.split_whitespace();
        let editor_bin = editor_parts.next().unwrap(); // checked non-empty above
        let status = std::process::Command::new(editor_bin)
            .args(editor_parts)
            .arg(&card_path)
            .status()
            .wrap_err("Failed to launch editor")?;

        // Reload to pick up changes made in editor, then touch and save
        let mut board = load_board(&kando_dir)?;
        if let Some((ci, ki)) = board.find_card(card_id) {
            let card_title = board.columns[ci].cards[ki].title.clone();
            let col_name = board.columns[ci].name.clone();
            board.columns[ci].cards[ki].touch();
            board.columns[ci].sort_cards();
            save_board(&kando_dir, &board)?;
            append_activity(&kando_dir, "edit", card_id, &card_title, &[("column", &col_name)]);

            if status.success() {
                println!("Updated {card_id}: {card_title}");
            } else {
                eprintln!("warning: editor exited with {status}");
            }
        } else {
            eprintln!("warning: card {card_id} not found after editing; changes may not have been saved");
        }
    }

    Ok(())
}

fn cmd_config_stale_days(cwd: &Path, days: u32) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board.policies.stale_days = days;
    save_board(&kando_dir, &board)?;
    if days == 0 {
        println!("Staleness indicators disabled.");
    } else {
        println!("Staleness threshold set to {days} days.");
    }
    Ok(())
}

fn cmd_config_auto_close_days(cwd: &Path, days: u32) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board.policies.auto_close_days = days;
    save_board(&kando_dir, &board)?;
    if days == 0 {
        println!("Auto-close disabled.");
    } else {
        println!("Auto-close threshold set to {days} days.");
    }
    Ok(())
}

fn cmd_config_auto_close_target(cwd: &Path, column: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board
        .columns
        .iter()
        .find(|c| c.slug == column)
        .ok_or_else(|| color_eyre::eyre::eyre!("Column '{}' not found", column))?;
    board.policies.auto_close_target = column.to_string();
    save_board(&kando_dir, &board)?;
    println!("Auto-close target set to '{column}'.");
    Ok(())
}

fn cmd_config_trash_purge_days(cwd: &Path, days: u32) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board.policies.trash_purge_days = days;
    save_board(&kando_dir, &board)?;
    if days == 0 {
        println!("Trash auto-purge disabled (trashed cards kept forever).");
    } else {
        println!("Trash auto-purge set to {days} days.");
    }
    Ok(())
}

fn cmd_config_archive_after_days(cwd: &Path, days: u32) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board.policies.archive_after_days = days;
    save_board(&kando_dir, &board)?;
    if days == 0 {
        println!("Auto-archive disabled.");
    } else {
        println!("Auto-archive set to {days} days after completion.");
    }
    Ok(())
}

fn cmd_config_nerd_font(cwd: &Path, value: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    board.nerd_font = value == "on";
    save_board(&kando_dir, &board)?;
    println!("Nerd Font icons {}.", if board.nerd_font { "enabled" } else { "disabled" });
    Ok(())
}

fn cmd_config_show(cwd: &Path) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;
    let p = &board.policies;

    println!("\nBoard Settings");
    println!("{}", "─".repeat(40));
    println!("  Board name:          {}", board.name);
    println!("  Nerd Font icons:     {}", if board.nerd_font { "on" } else { "off" });
    println!();
    println!("Policies");
    println!("{}", "─".repeat(40));
    if p.stale_days == 0 {
        println!("  stale-days:          disabled");
    } else {
        println!("  stale-days:          {} days", p.stale_days);
    }
    if p.auto_close_days == 0 {
        println!("  auto-close-days:     disabled");
        println!("  auto-close-target:   (n/a — auto-close disabled)");
    } else {
        println!("  auto-close-days:     {} days", p.auto_close_days);
        println!("  auto-close-target:   {}", p.auto_close_target);
    }
    if p.trash_purge_days == 0 {
        println!("  trash-purge-days:    disabled (keep forever)");
    } else {
        println!("  trash-purge-days:    {} days", p.trash_purge_days);
    }
    if p.archive_after_days == 0 {
        println!("  archive-after-days:  disabled");
    } else {
        println!("  archive-after-days:  {} days", p.archive_after_days);
    }
    println!();
    println!("WIP Limits");
    println!("{}", "─".repeat(40));
    let any_wip = board.columns.iter().any(|c| c.wip_limit.is_some());
    if any_wip {
        for col in &board.columns {
            if let Some(limit) = col.wip_limit {
                println!("  {:<20} {}", col.name, limit);
            }
        }
    } else {
        println!("  (none configured)");
    }
    println!();
    Ok(())
}

fn cmd_sync_status(cwd: &Path) -> color_eyre::Result<()> {
    use board::sync;

    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;

    println!("\nSync Status");
    println!("{}", "─".repeat(40));

    match board.sync_branch.as_deref() {
        Some(branch) => {
            println!("Branch:   {branch}");

            let git_root = sync::find_git_root(cwd);
            if let Some(ref root) = git_root {
                match sync::get_remote_url(root) {
                    Ok(url) => {
                        println!("Remote:   {url}");
                        let shadow = sync::shadow_dir_for(&url);
                        if shadow.is_dir() {
                            println!("Shadow:   {}", shadow.display());
                            // Show last commit time
                            let output = std::process::Command::new("git")
                                .args(["log", "-1", "--format=%ci", &format!("origin/{branch}")])
                                .current_dir(&shadow)
                                .output();
                            if let Ok(out) = output {
                                let last = String::from_utf8_lossy(&out.stdout);
                                let last = last.trim();
                                if !last.is_empty() {
                                    println!("Last sync: {last}");
                                }
                            }
                            // Show pending local changes
                            let output = std::process::Command::new("git")
                                .args(["status", "--porcelain"])
                                .current_dir(&shadow)
                                .output();
                            if let Ok(out) = output {
                                let changes = String::from_utf8_lossy(&out.stdout);
                                let count = changes.lines().filter(|l| !l.is_empty()).count();
                                if count > 0 {
                                    println!("Pending:  {count} uncommitted changes");
                                } else {
                                    println!("Pending:  no uncommitted changes");
                                }
                            }
                        } else {
                            println!("Shadow:   not initialized (run kando sync)");
                        }
                    }
                    Err(_) => println!("Remote:   not configured"),
                }
            } else {
                println!("Git:      not a git repository");
            }
        }
        None => {
            println!("Sync not configured.");
            println!("Run: kando init --branch <name> to enable");
        }
    }
    println!();
    Ok(())
}

fn cmd_doctor(cwd: &Path) -> color_eyre::Result<()> {
    use board::sync;

    let mut errors = 0u32;

    println!("\nkando doctor\n");
    println!("Board");

    // 1. Board exists
    let kando_dir = match cwd.join(".kando") {
        dir if dir.is_dir() => {
            println!("  \u{2713} Board found at {}", dir.display());
            dir
        }
        _ => {
            bail!("Board not found in {}. Run: kando init", cwd.display());
        }
    };

    // 2. Board loadable
    let board = match load_board(&kando_dir) {
        Ok(board) => {
            println!("  \u{2713} Board config valid (name: \"{}\")", board.name);
            board
        }
        Err(e) => {
            println!("  \u{2717} Board config invalid: {e}");
            println!("    \u{2192} Check .kando/config.toml for syntax errors");
            errors += 1;
            println!();
            if errors > 0 {
                println!("{errors} issue(s) found.");
            }
            return Ok(());
        }
    };

    // 3. Columns valid
    let col_count = board.columns.len();
    let mut col_errors = 0;
    for col in &board.columns {
        let col_dir = kando_dir.join("columns").join(&col.slug);
        if !col_dir.is_dir() {
            col_errors += 1;
        }
    }
    if col_errors == 0 {
        println!("  \u{2713} {col_count} columns OK");
    } else {
        println!("  \u{2717} {col_errors}/{col_count} column directories missing");
        errors += 1;
    }

    // 4. Card count
    let total_cards: usize = board.columns.iter().map(|c| c.cards.len()).sum();
    println!("  \u{2713} {total_cards} cards loaded");

    // 5. Git sync checks
    if let Some(ref branch) = board.sync_branch {
        println!("\nGit Sync (branch: {branch})");

        // Git repo
        let git_root = sync::find_git_root(cwd);
        match &git_root {
            Some(root) => println!("  \u{2713} Git repository found at {}", root.display()),
            None => {
                println!("  \u{2717} Not a git repository");
                println!("    \u{2192} Run: git init");
                errors += 1;
            }
        }

        // Remote URL
        if let Some(ref root) = git_root {
            match sync::get_remote_url(root) {
                Ok(url) => {
                    println!("  \u{2713} Remote: {url}");

                    // SSH agent
                    if sync::check_ssh_agent(&url) {
                        println!("  \u{2717} SSH agent has no loaded keys");
                        println!("    \u{2192} Run: eval $(ssh-agent -s) && ssh-add");
                        errors += 1;
                    } else if url.contains('@') {
                        println!("  \u{2713} SSH agent has loaded keys");
                    }

                    // Shadow clone
                    let shadow = sync::shadow_dir_for(&url);
                    if shadow.is_dir() {
                        println!("  \u{2713} Shadow clone exists at {}", shadow.display());
                    } else {
                        println!("  \u{2717} Shadow clone not found");
                        println!("    \u{2192} Run: kando sync");
                        errors += 1;
                    }

                    // Remote reachable
                    let ls_remote = std::process::Command::new("git")
                        .args(["ls-remote", "--exit-code", "--quiet", &url])
                        .current_dir(root)
                        .stdout(std::process::Stdio::null())
                        .stderr(std::process::Stdio::null())
                        .status();
                    match ls_remote {
                        Ok(status) if status.success() => {
                            println!("  \u{2713} Remote reachable");
                        }
                        _ => {
                            println!("  \u{2717} Remote not reachable");
                            println!("    \u{2192} Check your network connection and SSH config");
                            errors += 1;
                        }
                    }
                }
                Err(_) => {
                    println!("  \u{2717} No remote 'origin' configured");
                    println!("    \u{2192} Run: git remote add origin <url>");
                    errors += 1;
                }
            }
        }
    } else {
        println!("\nGit Sync");
        println!("  \u{2013} Not configured (run kando init --branch <name> to enable)");
    }

    println!();
    if errors == 0 {
        println!("All checks passed!");
    } else {
        println!(
            "{errors} issue{} found.",
            if errors == 1 { "" } else { "s" }
        );
    }
    println!();
    Ok(())
}

fn cmd_trash(cwd: &Path, action: Option<TrashAction>) -> color_eyre::Result<()> {
    use board::storage::{load_trash, restore_card};

    let kando_dir = find_kando_dir(cwd)?;

    match action {
        None => {
            // List trashed cards
            let entries = load_trash(&kando_dir);
            if entries.is_empty() {
                println!("Trash is empty.");
                return Ok(());
            }

            println!("\nTrash ({} card{}):", entries.len(), if entries.len() == 1 { "" } else { "s" });
            println!("{}", "─".repeat(60));
            for entry in &entries {
                println!(
                    "  {:<6} {:<30} from {}  ({})",
                    entry.id, entry.title, entry.from_column, entry.deleted
                );
            }
            println!("\nRestore with: kando trash restore <card-id>");
            println!();
        }
        Some(TrashAction::Restore { card_id }) => {
            let entries = load_trash(&kando_dir);
            let entry = entries.iter().find(|e| e.id == card_id)
                .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found in trash", card_id))?;

            let board = load_board(&kando_dir)?;
            let target_col = board.columns.iter().position(|c| c.slug == entry.from_column)
                .unwrap_or(0);
            let target_slug = &board.columns[target_col].slug;
            let title = entry.title.clone();
            let col_name = board.columns[target_col].name.clone();

            restore_card(&kando_dir, &card_id, target_slug)?;
            println!("Restored {} ({}) to {}", card_id, title, col_name);
        }
        Some(TrashAction::Purge) => {
            let entries = load_trash(&kando_dir);
            if entries.is_empty() {
                println!("Trash is already empty.");
                return Ok(());
            }
            let count = entries.len();
            let trash_dir = kando_dir.join(".trash");
            for entry in &entries {
                let card_file = trash_dir.join(format!("{}.md", entry.id));
                let _ = std::fs::remove_file(card_file);
            }
            // Clear the meta file
            let _ = std::fs::remove_file(trash_dir.join("_meta.toml"));
            println!("Permanently deleted {count} card{} from trash.", if count == 1 { "" } else { "s" });
        }
    }

    Ok(())
}

fn cmd_metrics(cwd: &Path, weeks: Option<u32>, csv: bool) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;
    let since = weeks.map(|w| chrono::Utc::now() - chrono::TimeDelta::weeks(w as i64));
    let metrics = board::metrics::compute_metrics(&board, since);
    if csv {
        print!("{}", board::metrics::format_csv(&metrics));
    } else {
        print!("{}", board::metrics::format_text(&metrics));
    }
    Ok(())
}

fn cmd_show(cwd: &Path, card_id: &str) -> color_eyre::Result<()> {
    use std::io::{BufWriter, ErrorKind, Write};

    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;

    let (col_idx, _) = board
        .find_card(card_id)
        .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found", card_id))?;

    let col_slug = &board.columns[col_idx].slug;
    let card_path = kando_dir
        .join("columns")
        .join(col_slug)
        .join(format!("{card_id}.md"));

    let content = std::fs::read_to_string(&card_path)
        .wrap_err_with(|| format!("failed to read card file: {}", card_path.display()))?;

    let stdout = std::io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    match write!(out, "{content}") {
        Ok(()) => {}
        Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
        Err(e) => return Err(e).wrap_err("error writing to stdout"),
    }
    match out.flush() {
        Ok(()) => {}
        Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
        Err(e) => return Err(e).wrap_err("error flushing stdout"),
    }
    Ok(())
}

fn cmd_archive(cwd: &Path, action: Option<ArchiveAction>) -> color_eyre::Result<()> {
    match action.unwrap_or(ArchiveAction::List) {
        ArchiveAction::List => cmd_archive_list(cwd),
        ArchiveAction::Search { query } => cmd_archive_search(cwd, query.as_deref().unwrap_or("")),
        ArchiveAction::Restore { card_id, column } => cmd_archive_restore(cwd, &card_id, &column),
    }
}

fn cmd_archive_list(cwd: &Path) -> color_eyre::Result<()> {
    use std::io::{BufWriter, ErrorKind, Write};
    use chrono::Utc;

    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;

    let archive_col = board.columns.iter()
        .find(|c| c.slug == "archive")
        .ok_or_else(|| color_eyre::eyre::eyre!("No 'archive' column found"))?;

    if archive_col.cards.is_empty() {
        println!("Archive is empty.");
        return Ok(());
    }

    let now = Utc::now();
    let stdout = std::io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    match writeln!(out, "Archive ({} cards):", archive_col.cards.len()) {
        Ok(()) => {}
        Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
        Err(e) => return Err(e).wrap_err("error writing to stdout"),
    }
    for card in &archive_col.cards {
        let age_start = card.completed.unwrap_or(card.updated);
        let age_days = (now - age_start).num_days().max(0);
        let date_str = age_start.format("%Y-%m-%d").to_string();
        match writeln!(
            out,
            "  {:<6}  {:<40}  completed {}  ({} days ago)",
            card.id,
            if card.title.chars().count() > 40 { format!("{}…", card.title.chars().take(39).collect::<String>()) } else { card.title.clone() },
            date_str,
            age_days,
        ) {
            Ok(()) => {}
            Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
            Err(e) => return Err(e).wrap_err("error writing to stdout"),
        }
    }
    match out.flush() {
        Ok(()) => {}
        Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
        Err(e) => return Err(e).wrap_err("error flushing stdout"),
    }
    Ok(())
}

fn cmd_archive_search(cwd: &Path, query: &str) -> color_eyre::Result<()> {
    use std::io::{BufWriter, ErrorKind, Write};
    use chrono::Utc;

    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;

    let archive_col = board.columns.iter()
        .find(|c| c.slug == "archive")
        .ok_or_else(|| color_eyre::eyre::eyre!("No 'archive' column found"))?;

    let query_lower = query.to_lowercase();
    let matches: Vec<&Card> = archive_col.cards.iter()
        .filter(|card| {
            query_lower.is_empty()
                || card.title.to_lowercase().contains(&query_lower)
                || card.body.to_lowercase().contains(&query_lower)
        })
        .collect();

    if matches.is_empty() {
        if query_lower.is_empty() {
            println!("Archive is empty.");
        } else {
            println!("No archived cards match '{query}'.");
        }
        return Ok(());
    }

    let now = Utc::now();
    let stdout = std::io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    for card in matches {
        let age_start = card.completed.unwrap_or(card.updated);
        let age_days = (now - age_start).num_days().max(0);
        let date_str = age_start.format("%Y-%m-%d").to_string();
        match writeln!(
            out,
            "  {:<6}  {:<40}  completed {}  ({} days ago)",
            card.id,
            if card.title.chars().count() > 40 { format!("{}…", card.title.chars().take(39).collect::<String>()) } else { card.title.clone() },
            date_str,
            age_days,
        ) {
            Ok(()) => {}
            Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
            Err(e) => return Err(e).wrap_err("error writing to stdout"),
        }
    }
    match out.flush() {
        Ok(()) => {}
        Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
        Err(e) => return Err(e).wrap_err("error flushing stdout"),
    }
    Ok(())
}

fn cmd_archive_restore(cwd: &Path, card_id: &str, column: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let archive_idx = board.columns.iter().position(|c| c.slug == "archive")
        .ok_or_else(|| color_eyre::eyre::eyre!("No 'archive' column found"))?;

    let card_idx = board.columns[archive_idx].cards.iter()
        .position(|c| c.id == card_id)
        .ok_or_else(|| color_eyre::eyre::eyre!("Card '{}' not found in archive", card_id))?;

    let target_idx = board.columns.iter().position(|c| c.slug == column)
        .ok_or_else(|| {
            let valid: Vec<&str> = board.columns.iter().map(|c| c.slug.as_str()).collect();
            color_eyre::eyre::eyre!(
                "Column '{}' not found. Valid columns: {}",
                column,
                valid.join(", ")
            )
        })?;

    if target_idx == archive_idx {
        bail!("Cannot restore a card to the archive column itself");
    }

    // Direct move to preserve completed/started timestamps (bypasses move_card()
    // which would clear card.completed when moving out of "done").
    let card = board.columns[archive_idx].cards.remove(card_idx);
    let title = card.title.clone();
    let target_name = board.columns[target_idx].name.clone();
    board.columns[target_idx].cards.push(card);
    board.columns[archive_idx].sort_cards();
    board.columns[target_idx].sort_cards();

    save_board(&kando_dir, &board)?;
    append_activity(&kando_dir, "unarchive", card_id, &title, &[("to", &target_name)]);

    println!("Restored {card_id} ({title}) to {target_name}");
    Ok(())
}

// ---------------------------------------------------------------------------
// Column management CLI
// ---------------------------------------------------------------------------

/// Resolve a column by exact slug or case-insensitive name.  Returns the index.
fn resolve_col_cli(board: &board::Board, query: &str) -> color_eyre::Result<usize> {
    let lower = query.to_lowercase();
    board
        .columns
        .iter()
        .position(|c| c.slug == query || c.name.to_lowercase() == lower)
        .ok_or_else(|| {
            let valid: Vec<String> = board
                .columns
                .iter()
                .map(|c| format!("{} ({})", c.name, c.slug))
                .collect();
            color_eyre::eyre::eyre!(
                "Column '{}' not found. Valid columns: {}",
                query,
                valid.join(", ")
            )
        })
}

fn cmd_col_cli(cwd: &Path, action: Option<ColAction>) -> color_eyre::Result<()> {
    match action.unwrap_or(ColAction::List) {
        ColAction::List => cmd_col_list(cwd),
        ColAction::Add { name, after } => cmd_col_add(cwd, &name, after.as_deref()),
        ColAction::Remove { column, move_to } => cmd_col_remove(cwd, &column, move_to.as_deref()),
        ColAction::Rename { column, new_name } => cmd_col_rename(cwd, &column, &new_name),
        ColAction::Move { column, position } => cmd_col_move(cwd, &column, &position),
        ColAction::Hide { column } => cmd_col_hide_cli(cwd, &column),
        ColAction::Show { column } => cmd_col_show_cli(cwd, &column),
    }
}

fn cmd_col_list(cwd: &Path) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let board = load_board(&kando_dir)?;

    println!("\nColumns ({} total):", board.columns.len());
    println!("{}", "─".repeat(62));
    println!(
        "  {:<3}  {:<22}  {:<16}  {:>5}  {:>4}",
        "#", "Name", "Slug", "Cards", "WIP"
    );
    println!("{}", "─".repeat(62));
    for (i, col) in board.columns.iter().enumerate() {
        let wip = col.wip_limit.map_or("-".to_string(), |n| n.to_string());
        let hidden_tag = if col.hidden { "  [hidden]" } else { "" };
        // Slugs are ASCII-only (enforced by validate_slug), so byte-indexing is safe.
        let slug_display = if col.slug.len() > 16 {
            format!("{}…", &col.slug[..15])
        } else {
            col.slug.clone()
        };
        println!(
            "  {:<3}  {:<22}  {:<16}  {:>5}  {:>4}{}",
            i + 1,
            if col.name.chars().count() > 22 {
                format!("{}…", col.name.chars().take(21).collect::<String>())
            } else {
                col.name.clone()
            },
            slug_display,
            col.cards.len(),
            wip,
            hidden_tag,
        );
    }
    println!();
    Ok(())
}

fn cmd_col_add(cwd: &Path, name: &str, after: Option<&str>) -> color_eyre::Result<()> {
    let name = name.trim();
    if name.is_empty() {
        color_eyre::eyre::bail!("Column name cannot be empty");
    }

    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let slug = generate_slug(name, &board.columns);

    let insert_order = if let Some(after_query) = after {
        let after_idx = resolve_col_cli(&board, after_query)?;
        board.columns[after_idx].order + 1
    } else {
        // Default: before the first hidden column (or at the end).
        board
            .columns
            .iter()
            .filter(|c| c.hidden)
            .map(|c| c.order)
            .min()
            .unwrap_or(u32::try_from(board.columns.len()).unwrap_or(u32::MAX))
    };

    // Shift existing columns at or after the insert position.
    for col in board.columns.iter_mut() {
        if col.order >= insert_order {
            col.order += 1;
        }
    }
    // Derive display name from slug so it matches what a load_board round-trip
    // would produce, rather than storing the raw user-supplied string.
    let derived_name = slug_to_name(&slug);
    board.columns.push(board::Column {
        slug: slug.clone(),
        name: derived_name.clone(),
        order: insert_order,
        wip_limit: None,
        hidden: false,
        cards: vec![],
    });
    normalize_column_orders(&mut board.columns);

    save_board(&kando_dir, &board)?;
    append_activity(&kando_dir, "col-add", &slug, &derived_name, &[]);

    println!("Created column '{}' (slug: {})", derived_name, slug);
    Ok(())
}

fn cmd_col_remove(cwd: &Path, column: &str, move_to: Option<&str>) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let col_idx = resolve_col_cli(&board, column)?;
    let slug = board.columns[col_idx].slug.clone();
    let name = board.columns[col_idx].name.clone();

    if slug == "archive" {
        color_eyre::eyre::bail!("The 'archive' column is reserved and cannot be removed");
    }
    if board.columns.len() == 1 {
        color_eyre::eyre::bail!("Cannot remove the last column");
    }

    let card_count = board.columns[col_idx].cards.len();
    if card_count > 0 {
        if let Some(target_query) = move_to {
            // Move all cards to the target column before removal.
            let target_idx = resolve_col_cli(&board, target_query)?;
            if target_idx == col_idx {
                color_eyre::eyre::bail!("Cannot move cards to the same column being removed");
            }
            // Extract all cards, then push to target. We intentionally skip
            // card.touch() here: the user is destroying a column, not editing
            // cards; preserving the original updated timestamp is correct.
            let cards: Vec<_> = board.columns[col_idx].cards.drain(..).collect();
            let moved = cards.len();
            board.columns[target_idx].cards.extend(cards);
            board.columns[target_idx].sort_cards();
            println!("Moved {moved} card(s) to '{}'", board.columns[target_idx].name);
        } else {
            color_eyre::eyre::bail!(
                "Column '{}' has {card_count} card(s). Move them first, or use --move-to <column>",
                name
            );
        }
    }

    board.columns.remove(col_idx);
    normalize_column_orders(&mut board.columns);

    // save_board must run before remove_column_dir: save writes config.toml with
    // the column removed, then remove_column_dir deletes the slug directory.
    // If remove_column_dir is never reached (e.g. disk full after save), the
    // orphaned directory is benign — load_board ignores dirs not in config.toml.
    save_board(&kando_dir, &board)?;
    remove_column_dir(&kando_dir, &slug)?;
    append_activity(&kando_dir, "col-remove", &slug, &name, &[]);

    println!("Removed column '{name}'");
    Ok(())
}

fn cmd_col_rename(cwd: &Path, column: &str, new_name: &str) -> color_eyre::Result<()> {
    let new_name = new_name.trim();
    if new_name.is_empty() {
        bail!("New column name cannot be empty");
    }

    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let col_idx = resolve_col_cli(&board, column)?;

    // slug_for_rename centralises the "archive" reservation check.
    let new_slug = slug_for_rename(new_name)
        .map_err(|e| color_eyre::eyre::eyre!("{}", e))?;

    let old_slug = board.columns[col_idx].slug.clone();
    let old_name = board.columns[col_idx].name.clone();

    if new_slug == old_slug {
        println!("'{}' is already at that slug (unchanged)", old_name);
        return Ok(());
    }

    // Conflict check: another column already has this slug.
    if board.columns.iter().enumerate().any(|(i, c)| i != col_idx && c.slug == new_slug) {
        bail!("A column with slug '{}' already exists", new_slug);
    }

    // Update board state in memory.
    let derived_name = slug_to_name(&new_slug);
    board.columns[col_idx].slug = new_slug.clone();
    board.columns[col_idx].name = derived_name.clone();
    if board.policies.auto_close_target == old_slug {
        board.policies.auto_close_target = new_slug.clone();
    }

    // Rename the directory first so save_board writes card files into the
    // correct location. On save failure we attempt a best-effort rollback of
    // the directory rename to keep the filesystem consistent.
    rename_column_dir(&kando_dir, &old_slug, &new_slug)
        .wrap_err("Could not rename column directory")?;
    save_board(&kando_dir, &board).inspect_err(|_| {
        let _ = rename_column_dir(&kando_dir, &new_slug, &old_slug);
    })?;

    append_activity(&kando_dir, "col-rename", &new_slug, &derived_name, &[("from", &old_slug)]);

    println!("Renamed '{}' → '{}' (slug: {} → {})", old_name, derived_name, old_slug, new_slug);
    Ok(())
}

fn cmd_col_move(cwd: &Path, column: &str, position: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;

    let col_idx = resolve_col_cli(&board, column)?;
    let slug = board.columns[col_idx].slug.clone();
    let name = board.columns[col_idx].name.clone();
    let len = board.columns.len();

    let target_idx = match position {
        "left" => {
            if col_idx == 0 {
                color_eyre::eyre::bail!("'{}' is already the leftmost column", name);
            }
            col_idx - 1
        }
        "right" => {
            if col_idx + 1 >= len {
                color_eyre::eyre::bail!("'{}' is already the rightmost column", name);
            }
            col_idx + 1
        }
        "first" => 0,
        // resolve_col_cli succeeds above, so len >= 1; subtraction is safe.
        "last" => len.saturating_sub(1),
        n => match n.parse::<usize>() {
            Ok(pos) if pos >= 1 && pos <= len => pos - 1,
            _ => color_eyre::eyre::bail!(
                "Invalid position: '{}'. Use left, right, first, last, or 1–{}",
                n,
                len
            ),
        },
    };

    if target_idx == col_idx {
        println!("'{}' is already at that position", name);
        return Ok(());
    }

    let col = board.columns.remove(col_idx);
    board.columns.insert(target_idx, col);
    // Assign orders based on new Vec positions. Do NOT call normalize_column_orders
    // here: that function sorts by the existing order fields first, which would
    // undo the remove+insert reordering we just performed.
    for (i, c) in board.columns.iter_mut().enumerate() {
        c.order = i as u32;
    }

    let new_pos = target_idx + 1;
    save_board(&kando_dir, &board)?;
    append_activity(
        &kando_dir,
        "col-move",
        &slug,
        &name,
        &[("to", &new_pos.to_string())],
    );

    println!("Moved '{}' to position {}", name, new_pos);
    Ok(())
}

fn cmd_col_hide_cli(cwd: &Path, column: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    let col_idx = resolve_col_cli(&board, column)?;
    let slug = board.columns[col_idx].slug.clone();
    let name = board.columns[col_idx].name.clone();
    board.columns[col_idx].hidden = true;
    save_board(&kando_dir, &board)?;
    append_activity(&kando_dir, "col-hide", &slug, &name, &[]);
    println!("Hidden column '{name}'");
    Ok(())
}

fn cmd_col_show_cli(cwd: &Path, column: &str) -> color_eyre::Result<()> {
    let kando_dir = find_kando_dir(cwd)?;
    let mut board = load_board(&kando_dir)?;
    let col_idx = resolve_col_cli(&board, column)?;
    let slug = board.columns[col_idx].slug.clone();
    let name = board.columns[col_idx].name.clone();
    board.columns[col_idx].hidden = false;
    save_board(&kando_dir, &board)?;
    append_activity(&kando_dir, "col-show", &slug, &name, &[]);
    println!("Showed column '{name}'");
    Ok(())
}

fn cmd_log(cwd: &Path) -> color_eyre::Result<()> {
    use std::io::{self, BufRead, BufWriter, ErrorKind, Write};

    let kando_dir = find_kando_dir(cwd)?;
    let log_path = kando_dir.join("activity.log");

    let file = match std::fs::File::open(&log_path) {
        Ok(f) => f,
        Err(e) if e.kind() == ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e).wrap_err("failed to open activity.log"),
    };

    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    for line in io::BufReader::new(file).lines() {
        let line = line.wrap_err("error reading activity.log")?;
        match writeln!(out, "{line}") {
            Ok(()) => {}
            Err(e) if e.kind() == ErrorKind::BrokenPipe => return Ok(()),
            Err(e) => return Err(e).wrap_err("error writing to stdout"),
        }
    }
    // Flush explicitly — BufWriter silently drops flush errors on drop.
    // Treat BrokenPipe as a clean exit (consumer closed the pipe).
    if let Err(e) = out.flush() {
        if e.kind() != ErrorKind::BrokenPipe {
            return Err(e).wrap_err("error flushing stdout");
        }
    }
    Ok(())
}

fn cmd_tui(cwd: &Path, nerd_font_flag: bool) -> color_eyre::Result<()> {
    let mut terminal = ratatui::init();
    let result = app::run(&mut terminal, cwd, nerd_font_flag);
    ratatui::restore();
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    fn make_kando_dir(parent: &Path) -> PathBuf {
        let kando_dir = parent.join(".kando");
        fs::create_dir(&kando_dir).unwrap();
        kando_dir
    }

    #[test]
    fn cmd_log_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        let result = cmd_log(dir.path());
        assert!(result.is_err());
    }

    #[test]
    fn cmd_log_no_activity_log_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        make_kando_dir(dir.path());
        assert!(cmd_log(dir.path()).is_ok());
    }

    #[test]
    fn cmd_log_empty_activity_log_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        fs::write(kando_dir.join("activity.log"), "").unwrap();
        assert!(cmd_log(dir.path()).is_ok());
    }

    #[test]
    fn cmd_log_valid_jsonl_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        fs::write(
            kando_dir.join("activity.log"),
            "{\"action\":\"create\",\"id\":\"1\"}\n{\"action\":\"move\",\"id\":\"1\"}\n",
        ).unwrap();
        assert!(cmd_log(dir.path()).is_ok());
    }

    #[test]
    fn cmd_log_no_trailing_newline_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        // BufReader::lines() must still yield the final line without a \n
        fs::write(kando_dir.join("activity.log"), "{\"action\":\"create\"}").unwrap();
        assert!(cmd_log(dir.path()).is_ok());
    }

    #[test]
    fn cmd_log_non_utf8_content_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        fs::write(kando_dir.join("activity.log"), b"\xFF\xFE{}\n").unwrap();
        let err = cmd_log(dir.path()).unwrap_err();
        assert!(
            format!("{err:#}").contains("error reading activity.log"),
            "unexpected error: {err:#}"
        );
    }

    #[test]
    fn cmd_log_nested_cwd_finds_kando_in_ancestor() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        // Write a real log so we confirm the file was reached via the ancestor walk
        fs::write(kando_dir.join("activity.log"), "{\"action\":\"create\"}\n").unwrap();
        let nested = dir.path().join("sub").join("sub2");
        fs::create_dir_all(&nested).unwrap();
        assert!(cmd_log(&nested).is_ok());
    }

    #[test]
    #[cfg(unix)]
    fn cmd_log_unreadable_file_returns_err() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = make_kando_dir(dir.path());
        let log_path = kando_dir.join("activity.log");
        fs::write(&log_path, "{\"action\":\"test\"}\n").unwrap();
        fs::set_permissions(&log_path, fs::Permissions::from_mode(0o000)).unwrap();
        // Skip when running as root (permission bits are ignored)
        if fs::File::open(&log_path).is_ok() {
            eprintln!("skipping cmd_log_unreadable_file_returns_err: running as root");
            return;
        }
        let err = cmd_log(dir.path()).unwrap_err();
        assert!(
            format!("{err:#}").contains("failed to open activity.log"),
            "unexpected error: {err:#}"
        );
    }

    // ── cmd_show tests ──

    #[test]
    fn cmd_show_prints_card_file_contents() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        let mut card = Card::new(id.clone(), "My pipe card".into());
        card.body = "Some description for piping.".into();
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        // The file should exist and cmd_show should not error
        assert!(cmd_show(dir.path(), &id).is_ok());

        // Verify the on-disk file has the expected structure
        let col_slug = &board.columns[0].slug;
        let content = fs::read_to_string(
            kando_dir.join("columns").join(col_slug).join(format!("{id}.md"))
        ).unwrap();
        assert!(content.contains("---"), "file should have frontmatter delimiters");
        assert!(content.contains("My pipe card"), "file should contain card title");
        assert!(content.contains("Some description for piping."), "file should contain body");
    }

    #[test]
    fn cmd_show_card_not_found_returns_err() {
        use board::storage::init_board;
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let result = cmd_show(dir.path(), "nonexistent");
        assert!(result.is_err());
    }

    #[test]
    fn cmd_show_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        let result = cmd_show(dir.path(), "001");
        assert!(result.is_err());
    }

    #[test]
    fn cmd_show_card_in_non_first_column() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        // Locate the "In Progress" column by slug to avoid coupling to index order.
        let col_idx = board.columns.iter().position(|c| c.slug == "in-progress")
            .expect("in-progress column not found");
        let id = board.next_card_id();
        board.columns[col_idx].cards.push(Card::new(id.clone(), "In Progress card".into()));
        save_board(&kando_dir, &board).unwrap();

        let result = cmd_show(dir.path(), &id);
        assert!(result.is_ok(), "cmd_show should find a card in a non-first column");
    }

    #[test]
    fn cmd_show_file_missing_from_disk() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Ghost card".into()));
        save_board(&kando_dir, &board).unwrap();

        // Delete the .md file from disk while the card remains in the board
        let col_slug = board.columns[0].slug.clone();
        let card_path = kando_dir.join("columns").join(&col_slug).join(format!("{id}.md"));
        fs::remove_file(&card_path).unwrap();

        let result = cmd_show(dir.path(), &id);
        assert!(result.is_err(), "cmd_show should error when .md file is missing from disk");
    }

    #[test]
    fn cmd_show_card_with_empty_body() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        // Card::new already initializes body to String::new(); use it directly.
        board.columns[0].cards.push(Card::new(id.clone(), "No body".into()));
        save_board(&kando_dir, &board).unwrap();

        // Verify the serialized file has frontmatter delimiters even with no body.
        let col_slug = board.columns[0].slug.clone();
        let on_disk = fs::read_to_string(
            kando_dir.join("columns").join(&col_slug).join(format!("{id}.md"))
        ).unwrap();
        assert!(on_disk.contains("---"), "file must have frontmatter delimiters even with empty body");
        assert!(on_disk.contains("No body"), "file must contain the card title");

        let result = cmd_show(dir.path(), &id);
        assert!(result.is_ok(), "cmd_show should succeed for a card with an empty body");
    }

    #[test]
    fn cmd_show_card_with_unicode_and_multiline_body() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        let mut card = Card::new(id.clone(), "Unicode card".into());
        card.body = "Line 1\nLine 2\nEmoji: 🚀🦀\nCJK: 日本語\nArabic: مرحبا".into();
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        // Verify the file content is correct before calling cmd_show
        let col_slug = board.columns[0].slug.clone();
        let on_disk = fs::read_to_string(
            kando_dir.join("columns").join(&col_slug).join(format!("{id}.md"))
        ).unwrap();
        assert!(on_disk.contains("🚀🦀"), "file should contain emoji");
        assert!(on_disk.contains("日本語"), "file should contain CJK characters");
        assert!(on_disk.contains("مرحبا"), "file should contain Arabic RTL text");

        let result = cmd_show(dir.path(), &id);
        assert!(result.is_ok(), "cmd_show should succeed for a card with unicode and multiline body");
    }

    #[test]
    fn cmd_show_disambiguates_prefix_matches() {
        // Verify exact-match semantics: a query that is a strict prefix of an
        // existing card ID (but not itself a card ID) must return an error.
        // This catches any hypothetical "prefix expansion" or starts_with matching.
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();

        // Create only card "10" — there is no card with ID "1".
        board.next_card_id = 10;
        let id_ten = board.next_card_id(); // "10"
        board.columns[0].cards.push(Card::new(id_ten.clone(), "Card ten".into()));
        save_board(&kando_dir, &board).unwrap();

        // Exact match must succeed.
        assert!(cmd_show(dir.path(), &id_ten).is_ok(), "card '10' should be found by exact ID");

        // "1" is a strict prefix of "10" but is not a card ID — must return an error,
        // not silently return card "10"'s content.
        assert!(
            cmd_show(dir.path(), "1").is_err(),
            "prefix '1' of card '10' must not match; exact ID matching only"
        );
    }

    #[test]
    fn cmd_show_card_id_with_leading_trailing_whitespace_returns_err() {
        // cmd_show does not trim the card_id — whitespace-padded IDs should not
        // match valid card IDs, preventing silent ID mismatches.
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id(); // "1"
        board.columns[0].cards.push(Card::new(id.clone(), "Card".into()));
        save_board(&kando_dir, &board).unwrap();

        // Padded IDs must return a not-found error, not an I/O or parse error.
        let leading_err = cmd_show(dir.path(), &format!(" {id}"))
            .expect_err("leading-space ID should not match any card");
        assert!(
            format!("{leading_err:#}").contains("not found"),
            "expected a not-found error, got: {leading_err:#}"
        );

        let trailing_err = cmd_show(dir.path(), &format!("{id} "))
            .expect_err("trailing-space ID should not match any card");
        assert!(
            format!("{trailing_err:#}").contains("not found"),
            "expected a not-found error, got: {trailing_err:#}"
        );
    }

    // Note: cmd_show_output_matches_file_contents_exactly is not implemented as a
    // unit test because cmd_show writes directly to the process stdout (locked).
    // Capturing stdout in a unit test requires either a writer parameter or running
    // cmd_show as a subprocess. The existing cmd_show_prints_card_file_contents
    // test validates that the on-disk file contains the expected title and body;
    // since cmd_show calls `write!(out, "{content}")` with a `String` (whose
    // Display impl is a verbatim pass-through), the bytes that reach stdout are
    // identical to the file contents. End-to-end coverage is provided by
    // cmd_show_card_in_non_first_column and cmd_show_card_with_unicode_and_multiline_body.

    // ── Test helpers ──

    fn setup_board_with_card(parent: &Path) -> (std::path::PathBuf, String) {
        use board::storage::{init_board, save_board};
        init_board(parent, "Test", None).unwrap();
        let kando_dir = parent.join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        let mut card = Card::new(id.clone(), "Test card".into());
        card.tags = vec!["bug".into()];
        card.assignees = vec!["alice".into()];
        board.columns[0].cards.push(card);
        save_board(&kando_dir, &board).unwrap();
        (kando_dir, id)
    }

    // ── cmd_delete tests ──

    #[test]
    fn cmd_delete_happy_path_card_removed_from_board() {
        let dir = tempfile::tempdir().unwrap();
        let (_, id) = setup_board_with_card(dir.path());
        cmd_delete(dir.path(), &id).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert!(board.find_card(&id).is_none(), "card should be gone from board after delete");
    }

    #[test]
    fn cmd_delete_card_moved_to_trash_not_deleted() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_delete(dir.path(), &id).unwrap();
        let trash_file = kando_dir.join(".trash").join(format!("{id}.md"));
        assert!(trash_file.exists(), "card file should be in trash, not permanently deleted");
        // Also verify the trash metadata entry was recorded
        let entries = board::storage::load_trash(&kando_dir);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].id, id);
    }

    #[test]
    fn cmd_delete_activity_log_written() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_delete(dir.path(), &id).unwrap();
        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        let lines: Vec<&str> = log.lines().collect();
        assert_eq!(lines.len(), 1, "expected exactly one activity log entry");
        let line = lines[0];
        assert!(line.contains("\"action\":\"delete\""), "activity log should record delete action");
        assert!(line.contains(&format!("\"id\":\"{id}\"")), "activity log should record card id");
    }

    #[test]
    fn cmd_delete_card_not_found_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_card(dir.path());
        let result = cmd_delete(dir.path(), "nonexistent");
        assert!(result.is_err(), "deleting a non-existent card should return Err");
    }

    // ── cmd_edit tests ──

    #[test]
    fn cmd_edit_title_change() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_edit(dir.path(), &id, Some("New Title"), None, vec![], vec![], vec![], vec![], false, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert_eq!(board.columns[ci].cards[ki].title, "New Title");
    }

    #[test]
    fn cmd_edit_priority_change() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_edit(dir.path(), &id, None, Some(Priority::High), vec![], vec![], vec![], vec![], false, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert_eq!(board.columns[ci].cards[ki].priority, Priority::High);
    }

    #[test]
    fn cmd_edit_tag_add_normalizes_to_lowercase() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_edit(dir.path(), &id, None, None, vec!["URGENT".into()], vec![], vec![], vec![], false, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert!(board.columns[ci].cards[ki].tags.contains(&"urgent".to_string()), "tag should be stored as lowercase");
    }

    #[test]
    fn cmd_edit_tag_remove_normalizes_to_lowercase() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path()); // card has "bug" tag
        cmd_edit(dir.path(), &id, None, None, vec![], vec!["BUG".into()], vec![], vec![], false, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert!(!board.columns[ci].cards[ki].tags.contains(&"bug".to_string()), "tag should be removed despite mixed case input");
    }

    #[test]
    fn cmd_edit_add_existing_tag_is_idempotent() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path()); // card has "bug" tag
        cmd_edit(dir.path(), &id, None, None, vec!["bug".into()], vec![], vec![], vec![], false, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        let count = board.columns[ci].cards[ki].tags.iter().filter(|t| t.as_str() == "bug").count();
        assert_eq!(count, 1, "adding an existing tag should not duplicate it");
    }

    #[test]
    fn cmd_edit_remove_nonexistent_tag_succeeds() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        let result = cmd_edit(dir.path(), &id, None, None, vec![], vec!["nonexistent".into()], vec![], vec![], false, false);
        assert!(result.is_ok(), "removing a non-existent tag should not error");
        // Existing tags must not be disturbed
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert_eq!(board.columns[ci].cards[ki].tags, vec!["bug".to_string()],
            "existing tags should be unmodified after removing a non-existent tag");
    }

    #[test]
    fn cmd_edit_blocked_sets_flag() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_edit(dir.path(), &id, None, None, vec![], vec![], vec![], vec![], true, false).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert!(board.columns[ci].cards[ki].blocked, "blocked flag should be set");
    }

    #[test]
    fn cmd_edit_unblocked_clears_flag() {
        use board::storage::save_board;
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        // Pre-set blocked = true
        let mut board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        board.columns[ci].cards[ki].blocked = true;
        save_board(&kando_dir, &board).unwrap();

        cmd_edit(dir.path(), &id, None, None, vec![], vec![], vec![], vec![], false, true).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert!(!board.columns[ci].cards[ki].blocked, "blocked flag should be cleared by --unblocked");
    }

    #[test]
    fn cmd_edit_blocked_and_unblocked_both_true_unblocked_wins() {
        // At the CLI level, clap's conflicts_with prevents this.
        // At the function level, unblocked runs after blocked so it wins.
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_card(dir.path());
        cmd_edit(dir.path(), &id, None, None, vec![], vec![], vec![], vec![], true, true).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let (ci, ki) = board.find_card(&id).unwrap();
        assert!(!board.columns[ci].cards[ki].blocked, "when both blocked and unblocked are true, unblocked wins");
    }

    #[test]
    fn cmd_edit_card_not_found_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_card(dir.path());
        let result = cmd_edit(dir.path(), "nonexistent", Some("Title"), None, vec![], vec![], vec![], vec![], false, false);
        assert!(result.is_err(), "editing a non-existent card should return Err");
    }

    // ── cmd_config_* tests ──

    #[test]
    fn cmd_config_stale_days_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_stale_days(dir.path(), 21).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.stale_days, 21);
    }

    #[test]
    fn cmd_config_auto_close_days_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_auto_close_days(dir.path(), 14).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.auto_close_days, 14);
    }

    #[test]
    fn cmd_config_auto_close_target_valid_column_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        // Use a slug from the actual board rather than hardcoding "archive"
        let board = load_board(&kando_dir).unwrap();
        let slug = board.columns.last().unwrap().slug.clone();
        cmd_config_auto_close_target(dir.path(), &slug).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.auto_close_target, slug);
    }

    #[test]
    fn cmd_config_auto_close_target_invalid_column_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let result = cmd_config_auto_close_target(dir.path(), "nonexistent-column");
        assert!(result.is_err(), "setting auto-close target to a non-existent column should error");
    }

    #[test]
    fn cmd_config_trash_purge_days_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_trash_purge_days(dir.path(), 7).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.trash_purge_days, 7);
    }

    #[test]
    fn cmd_config_nerd_font_on_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_nerd_font(dir.path(), "on").unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert!(board.nerd_font, "nerd_font should be true after 'on'");
    }

    #[test]
    fn cmd_config_nerd_font_off_persists() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        cmd_config_nerd_font(dir.path(), "on").unwrap();
        // Verify "on" actually worked before testing "off"
        assert!(load_board(&kando_dir).unwrap().nerd_font, "precondition: nerd_font should be true after 'on'");
        cmd_config_nerd_font(dir.path(), "off").unwrap();
        assert!(!load_board(&kando_dir).unwrap().nerd_font, "nerd_font should be false after 'off'");
    }

    #[test]
    fn cmd_config_show_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        assert!(cmd_config_show(dir.path()).is_ok(), "cmd_config_show should not error");
    }

    // ── cmd_config_archive_after_days tests ──

    #[test]
    fn cmd_config_archive_after_days_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_config_archive_after_days(dir.path(), 7).is_err());
    }

    #[test]
    fn cmd_config_archive_after_days_persists_value() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_archive_after_days(dir.path(), 14).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.archive_after_days, 14);
    }

    #[test]
    fn cmd_config_archive_after_days_zero_disables() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        // First set a non-zero value, then set 0 to disable
        cmd_config_archive_after_days(dir.path(), 7).unwrap();
        cmd_config_archive_after_days(dir.path(), 0).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.archive_after_days, 0);
    }

    #[test]
    fn cmd_config_archive_after_days_overwrite_works() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        cmd_config_archive_after_days(dir.path(), 7).unwrap();
        cmd_config_archive_after_days(dir.path(), 30).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.archive_after_days, 30);
    }

    #[test]
    fn cmd_config_archive_after_days_default_is_zero() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.archive_after_days, 0, "archive_after_days must default to 0 (disabled)");
    }

    // ── cmd_archive dispatch test ──

    #[test]
    fn cmd_archive_no_action_defaults_to_list() {
        // When no action is given, cmd_archive defaults to list.
        // An empty archive board should return Ok.
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        assert!(cmd_archive(dir.path(), None).is_ok());
    }

    // ── cmd_archive_list tests ──

    /// Helper: create a board and add a card to the archive column.
    fn setup_board_with_archived_card(parent: &Path) -> (PathBuf, String) {
        use board::storage::{init_board, save_board};
        init_board(parent, "Test", None).unwrap();
        let kando_dir = parent.join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let id = board.next_card_id();
        let card = Card::new(id.clone(), "Archived task".into());
        board.columns[archive_idx].cards.push(card);
        save_board(&kando_dir, &board).unwrap();
        (kando_dir, id)
    }

    #[test]
    fn cmd_archive_list_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_archive_list(dir.path()).is_err());
    }

    #[test]
    fn cmd_archive_list_empty_archive_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        assert!(cmd_archive_list(dir.path()).is_ok());
    }

    #[test]
    fn cmd_archive_list_with_cards_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path());
        assert!(cmd_archive_list(dir.path()).is_ok());
    }

    #[test]
    fn cmd_archive_list_uses_completed_date_returns_ok() {
        use board::storage::save_board;
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, id) = setup_board_with_archived_card(dir.path());
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let card_idx = board.columns[archive_idx].cards.iter().position(|c| c.id == id).unwrap();
        board.columns[archive_idx].cards[card_idx].completed =
            Some(chrono::Utc::now() - chrono::Duration::days(30));
        save_board(&kando_dir, &board).unwrap();
        assert!(cmd_archive_list(dir.path()).is_ok());
    }

    #[test]
    fn cmd_archive_list_falls_back_to_updated_returns_ok() {
        // Card with completed = None — should fall back to updated for display date.
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path()); // card has no completed set
        assert!(cmd_archive_list(dir.path()).is_ok());
    }

    #[test]
    fn cmd_archive_list_long_title_does_not_panic() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let id = board.next_card_id();
        // Title with 60 ASCII chars AND multi-byte Unicode to exercise char-safe truncation
        let long_title = "A".repeat(39) + "日本語テスト"; // > 40 chars total
        board.columns[archive_idx].cards.push(Card::new(id, long_title));
        save_board(&kando_dir, &board).unwrap();
        assert!(cmd_archive_list(dir.path()).is_ok(), "long unicode title must not panic");
    }

    // ── cmd_archive_search tests ──

    #[test]
    fn cmd_archive_search_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_archive_search(dir.path(), "query").is_err());
    }

    #[test]
    fn cmd_archive_search_title_match_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path()); // title: "Archived task"
        assert!(cmd_archive_search(dir.path(), "Archived").is_ok());
    }

    #[test]
    fn cmd_archive_search_body_match_returns_ok() {
        use board::storage::{init_board, save_board};
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let id = board.next_card_id();
        let mut card = Card::new(id, "Plain title".into());
        card.body = "Unique body keyword xyzzy".into();
        board.columns[archive_idx].cards.push(card);
        save_board(&kando_dir, &board).unwrap();
        assert!(cmd_archive_search(dir.path(), "xyzzy").is_ok());
    }

    #[test]
    fn cmd_archive_search_no_match_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path());
        // Non-matching query — should print "No archived cards match" and return Ok
        assert!(cmd_archive_search(dir.path(), "zzznomatch").is_ok());
    }

    #[test]
    fn cmd_archive_search_empty_query_returns_ok() {
        // Empty query should list all cards without error
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path());
        assert!(cmd_archive_search(dir.path(), "").is_ok());
    }

    #[test]
    fn cmd_archive_search_empty_query_empty_archive_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        // Empty archive + empty query → "Archive is empty."
        assert!(cmd_archive_search(dir.path(), "").is_ok());
    }

    #[test]
    fn cmd_archive_search_case_insensitive_returns_ok() {
        let dir = tempfile::tempdir().unwrap();
        setup_board_with_archived_card(dir.path()); // title: "Archived task"
        // Uppercase query should still match lowercase title substring
        assert!(cmd_archive_search(dir.path(), "ARCHIVED").is_ok());
    }

    // ── cmd_archive_restore tests ──

    #[test]
    fn cmd_archive_restore_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_archive_restore(dir.path(), "001", "done").is_err());
    }

    #[test]
    fn cmd_archive_restore_card_not_in_archive_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let err = cmd_archive_restore(dir.path(), "nonexistent", "done").unwrap_err();
        assert!(
            format!("{err:#}").contains("not found in archive"),
            "unexpected error: {err:#}"
        );
    }

    #[test]
    fn cmd_archive_restore_invalid_column_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        let (_, card_id) = setup_board_with_archived_card(dir.path());
        let err = cmd_archive_restore(dir.path(), &card_id, "nonexistent-col").unwrap_err();
        assert!(
            format!("{err:#}").contains("not found"),
            "unexpected error: {err:#}"
        );
    }

    #[test]
    fn cmd_archive_restore_to_archive_column_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        let (_, card_id) = setup_board_with_archived_card(dir.path());
        let err = cmd_archive_restore(dir.path(), &card_id, "archive").unwrap_err();
        assert!(
            format!("{err:#}").contains("archive column itself"),
            "unexpected error: {err:#}"
        );
    }

    #[test]
    fn cmd_archive_restore_to_done_moves_card() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, card_id) = setup_board_with_archived_card(dir.path());

        cmd_archive_restore(dir.path(), &card_id, "done").unwrap();

        let board = load_board(&kando_dir).unwrap();
        let done_idx = board.columns.iter().position(|c| c.slug == "done").unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        assert!(
            board.columns[done_idx].cards.iter().any(|c| c.id == card_id),
            "card should be in done column after restore"
        );
        assert!(
            !board.columns[archive_idx].cards.iter().any(|c| c.id == card_id),
            "card should no longer be in archive after restore"
        );
    }

    #[test]
    fn cmd_archive_restore_to_custom_column_moves_card() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, card_id) = setup_board_with_archived_card(dir.path());

        cmd_archive_restore(dir.path(), &card_id, "backlog").unwrap();

        let board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        assert!(
            board.columns[backlog_idx].cards.iter().any(|c| c.id == card_id),
            "card should be in backlog after restoring to backlog"
        );
    }

    #[test]
    fn cmd_archive_restore_preserves_completed_timestamp() {
        use board::storage::save_board;
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let id = board.next_card_id();
        let fixed_completed = chrono::DateTime::from_timestamp(1_000_000, 0).unwrap();
        let mut card = Card::new(id.clone(), "Restore test".into());
        card.completed = Some(fixed_completed);
        board.columns[archive_idx].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        cmd_archive_restore(dir.path(), &id, "done").unwrap();

        let board = load_board(&kando_dir).unwrap();
        let done_idx = board.columns.iter().position(|c| c.slug == "done").unwrap();
        let restored = board.columns[done_idx].cards.iter().find(|c| c.id == id).unwrap();
        assert_eq!(
            restored.completed,
            Some(fixed_completed),
            "restore must preserve the completed timestamp (direct move bypasses move_card)"
        );
    }

    #[test]
    fn cmd_archive_restore_preserves_started_timestamp() {
        use board::storage::save_board;
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let mut board = load_board(&kando_dir).unwrap();
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        let id = board.next_card_id();
        let fixed_started = chrono::DateTime::from_timestamp(500_000, 0).unwrap();
        let mut card = Card::new(id.clone(), "Started restore test".into());
        card.started = Some(fixed_started);
        board.columns[archive_idx].cards.push(card);
        save_board(&kando_dir, &board).unwrap();

        cmd_archive_restore(dir.path(), &id, "done").unwrap();

        let board = load_board(&kando_dir).unwrap();
        let done_idx = board.columns.iter().position(|c| c.slug == "done").unwrap();
        let restored = board.columns[done_idx].cards.iter().find(|c| c.id == id).unwrap();
        assert_eq!(
            restored.started,
            Some(fixed_started),
            "restore must preserve the started timestamp"
        );
    }

    #[test]
    fn cmd_archive_restore_saves_board() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, card_id) = setup_board_with_archived_card(dir.path());

        cmd_archive_restore(dir.path(), &card_id, "done").unwrap();

        // Reload from disk to verify the save happened
        let board = load_board(&kando_dir).unwrap();
        let found = board.columns.iter().any(|col| col.cards.iter().any(|c| c.id == card_id));
        assert!(found, "card should be findable on disk after restore");
        let archive_idx = board.columns.iter().position(|c| c.slug == "archive").unwrap();
        assert!(
            !board.columns[archive_idx].cards.iter().any(|c| c.id == card_id),
            "archive column should not contain the card after restore"
        );
    }

    #[test]
    fn cmd_archive_restore_appends_activity_log() {
        let dir = tempfile::tempdir().unwrap();
        let (kando_dir, card_id) = setup_board_with_archived_card(dir.path());

        cmd_archive_restore(dir.path(), &card_id, "done").unwrap();

        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        assert!(log.contains("\"action\":\"unarchive\""), "activity log should record unarchive action");
        assert!(
            log.contains(&format!("\"id\":\"{card_id}\"")),
            "activity log should include card id"
        );
    }

    // ── resolve_col_cli ──

    fn board_for_col_tests() -> (tempfile::TempDir, std::path::PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        (dir, kando_dir)
    }

    #[test]
    fn resolve_col_cli_exact_slug_match() {
        let (dir, kando_dir) = board_for_col_tests();
        let board = load_board(&kando_dir).unwrap();
        let idx = resolve_col_cli(&board, "backlog").unwrap();
        assert_eq!(board.columns[idx].slug, "backlog");
        let _ = dir;
    }

    #[test]
    fn resolve_col_cli_name_case_insensitive() {
        let (dir, kando_dir) = board_for_col_tests();
        let board = load_board(&kando_dir).unwrap();
        let idx = resolve_col_cli(&board, "IN PROGRESS").unwrap();
        assert_eq!(board.columns[idx].slug, "in-progress");
        let _ = dir;
    }

    #[test]
    fn resolve_col_cli_not_found_returns_err_with_hint() {
        let (dir, kando_dir) = board_for_col_tests();
        let board = load_board(&kando_dir).unwrap();
        let err = resolve_col_cli(&board, "nonexistent").unwrap_err();
        let msg = format!("{err:#}");
        assert!(msg.contains("not found"), "error should say 'not found': {msg}");
        assert!(msg.contains("Backlog"), "error should list valid columns: {msg}");
        let _ = dir;
    }

    #[test]
    fn resolve_col_cli_partial_slug_no_match() {
        let (dir, kando_dir) = board_for_col_tests();
        let board = load_board(&kando_dir).unwrap();
        // "back" is a prefix of "backlog" but not an exact match
        assert!(resolve_col_cli(&board, "back").is_err());
        let _ = dir;
    }

    // ── cmd_col_list ──

    #[test]
    fn cmd_col_list_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_col_list(dir.path()).is_err());
    }

    #[test]
    fn cmd_col_list_returns_ok_with_board() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_list(dir.path()).is_ok());
    }

    // ── cmd_col_add ──

    #[test]
    fn cmd_col_add_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_col_add(dir.path(), "Test", None).is_err());
    }

    #[test]
    fn cmd_col_add_empty_name_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_add(dir.path(), "   ", None).unwrap_err();
        assert!(format!("{err:#}").contains("cannot be empty"));
    }

    #[test]
    fn cmd_col_add_happy_path_column_created() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(board.columns.iter().any(|c| c.name == "Staging"));
    }

    #[test]
    fn cmd_col_add_slug_generated_from_name() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "My Feature", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(board.columns.iter().any(|c| c.slug == "my-feature"));
    }

    #[test]
    fn cmd_col_add_inserted_before_hidden_column() {
        let (dir, kando_dir) = board_for_col_tests();
        let before = load_board(&kando_dir).unwrap();
        let archive_idx_before = before.columns.iter().position(|c| c.slug == "archive").unwrap();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        let after = load_board(&kando_dir).unwrap();
        let staging_idx = after.columns.iter().position(|c| c.slug == "staging").unwrap();
        let archive_idx = after.columns.iter().position(|c| c.slug == "archive").unwrap();
        assert!(staging_idx < archive_idx, "new column should appear before the hidden archive column");
        assert_eq!(archive_idx, archive_idx_before + 1, "archive index should shift by one");
    }

    #[test]
    fn cmd_col_add_after_named_column() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", Some("backlog")).unwrap();
        let board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        let staging_idx = board.columns.iter().position(|c| c.slug == "staging").unwrap();
        assert_eq!(staging_idx, backlog_idx + 1, "staging should be directly after backlog");
    }

    #[test]
    fn cmd_col_add_after_invalid_column_returns_err() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_add(dir.path(), "Staging", Some("nonexistent")).is_err());
    }

    #[test]
    fn cmd_col_add_orders_normalized_after_insert() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        for (i, col) in board.columns.iter().enumerate() {
            assert_eq!(col.order, i as u32, "column '{}' order should be {}", col.slug, i);
        }
    }

    #[test]
    fn cmd_col_add_saves_board_persistently() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(board.columns.iter().any(|c| c.slug == "staging"), "column should persist after reload");
    }

    #[test]
    fn cmd_col_add_creates_column_dir_on_disk() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        assert!(kando_dir.join("columns").join("staging").is_dir(), "column directory should be created");
    }

    #[test]
    fn cmd_col_add_duplicate_name_gets_unique_slug() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Backlog", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        // Original backlog slug is "backlog"; duplicate should get "backlog-2"
        assert!(board.columns.iter().any(|c| c.slug == "backlog-2"), "duplicate should get slug backlog-2");
    }

    #[test]
    fn cmd_col_add_archive_name_gets_different_slug() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Archive", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        // Reserved slug "archive" already exists, so new column gets slug "archive-col".
        // The display name is derived from the slug: "Archive Col".
        let new_col = board.columns.iter().find(|c| c.slug == "archive-col");
        assert!(new_col.is_some(), "new column should get slug 'archive-col': {:?}", board.columns.iter().map(|c| &c.slug).collect::<Vec<_>>());
    }

    #[test]
    fn cmd_col_add_appends_activity_log() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_add(dir.path(), "Staging", None).unwrap();
        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap_or_default();
        assert!(log.contains("\"action\":\"col-add\""), "activity log should have col-add entry");
        assert!(log.contains("\"id\":\"staging\""), "activity log should reference the new slug");
    }

    // ── cmd_col_remove ──

    #[test]
    fn cmd_col_remove_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_col_remove(dir.path(), "backlog", None).is_err());
    }

    #[test]
    fn cmd_col_remove_archive_reserved_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_remove(dir.path(), "archive", None).unwrap_err();
        assert!(format!("{err:#}").contains("reserved"));
    }

    #[test]
    fn cmd_col_remove_nonexistent_column_returns_err() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_remove(dir.path(), "nonexistent", None).is_err());
    }

    #[test]
    fn cmd_col_remove_last_column_returns_err() {
        // Reduce the board to a single column (archive and all others stripped),
        // leaving only "backlog". The guard is `board.columns.len() == 1`.
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        board.columns.retain(|c| c.slug == "backlog");
        board::storage::save_board(&kando_dir, &board).unwrap();
        let err = cmd_col_remove(dir.path(), "backlog", None).unwrap_err();
        assert!(format!("{err:#}").contains("last column"));
    }

    #[test]
    fn cmd_col_remove_nonempty_without_move_to_returns_err() {
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        board.columns[backlog_idx].cards.push(Card::new("001".into(), "Test Card".into()));
        board::storage::save_board(&kando_dir, &board).unwrap();
        let err = cmd_col_remove(dir.path(), "backlog", None).unwrap_err();
        let msg = format!("{err:#}");
        assert!(msg.contains("1 card"), "should mention card count: {msg}");
        assert!(msg.contains("--move-to"), "should suggest --move-to: {msg}");
    }

    #[test]
    fn cmd_col_remove_empty_column_succeeds() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_remove(dir.path(), "backlog", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(!board.columns.iter().any(|c| c.slug == "backlog"));
    }

    #[test]
    fn cmd_col_remove_empty_column_dir_deleted() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_remove(dir.path(), "backlog", None).unwrap();
        assert!(!kando_dir.join("columns").join("backlog").exists(), "column dir should be removed from disk");
    }

    #[test]
    fn cmd_col_remove_with_move_to_drains_cards() {
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        board.columns[backlog_idx].cards.push(Card::new("001".into(), "Card 1".into()));
        board.columns[backlog_idx].cards.push(Card::new("002".into(), "Card 2".into()));
        board::storage::save_board(&kando_dir, &board).unwrap();
        cmd_col_remove(dir.path(), "backlog", Some("done")).unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(!board.columns.iter().any(|c| c.slug == "backlog"), "removed column should be gone");
        let done = board.columns.iter().find(|c| c.slug == "done").unwrap();
        assert_eq!(done.cards.len(), 2, "both cards should be in 'done'");
    }

    #[test]
    fn cmd_col_remove_move_to_same_column_returns_err() {
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        board.columns[backlog_idx].cards.push(Card::new("001".into(), "Card".into()));
        board::storage::save_board(&kando_dir, &board).unwrap();
        let err = cmd_col_remove(dir.path(), "backlog", Some("backlog")).unwrap_err();
        assert!(format!("{err:#}").contains("same column"));
    }

    #[test]
    fn cmd_col_remove_move_to_invalid_column_returns_err() {
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        board.columns[backlog_idx].cards.push(Card::new("001".into(), "Card".into()));
        board::storage::save_board(&kando_dir, &board).unwrap();
        assert!(cmd_col_remove(dir.path(), "backlog", Some("nonexistent")).is_err());
    }

    #[test]
    fn cmd_col_remove_orders_normalized_after_remove() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_remove(dir.path(), "in-progress", None).unwrap();
        let board = load_board(&kando_dir).unwrap();
        for (i, col) in board.columns.iter().enumerate() {
            assert_eq!(col.order, i as u32, "orders should be dense after remove");
        }
    }

    #[test]
    fn cmd_col_remove_appends_activity_log() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_remove(dir.path(), "backlog", None).unwrap();
        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap_or_default();
        assert!(log.contains("\"action\":\"col-remove\""));
        assert!(log.contains("\"id\":\"backlog\""));
    }

    // ── cmd_col_rename ──

    #[test]
    fn cmd_col_rename_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_col_rename(dir.path(), "backlog", "Queue").is_err());
    }

    #[test]
    fn cmd_col_rename_empty_new_name_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_rename(dir.path(), "backlog", "   ").unwrap_err();
        assert!(format!("{err:#}").contains("cannot be empty"));
    }

    #[test]
    fn cmd_col_rename_happy_path_name_updated() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_rename(dir.path(), "backlog", "Queue").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(board.columns.iter().any(|c| c.name == "Queue"));
    }

    #[test]
    fn cmd_col_rename_slug_changes() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_rename(dir.path(), "backlog", "Queue").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert!(board.columns.iter().any(|c| c.slug == "queue"), "slug should change to 'queue'");
        assert!(!board.columns.iter().any(|c| c.slug == "backlog"), "old slug 'backlog' should be gone");
    }

    #[test]
    fn cmd_col_rename_name_derived_from_slug() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_rename(dir.path(), "backlog", "Queue").unwrap();
        let board = load_board(&kando_dir).unwrap();
        let col = board.columns.iter().find(|c| c.slug == "queue").unwrap();
        assert_eq!(col.name, "Queue");
    }

    #[test]
    fn cmd_col_rename_duplicate_slug_returns_err() {
        let (dir, _) = board_for_col_tests();
        // "In Progress" → slug "in-progress" already exists
        // "Backlog" → slug "backlog" already exists for another column
        let err = cmd_col_rename(dir.path(), "in-progress", "Backlog").unwrap_err();
        assert!(format!("{err:#}").contains("already exists"));
    }

    #[test]
    fn cmd_col_rename_same_name_as_self_is_allowed() {
        let (dir, _) = board_for_col_tests();
        // Renaming "Backlog" to its own slug-equivalent name is a no-op, no error.
        assert!(cmd_col_rename(dir.path(), "backlog", "Backlog").is_ok());
    }

    #[test]
    fn cmd_col_rename_saves_board_persistently() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_rename(dir.path(), "backlog", "Queue").unwrap();
        let board = load_board(&kando_dir).unwrap();
        let col = board.columns.iter().find(|c| c.slug == "queue").unwrap();
        assert_eq!(col.name, "Queue");
    }

    #[test]
    fn cmd_col_rename_appends_activity_log() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_rename(dir.path(), "backlog", "Queue").unwrap();
        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap_or_default();
        assert!(log.contains("\"action\":\"col-rename\""));
        assert!(log.contains("\"id\":\"queue\""), "should reference the new slug: {log}");
        assert!(log.contains("\"from\":\"backlog\""), "should log old slug as 'from': {log}");
    }

    #[test]
    fn cmd_col_rename_nonexistent_column_returns_err() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_rename(dir.path(), "nonexistent", "Whatever").is_err());
    }

    // ── cmd_col_move ──

    #[test]
    fn cmd_col_move_no_kando_dir_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        assert!(cmd_col_move(dir.path(), "backlog", "right").is_err());
    }

    #[test]
    fn cmd_col_move_left_happy_path() {
        let (dir, kando_dir) = board_for_col_tests();
        // In Progress is at index 1; move it left → index 0
        cmd_col_move(dir.path(), "in-progress", "left").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns[0].slug, "in-progress");
        assert_eq!(board.columns[1].slug, "backlog");
    }

    #[test]
    fn cmd_col_move_left_already_leftmost_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_move(dir.path(), "backlog", "left").unwrap_err();
        assert!(format!("{err:#}").contains("leftmost"));
    }

    #[test]
    fn cmd_col_move_right_happy_path() {
        let (dir, kando_dir) = board_for_col_tests();
        // Backlog is at index 0; move it right → index 1
        cmd_col_move(dir.path(), "backlog", "right").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns[0].slug, "in-progress");
        assert_eq!(board.columns[1].slug, "backlog");
    }

    #[test]
    fn cmd_col_move_right_already_rightmost_returns_err() {
        let (dir, _) = board_for_col_tests();
        // Move "done" to last position first so we test with a non-hidden rightmost column.
        cmd_col_move(dir.path(), "done", "last").unwrap();
        let err = cmd_col_move(dir.path(), "done", "right").unwrap_err();
        assert!(format!("{err:#}").contains("rightmost"));
    }

    #[test]
    fn cmd_col_move_first_happy_path() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_move(dir.path(), "done", "first").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns[0].slug, "done");
    }

    #[test]
    fn cmd_col_move_last_happy_path() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_move(dir.path(), "backlog", "last").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns.last().unwrap().slug, "backlog");
    }

    #[test]
    fn cmd_col_move_numeric_position_valid() {
        let (dir, kando_dir) = board_for_col_tests();
        // Move backlog (currently position 1) to position 3 → index 2.
        cmd_col_move(dir.path(), "backlog", "3").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns[2].slug, "backlog");
    }

    #[test]
    fn cmd_col_move_numeric_position_0_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_move(dir.path(), "backlog", "0").unwrap_err();
        assert!(format!("{err:#}").contains("Invalid position"));
    }

    #[test]
    fn cmd_col_move_numeric_position_exceeds_len_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_move(dir.path(), "backlog", "99").unwrap_err();
        assert!(format!("{err:#}").contains("Invalid position"));
    }

    #[test]
    fn cmd_col_move_invalid_string_returns_err() {
        let (dir, _) = board_for_col_tests();
        let err = cmd_col_move(dir.path(), "backlog", "sideways").unwrap_err();
        assert!(format!("{err:#}").contains("Invalid position"));
    }

    #[test]
    fn cmd_col_move_orders_assigned_by_vec_position() {
        // Critical regression test: after a move, order values must reflect
        // the new Vec positions, NOT the stale pre-move order values.
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_move(dir.path(), "done", "first").unwrap();
        let board = load_board(&kando_dir).unwrap();
        for (i, col) in board.columns.iter().enumerate() {
            assert_eq!(col.order, i as u32,
                "column '{}' at index {} should have order {}", col.slug, i, i);
        }
    }

    #[test]
    fn cmd_col_move_saves_board_persistently() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_move(dir.path(), "done", "first").unwrap();
        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.columns[0].slug, "done", "move should persist across reload");
    }

    #[test]
    fn cmd_col_move_appends_activity_log() {
        let (dir, kando_dir) = board_for_col_tests();
        cmd_col_move(dir.path(), "done", "first").unwrap();
        let log = fs::read_to_string(kando_dir.join("activity.log")).unwrap_or_default();
        assert!(log.contains("\"action\":\"col-move\""));
        assert!(log.contains("\"id\":\"done\""));
        // "done" moved to "first" → target_idx=0 → new_pos=1 (1-indexed).
        assert!(log.contains("\"to\":\"1\""), "should log position 1 as 'to': {log}");
    }

    #[test]
    fn cmd_col_move_nonexistent_column_returns_err() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_move(dir.path(), "nonexistent", "first").is_err());
    }

    #[test]
    fn cmd_col_move_same_position_is_noop() {
        let (dir, kando_dir) = board_for_col_tests();
        // backlog is at index 0, which is position "1"
        cmd_col_move(dir.path(), "backlog", "1").unwrap();
        let board = load_board(&kando_dir).unwrap();
        // Board should be unchanged
        assert_eq!(board.columns[0].slug, "backlog");
    }

    #[test]
    fn cmd_col_move_cards_preserved_after_reorder() {
        let (dir, kando_dir) = board_for_col_tests();
        let mut board = load_board(&kando_dir).unwrap();
        let backlog_idx = board.columns.iter().position(|c| c.slug == "backlog").unwrap();
        board.columns[backlog_idx].cards.push(Card::new("001".into(), "Important Card".into()));
        board::storage::save_board(&kando_dir, &board).unwrap();
        cmd_col_move(dir.path(), "backlog", "last").unwrap();
        let board = load_board(&kando_dir).unwrap();
        let backlog = board.columns.iter().find(|c| c.slug == "backlog").unwrap();
        assert_eq!(backlog.cards.len(), 1, "cards should be preserved after column reorder");
        assert_eq!(backlog.cards[0].id, "001");
    }

    // ── cmd_col_cli dispatcher ──

    #[test]
    fn cmd_col_cli_no_action_defaults_to_list() {
        let (dir, _) = board_for_col_tests();
        assert!(cmd_col_cli(dir.path(), None).is_ok());
    }

    #[test]
    fn bubble_up_days_alias_deserializes_as_stale_days() {
        // Simulate a legacy config.toml that uses the old field name "bubble_up_days".
        // Use a non-default value (42) so that a broken alias would produce the default (7), not 42.
        let dir = tempfile::tempdir().unwrap();
        board::storage::init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let config_path = kando_dir.join("config.toml");

        // Write a non-default value via the normal API (serializes as "stale_days = 42")
        cmd_config_stale_days(dir.path(), 42).unwrap();

        // Patch the specific "stale_days = 42" line to use the legacy key
        let content = fs::read_to_string(&config_path).unwrap();
        let patched = content.replace("stale_days = 42", "bubble_up_days = 42");
        fs::write(&config_path, patched).unwrap();

        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.policies.stale_days, 42,
            "legacy bubble_up_days = 42 should deserialize as stale_days = 42 via serde alias");
    }
}
