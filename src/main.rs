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

use board::storage::{find_kando_dir, init_board, load_board, save_board};
use board::{Card, Priority};

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
    /// Stream activity log to stdout (JSONL, one entry per line)
    Log,
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
enum ConfigSetting {
    /// Set WIP limit for a column
    Wip {
        /// Column slug (e.g. in-progress)
        column: String,
        /// WIP limit (0 to remove)
        limit: u32,
    },
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
        Some(Command::Move { card_id, column }) => cmd_move(&cwd, &card_id, &column),
        Some(Command::Tags) => cmd_tags(&cwd),
        Some(Command::Sync) => cmd_sync(&cwd),
        Some(Command::Config { setting }) => match setting {
            ConfigSetting::Wip { column, limit } => cmd_config_wip(&cwd, &column, limit),
        },
        Some(Command::SyncStatus) => cmd_sync_status(&cwd),
        Some(Command::Doctor) => cmd_doctor(&cwd),
        Some(Command::Metrics { weeks, csv }) => cmd_metrics(&cwd, weeks, csv),
        Some(Command::Trash { action }) => cmd_trash(&cwd, action),
        Some(Command::Log) => cmd_log(&cwd),
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
}
