use std::fs;
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::{Board, Card, Column, Policies};
use crate::config::BoardConfig;

#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("toml serialization error: {0}")]
    TomlSer(#[from] toml::ser::Error),
    #[error("toml deserialization error: {0}")]
    TomlDe(#[from] toml::de::Error),
    #[error(".kando directory not found (walk up from {0})")]
    NotFound(PathBuf),
    #[error("invalid card file {path}: {reason}")]
    InvalidCard { path: PathBuf, reason: String },
    #[error("invalid slug: {0:?} (must match [a-z0-9-]+)")]
    InvalidSlug(String),
}

/// Validate that a slug is safe for use as a directory name.
/// Must start with a letter or digit, then only lowercase alphanumeric and hyphens.
fn validate_slug(slug: &str) -> Result<(), StorageError> {
    let first = slug.as_bytes().first().copied().unwrap_or(0);
    if slug.is_empty()
        || !(first.is_ascii_lowercase() || first.is_ascii_digit())
        || !slug.bytes().all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-')
    {
        return Err(StorageError::InvalidSlug(slug.to_string()));
    }
    Ok(())
}

/// Find the .kando directory by walking up from `start`.
pub fn find_kando_dir(start: &Path) -> Result<PathBuf, StorageError> {
    let mut dir = start.to_path_buf();
    loop {
        let candidate = dir.join(".kando");
        if candidate.is_dir() {
            return Ok(candidate);
        }
        if !dir.pop() {
            return Err(StorageError::NotFound(start.to_path_buf()));
        }
    }
}

/// Initialize a new .kando directory with default config and columns.
pub fn init_board(root: &Path, name: &str, sync_branch: Option<&str>) -> Result<PathBuf, StorageError> {
    let kando_dir = root.join(".kando");
    fs::create_dir_all(&kando_dir)?;

    let columns_dir = kando_dir.join("columns");

    let default_columns = vec![
        ColumnConfig {
            slug: "backlog".into(),
            name: "Backlog".into(),
            order: 0,
            wip_limit: None,
            hidden: None,
        },
        ColumnConfig {
            slug: "in-progress".into(),
            name: "In Progress".into(),
            order: 1,
            wip_limit: Some(3),
            hidden: None,
        },
        ColumnConfig {
            slug: "done".into(),
            name: "Done".into(),
            order: 2,
            wip_limit: None,
            hidden: None,
        },
        ColumnConfig {
            slug: "archive".into(),
            name: "Archive".into(),
            order: 3,
            wip_limit: None,
            hidden: Some(true),
        },
    ];

    // Create column directories with _meta.toml
    for col in &default_columns {
        let col_dir = columns_dir.join(&col.slug);
        fs::create_dir_all(&col_dir)?;

        let meta_str = toml::to_string_pretty(col)?;
        fs::write(col_dir.join("_meta.toml"), meta_str)?;
    }

    // Write config.toml
    let config = BoardConfig {
        board: BoardSection {
            name: name.to_string(),
            next_card_id: 1,
            policies: Policies::default(),
            sync_branch: sync_branch.map(|s| s.to_string()),
            tutorial_shown: false,
            nerd_font: false,
            created_at: Some(Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string()),
        },
        columns: default_columns,
    };
    let config_str = toml::to_string_pretty(&config)?;
    fs::write(kando_dir.join("config.toml"), config_str)?;

    Ok(kando_dir)
}

/// Load the full board from a .kando directory.
pub fn load_board(kando_dir: &Path) -> Result<Board, StorageError> {
    let config_path = kando_dir.join("config.toml");
    let config_str = fs::read_to_string(&config_path)?;
    let config: BoardConfig = toml::from_str(&config_str)?;

    let columns_dir = kando_dir.join("columns");
    let mut columns = Vec::new();

    for col_config in &config.columns {
        validate_slug(&col_config.slug)?;
        let col_dir = columns_dir.join(&col_config.slug);
        let meta = if col_dir.join("_meta.toml").exists() {
            let meta_str = fs::read_to_string(col_dir.join("_meta.toml"))?;
            toml::from_str::<ColumnConfig>(&meta_str)?
        } else {
            col_config.clone()
        };

        // Load cards
        let mut cards = Vec::new();
        if col_dir.exists() {
            for entry in fs::read_dir(&col_dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) == Some("md") {
                    match load_card(&path) {
                        Ok(card) => cards.push(card),
                        Err(e) => {
                            eprintln!("Warning: skipping invalid card {}: {e}", path.display());
                        }
                    }
                }
            }
        }

        let mut col = Column {
            slug: col_config.slug.clone(),
            name: meta.name,
            order: meta.order,
            wip_limit: meta.wip_limit,
            hidden: meta.hidden.unwrap_or(false),
            cards,
        };
        col.sort_cards();
        columns.push(col);
    }

    columns.sort_by_key(|c| c.order);

    let created_at = config
        .board
        .created_at
        .as_deref()
        .and_then(|s| s.parse::<DateTime<Utc>>().ok());

    Ok(Board {
        name: config.board.name,
        next_card_id: config.board.next_card_id,
        policies: config.board.policies,
        sync_branch: config.board.sync_branch,
        tutorial_shown: config.board.tutorial_shown,
        nerd_font: config.board.nerd_font,
        created_at,
        columns,
    })
}

/// Save the full board back to the .kando directory.
pub fn save_board(kando_dir: &Path, board: &Board) -> Result<(), StorageError> {
    let columns_dir = kando_dir.join("columns");

    // Build config
    let column_configs: Vec<ColumnConfig> = board
        .columns
        .iter()
        .map(|col| ColumnConfig {
            slug: col.slug.clone(),
            name: col.name.clone(),
            order: col.order,
            wip_limit: col.wip_limit,
            hidden: if col.hidden { Some(true) } else { None },
        })
        .collect();

    let config = BoardConfig {
        board: BoardSection {
            name: board.name.clone(),
            next_card_id: board.next_card_id,
            policies: board.policies.clone(),
            sync_branch: board.sync_branch.clone(),
            tutorial_shown: board.tutorial_shown,
            nerd_font: board.nerd_font,
            created_at: board.created_at.map(|dt| dt.format("%Y-%m-%dT%H:%M:%SZ").to_string()),
        },
        columns: column_configs,
    };
    // Validate all slugs upfront before writing anything
    for col in &board.columns {
        validate_slug(&col.slug)?;
    }

    let config_str = toml::to_string_pretty(&config)?;
    fs::write(kando_dir.join("config.toml"), config_str)?;

    // Save each column
    for col in &board.columns {
        let col_dir = columns_dir.join(&col.slug);
        fs::create_dir_all(&col_dir)?;

        // Write _meta.toml
        let meta = ColumnConfig {
            slug: col.slug.clone(),
            name: col.name.clone(),
            order: col.order,
            wip_limit: col.wip_limit,
            hidden: if col.hidden { Some(true) } else { None },
        };
        let meta_str = toml::to_string_pretty(&meta)?;
        fs::write(col_dir.join("_meta.toml"), meta_str)?;

        // Remove card files that no longer exist in this column
        if col_dir.exists() {
            let current_ids: std::collections::HashSet<String> =
                col.cards.iter().map(|c| format!("{}.md", c.id)).collect();
            for entry in fs::read_dir(&col_dir)? {
                let entry = entry?;
                let name = entry.file_name().to_string_lossy().to_string();
                if name.ends_with(".md") && !current_ids.contains(&name) {
                    fs::remove_file(entry.path())?;
                }
            }
        }

        // Write card files (skip unchanged cards)
        for card in &col.cards {
            let card_path = col_dir.join(format!("{}.md", card.id));
            let content = serialize_card(card);
            let needs_write = match fs::read_to_string(&card_path) {
                Ok(existing) => existing.replace("\r\n", "\n") != content,
                Err(_) => true,
            };
            if needs_write {
                fs::write(&card_path, content)?;
            }
        }
    }

    Ok(())
}

/// Parse a card .md file with TOML frontmatter.
fn load_card(path: &Path) -> Result<Card, StorageError> {
    let content = fs::read_to_string(path)?;
    let (frontmatter, body) = parse_frontmatter(&content).ok_or_else(|| {
        StorageError::InvalidCard {
            path: path.to_path_buf(),
            reason: "missing or invalid TOML frontmatter".into(),
        }
    })?;

    let mut card: Card = toml::from_str(&frontmatter).map_err(|e| StorageError::InvalidCard {
        path: path.to_path_buf(),
        reason: format!("invalid TOML: {e}"),
    })?;
    // Validate card ID is safe for use as a filename
    if card.id.is_empty()
        || !card.id.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
    {
        return Err(StorageError::InvalidCard {
            path: path.to_path_buf(),
            reason: format!("unsafe card id: {:?}", card.id),
        });
    }
    card.body = body;
    Ok(card)
}

/// Serialize a card to the frontmatter + markdown body format.
fn serialize_card(card: &Card) -> String {
    // Build frontmatter manually for clean formatting
    let mut fm = String::new();
    fm.push_str(&format!("id = {:?}\n", card.id));
    fm.push_str(&format!("title = {:?}\n", card.title));
    fm.push_str(&format!(
        "created = \"{}\"\n",
        card.created.format("%Y-%m-%dT%H:%M:%SZ")
    ));
    fm.push_str(&format!(
        "updated = \"{}\"\n",
        card.updated.format("%Y-%m-%dT%H:%M:%SZ")
    ));
    fm.push_str(&format!("priority = {:?}\n", card.priority.as_str()));
    if !card.tags.is_empty() {
        let tags: Vec<String> = card.tags.iter().map(|t| format!("{t:?}")).collect();
        fm.push_str(&format!("tags = [{}]\n", tags.join(", ")));
    } else {
        fm.push_str("tags = []\n");
    }
    if !card.assignees.is_empty() {
        let assignees: Vec<String> = card.assignees.iter().map(|a| format!("{a:?}")).collect();
        fm.push_str(&format!("assignees = [{}]\n", assignees.join(", ")));
    }
    if card.blocked {
        fm.push_str("blocked = true\n");
    }
    if let Some(started) = card.started {
        fm.push_str(&format!(
            "started = \"{}\"\n",
            started.format("%Y-%m-%dT%H:%M:%SZ")
        ));
    }
    if let Some(completed) = card.completed {
        fm.push_str(&format!(
            "completed = \"{}\"\n",
            completed.format("%Y-%m-%dT%H:%M:%SZ")
        ));
    }

    let mut out = String::new();
    out.push_str("---\n");
    out.push_str(&fm);
    out.push_str("---\n");
    if !card.body.is_empty() {
        out.push('\n');
        out.push_str(&card.body);
        if !card.body.ends_with('\n') {
            out.push('\n');
        }
    }
    out
}

/// Parse `---` delimited TOML frontmatter from a string.
/// Returns (frontmatter, body).
///
/// Normalizes `\r\n` to `\n` so files edited on Windows parse correctly.
fn parse_frontmatter(content: &str) -> Option<(String, String)> {
    let content = content.replace("\r\n", "\n");
    let content = content.trim_start();
    if !content.starts_with("---") {
        return None;
    }
    let after_first = &content[3..];
    let after_first = after_first.strip_prefix('\n').unwrap_or(after_first);
    let end = after_first.find("\n---")?;
    let frontmatter = after_first[..end].to_string();
    let rest = &after_first[end + 4..];
    let body = rest.strip_prefix('\n').unwrap_or(rest).to_string();
    let body = body.trim().to_string();
    Some((frontmatter, body))
}

// ── Serialization helpers for config/meta files ──

#[derive(Debug, Serialize, Deserialize)]
pub struct BoardSection {
    pub name: String,
    pub next_card_id: u32,
    #[serde(default)]
    pub policies: Policies,
    /// Git branch to sync with. None = no sync.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sync_branch: Option<String>,
    /// Whether the first-launch tutorial has been shown.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub tutorial_shown: bool,
    /// Use Nerd Font glyphs instead of ASCII icons.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub nerd_font: bool,
    /// When the board was created (ISO-8601 string).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created_at: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColumnConfig {
    #[serde(default)]
    pub slug: String,
    pub name: String,
    pub order: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wip_limit: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hidden: Option<bool>,
}

// ── Trash (soft-delete) ──

/// A single entry in `.kando/.trash/_meta.toml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrashEntry {
    pub id: String,
    pub deleted: String, // ISO-8601 string, quoted for serde/toml compat
    pub from_column: String,
    pub title: String,
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct TrashMeta {
    #[serde(default)]
    entries: Vec<TrashEntry>,
}

/// Path to the trash directory.
fn trash_dir(kando_dir: &Path) -> PathBuf {
    kando_dir.join(".trash")
}

/// Path to the trash metadata file.
fn trash_meta_path(kando_dir: &Path) -> PathBuf {
    trash_dir(kando_dir).join("_meta.toml")
}

/// Move a card file to `.trash/` and record metadata.
/// Returns the `TrashEntry` (for undo tracking).
pub fn trash_card(
    kando_dir: &Path,
    col_slug: &str,
    card_id: &str,
    card_title: &str,
) -> Result<TrashEntry, StorageError> {
    let src = kando_dir
        .join("columns")
        .join(col_slug)
        .join(format!("{card_id}.md"));
    let dst_dir = trash_dir(kando_dir);
    fs::create_dir_all(&dst_dir)?;
    let dst = dst_dir.join(format!("{card_id}.md"));

    // Move file (rename if same filesystem, else copy+delete)
    if fs::rename(&src, &dst).is_err() {
        fs::copy(&src, &dst)?;
        fs::remove_file(&src)?;
    }

    let entry = TrashEntry {
        id: card_id.to_string(),
        deleted: Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
        from_column: col_slug.to_string(),
        title: card_title.to_string(),
    };

    // Append entry to _meta.toml
    let mut meta = load_trash_meta(kando_dir);
    meta.entries.push(entry.clone());
    save_trash_meta(kando_dir, &meta)?;

    Ok(entry)
}

/// Restore a card from `.trash/` back to a column directory.
/// Removes the entry from `_meta.toml`.
pub fn restore_card(
    kando_dir: &Path,
    card_id: &str,
    target_col_slug: &str,
) -> Result<(), StorageError> {
    let src = trash_dir(kando_dir).join(format!("{card_id}.md"));
    let dst_dir = kando_dir.join("columns").join(target_col_slug);
    fs::create_dir_all(&dst_dir)?;
    let dst = dst_dir.join(format!("{card_id}.md"));

    if fs::rename(&src, &dst).is_err() {
        fs::copy(&src, &dst)?;
        fs::remove_file(&src)?;
    }

    // Remove from _meta.toml
    let mut meta = load_trash_meta(kando_dir);
    meta.entries.retain(|e| e.id != card_id);
    save_trash_meta(kando_dir, &meta)?;

    Ok(())
}

/// Load all trash entries from `_meta.toml`.
pub fn load_trash(kando_dir: &Path) -> Vec<TrashEntry> {
    load_trash_meta(kando_dir).entries
}

/// Purge trash entries older than `max_age_days`. Returns IDs of purged cards.
pub fn purge_trash(kando_dir: &Path, max_age_days: u32) -> Result<Vec<String>, StorageError> {
    if max_age_days == 0 {
        return Ok(Vec::new());
    }

    let mut meta = load_trash_meta(kando_dir);
    let now = Utc::now();
    let mut purged = Vec::new();
    let tdir = trash_dir(kando_dir);

    meta.entries.retain(|entry| {
        let keep = match entry.deleted.parse::<DateTime<Utc>>() {
            Ok(deleted) => (now - deleted).num_days() < i64::from(max_age_days),
            Err(_) => true, // keep entries with unparseable dates
        };
        if !keep {
            let card_file = tdir.join(format!("{}.md", entry.id));
            let _ = fs::remove_file(card_file);
            purged.push(entry.id.clone());
        }
        keep
    });

    if !purged.is_empty() {
        save_trash_meta(kando_dir, &meta)?;
    }

    Ok(purged)
}

/// Internal: load trash metadata, returning empty on missing/corrupt file.
fn load_trash_meta(kando_dir: &Path) -> TrashMeta {
    let path = trash_meta_path(kando_dir);
    match fs::read_to_string(&path) {
        Ok(content) => toml::from_str(&content).unwrap_or_default(),
        Err(_) => TrashMeta::default(),
    }
}

/// Internal: write trash metadata to disk.
fn save_trash_meta(kando_dir: &Path, meta: &TrashMeta) -> Result<(), StorageError> {
    let dir = trash_dir(kando_dir);
    fs::create_dir_all(&dir)?;
    let content = toml::to_string_pretty(meta)?;
    fs::write(trash_meta_path(kando_dir), content)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Activity log (.kando/activity.log — committed, append-only JSONL)
// ---------------------------------------------------------------------------

/// Escape a string as a JSON-encoded string value (including surrounding quotes).
///
/// ASCII control characters (U+0000–U+001F) are written as `\uXXXX`.
/// All other characters, including non-ASCII Unicode, are written as raw UTF-8,
/// which is valid JSON per RFC 8259 §8.1.  Surrogate-pair encoding for code
/// points above U+FFFF is intentionally not performed; standard JSON parsers
/// handle raw UTF-8 correctly.
fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"'  => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Append a single JSONL event to `.kando/activity.log`.
///
/// The common fields are `ts`, `action`, `id`, and `title`.  `extras` is a
/// slice of `(key, value)` pairs that are appended after them.  The write is
/// best-effort: any I/O error is silently discarded so a log failure never
/// interrupts normal board operations.
pub fn append_activity(
    kando_dir: &Path,
    action: &str,
    card_id: &str,
    card_title: &str,
    extras: &[(&str, &str)],
) {
    let _ = try_append_activity(kando_dir, action, card_id, card_title, extras);
}

fn try_append_activity(
    kando_dir: &Path,
    action: &str,
    card_id: &str,
    card_title: &str,
    extras: &[(&str, &str)],
) -> std::io::Result<()> {
    use std::io::Write;
    let ts = chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let mut line = format!(
        "{{\"ts\":{},\"action\":{},\"id\":{},\"title\":{}",
        json_escape(&ts),
        json_escape(action),
        json_escape(card_id),
        json_escape(card_title),
    );
    for (k, v) in extras {
        line.push(',');
        line.push_str(&json_escape(k));
        line.push(':');
        line.push_str(&json_escape(v));
    }
    line.push('}');

    let path = kando_dir.join("activity.log");
    let mut file = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)?;
    writeln!(file, "{line}")?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Local config (.kando/local.toml — gitignored, per-user preferences)
// ---------------------------------------------------------------------------

/// Load per-user local preferences from `.kando/local.toml`.
/// Returns `Ok(default)` if the file is absent; surfaces a `StorageError`
/// if the file exists but cannot be parsed, so callers can warn the user.
pub fn load_local_config(kando_dir: &Path) -> Result<crate::config::LocalConfig, StorageError> {
    let path = kando_dir.join("local.toml");
    if !path.exists() {
        return Ok(crate::config::LocalConfig::default());
    }
    let content = fs::read_to_string(&path)?;
    Ok(toml::from_str(&content)?)
}

/// Persist per-user local preferences to `.kando/local.toml`.
/// A best-effort attempt is made to add `local.toml` to `.kando/.gitignore`;
/// gitignore failures are intentionally ignored so they never block the save.
pub fn save_local_config(kando_dir: &Path, config: &crate::config::LocalConfig) -> Result<(), StorageError> {
    let content = toml::to_string_pretty(config)?;
    fs::write(kando_dir.join("local.toml"), content)?;
    let _ = ensure_local_gitignore(kando_dir); // best-effort
    Ok(())
}

/// Ensure `.kando/.gitignore` contains a `local.toml` entry.
/// Opens the file once with read+write access to avoid a TOCTOU window.
fn ensure_local_gitignore(kando_dir: &Path) -> Result<(), StorageError> {
    use std::io::{Read, Seek, Write};
    let path = kando_dir.join(".gitignore");
    let entry = "local.toml";
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    if content.lines().any(|l| l.trim() == entry) {
        return Ok(());
    }
    file.seek(std::io::SeekFrom::End(0))?;
    if !content.is_empty() && !content.ends_with('\n') {
        file.write_all(b"\n")?;
    }
    writeln!(file, "{entry}")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Priority;
    use std::fs;

    #[test]
    fn test_parse_frontmatter() {
        let content = "---\nid = \"001\"\ntitle = \"Test\"\n---\n\nBody text here.\n";
        let (fm, body) = parse_frontmatter(content).unwrap();
        assert!(fm.contains("id = \"001\""));
        assert_eq!(body, "Body text here.");
    }

    #[test]
    fn test_parse_frontmatter_crlf() {
        let content = "---\r\nid = \"001\"\r\ntitle = \"Test\"\r\n---\r\n\r\nBody text here.\r\n";
        let (fm, body) = parse_frontmatter(content).unwrap();
        assert!(fm.contains("id = \"001\""));
        assert_eq!(body, "Body text here.");
    }

    #[test]
    fn test_parse_frontmatter_no_body() {
        let content = "---\nid = \"001\"\ntitle = \"Test\"\n---\n";
        let (fm, body) = parse_frontmatter(content).unwrap();
        assert!(fm.contains("id = \"001\""));
        assert!(body.is_empty());
    }

    #[test]
    fn test_card_roundtrip() {
        let mut card = Card::new("042".into(), "Test roundtrip".into());
        card.tags = vec!["bug".into(), "ui".into()];
        card.priority = Priority::High;
        card.body = "Some description here.".into();

        let serialized = serialize_card(&card);
        let (fm, body) = parse_frontmatter(&serialized).unwrap();
        let deserialized: Card = toml::from_str(&fm).unwrap();

        assert_eq!(deserialized.id, "042");
        assert_eq!(deserialized.title, "Test roundtrip");
        assert_eq!(deserialized.priority, Priority::High);
        assert_eq!(deserialized.tags, vec!["bug", "ui"]);
        assert_eq!(body, "Some description here.");
    }

    #[test]
    fn test_init_and_load_board() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test Project", None).unwrap();

        let kando_dir = dir.path().join(".kando");
        assert!(kando_dir.exists());
        assert!(kando_dir.join("config.toml").exists());
        assert!(kando_dir.join("columns/backlog/_meta.toml").exists());
        assert!(kando_dir.join("columns/in-progress/_meta.toml").exists());
        assert!(kando_dir.join("columns/done/_meta.toml").exists());
        assert!(kando_dir.join("columns/archive/_meta.toml").exists());

        let board = load_board(&kando_dir).unwrap();
        assert_eq!(board.name, "Test Project");
        assert_eq!(board.columns.len(), 4);
        assert_eq!(board.columns[0].slug, "backlog");
        assert_eq!(board.columns[1].slug, "in-progress");
        assert_eq!(board.columns[1].wip_limit, Some(3));
        assert_eq!(board.columns[3].hidden, true);
    }

    #[test]
    fn test_save_and_load_with_cards() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        let mut card = Card::new(id, "Test card".into());
        card.tags = vec!["test".into()];
        board.columns[0].cards.push(card);

        save_board(&kando_dir, &board).unwrap();

        let reloaded = load_board(&kando_dir).unwrap();
        assert_eq!(reloaded.columns[0].cards.len(), 1);
        assert_eq!(reloaded.columns[0].cards[0].title, "Test card");
        assert_eq!(reloaded.columns[0].cards[0].tags, vec!["test"]);
        assert_eq!(reloaded.next_card_id, 2);
    }

    #[test]
    fn test_find_kando_dir() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();

        // Create a nested directory
        let nested = dir.path().join("src/deep/nested");
        fs::create_dir_all(&nested).unwrap();

        let found = find_kando_dir(&nested).unwrap();
        assert_eq!(found, dir.path().join(".kando"));
    }

    // -----------------------------------------------------------------------
    // Trash (soft-delete) tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_trash_card_moves_file_and_records_meta() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // Create a card on disk
        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Trash me".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = &board.columns[0].slug;
        let src = kando_dir.join("columns").join(col_slug).join(format!("{id}.md"));
        assert!(src.exists());

        // Trash it
        let entry = trash_card(&kando_dir, col_slug, &id, "Trash me").unwrap();
        assert_eq!(entry.id, id);
        assert_eq!(entry.from_column, col_slug.as_str());
        assert_eq!(entry.title, "Trash me");

        // Source gone, trash file present
        assert!(!src.exists());
        let trash_file = kando_dir.join(".trash").join(format!("{id}.md"));
        assert!(trash_file.exists());

        // Meta records the entry
        let entries = load_trash(&kando_dir);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].id, id);
    }

    #[test]
    fn test_restore_card_moves_file_back() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Restore me".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = board.columns[0].slug.clone();
        trash_card(&kando_dir, &col_slug, &id, "Restore me").unwrap();

        // Restore to same column
        restore_card(&kando_dir, &id, &col_slug).unwrap();

        // File is back in the column directory
        let restored = kando_dir.join("columns").join(&col_slug).join(format!("{id}.md"));
        assert!(restored.exists());

        // Trash file gone, meta cleared
        let trash_file = kando_dir.join(".trash").join(format!("{id}.md"));
        assert!(!trash_file.exists());
        assert!(load_trash(&kando_dir).is_empty());
    }

    #[test]
    fn test_restore_card_to_different_column() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Move me".into()));
        save_board(&kando_dir, &board).unwrap();

        let from_slug = board.columns[0].slug.clone();
        let to_slug = board.columns[1].slug.clone();
        trash_card(&kando_dir, &from_slug, &id, "Move me").unwrap();

        // Restore to a different column
        restore_card(&kando_dir, &id, &to_slug).unwrap();

        let restored = kando_dir.join("columns").join(&to_slug).join(format!("{id}.md"));
        assert!(restored.exists());
        let old_loc = kando_dir.join("columns").join(&from_slug).join(format!("{id}.md"));
        assert!(!old_loc.exists());
    }

    #[test]
    fn test_purge_trash_removes_old_entries() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Old card".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = board.columns[0].slug.clone();
        trash_card(&kando_dir, &col_slug, &id, "Old card").unwrap();

        // Manually backdate the entry to 60 days ago
        let mut meta = load_trash_meta(&kando_dir);
        let old_date = (Utc::now() - chrono::TimeDelta::days(60))
            .format("%Y-%m-%dT%H:%M:%SZ")
            .to_string();
        meta.entries[0].deleted = old_date;
        save_trash_meta(&kando_dir, &meta).unwrap();

        // Purge with 30-day limit
        let purged = purge_trash(&kando_dir, 30).unwrap();
        assert_eq!(purged, vec![id.clone()]);

        // File removed, meta empty
        let trash_file = kando_dir.join(".trash").join(format!("{id}.md"));
        assert!(!trash_file.exists());
        assert!(load_trash(&kando_dir).is_empty());
    }

    #[test]
    fn test_purge_trash_keeps_recent_entries() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Recent".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = board.columns[0].slug.clone();
        trash_card(&kando_dir, &col_slug, &id, "Recent").unwrap();

        // Purge with 30 days — card was just trashed, should survive
        let purged = purge_trash(&kando_dir, 30).unwrap();
        assert!(purged.is_empty());

        // Entry still present
        assert_eq!(load_trash(&kando_dir).len(), 1);
    }

    #[test]
    fn test_purge_trash_zero_days_is_disabled() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        let id = board.next_card_id();
        board.columns[0].cards.push(Card::new(id.clone(), "Safe".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = board.columns[0].slug.clone();
        trash_card(&kando_dir, &col_slug, &id, "Safe").unwrap();

        // Backdate to 100 days ago
        let mut meta = load_trash_meta(&kando_dir);
        let old_date = (Utc::now() - chrono::TimeDelta::days(100))
            .format("%Y-%m-%dT%H:%M:%SZ")
            .to_string();
        meta.entries[0].deleted = old_date;
        save_trash_meta(&kando_dir, &meta).unwrap();

        // max_age_days == 0 means never purge
        let purged = purge_trash(&kando_dir, 0).unwrap();
        assert!(purged.is_empty());
        assert_eq!(load_trash(&kando_dir).len(), 1);
    }

    #[test]
    fn test_load_trash_empty_when_no_trash_dir() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // No .trash directory exists yet
        assert!(load_trash(&kando_dir).is_empty());
    }

    #[test]
    fn test_trash_card_missing_source_file() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // Try to trash a card that doesn't exist on disk
        let result = trash_card(&kando_dir, "backlog", "nonexistent", "Ghost");
        assert!(result.is_err());
    }

    #[test]
    fn test_restore_card_missing_trash_file() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // Create .trash dir and _meta.toml with an entry but no actual file
        let trash = kando_dir.join(".trash");
        fs::create_dir_all(&trash).unwrap();
        let meta = TrashMeta {
            entries: vec![TrashEntry {
                id: "orphan".into(),
                deleted: "2025-01-01T00:00:00Z".into(),
                from_column: "backlog".into(),
                title: "Orphan".into(),
            }],
        };
        save_trash_meta(&kando_dir, &meta).unwrap();

        // Restore should fail because the .md file is missing
        let result = restore_card(&kando_dir, "orphan", "backlog");
        assert!(result.is_err());
    }

    #[test]
    fn test_board_created_at_set_on_init() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let board = load_board(&kando_dir).unwrap();
        assert!(board.created_at.is_some(), "init_board should set created_at");
        // Should be recent (within last 5 seconds)
        let age = (Utc::now() - board.created_at.unwrap()).num_seconds();
        assert!(age < 5, "created_at should be recent, but was {age}s ago");
    }

    #[test]
    fn test_board_created_at_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let board = load_board(&kando_dir).unwrap();
        let original_created_at = board.created_at.unwrap();

        // Save and reload — created_at should be preserved
        save_board(&kando_dir, &board).unwrap();
        let reloaded = load_board(&kando_dir).unwrap();
        assert!(reloaded.created_at.is_some());
        let delta = (reloaded.created_at.unwrap() - original_created_at).num_seconds().abs();
        assert!(delta <= 1, "created_at should roundtrip within 1 second, delta was {delta}s");
    }

    #[test]
    fn test_board_created_at_none_for_legacy_config() {
        // Simulate a legacy config.toml without created_at
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // Read config, remove created_at, write back
        let config_path = kando_dir.join("config.toml");
        let config_str = fs::read_to_string(&config_path).unwrap();
        let cleaned: String = config_str.lines()
            .filter(|line| !line.starts_with("created_at"))
            .collect::<Vec<_>>()
            .join("\n");
        fs::write(&config_path, cleaned).unwrap();

        let board = load_board(&kando_dir).unwrap();
        assert!(board.created_at.is_none(), "legacy board without created_at should load as None");
    }

    #[test]
    fn test_card_completed_roundtrip() {
        let mut card = Card::new("100".into(), "Completed card".into());
        let completed_time = Utc::now();
        card.completed = Some(completed_time);

        let serialized = serialize_card(&card);
        assert!(serialized.contains("completed = \""));

        let (fm, _body) = parse_frontmatter(&serialized).unwrap();
        let deserialized: Card = toml::from_str(&fm).unwrap();
        assert!(deserialized.completed.is_some());
        // Timestamps lose sub-second precision through serialization
        let delta = (deserialized.completed.unwrap() - completed_time).num_seconds().abs();
        assert!(delta <= 1, "completed timestamp should roundtrip within 1 second");
    }

    #[test]
    fn test_card_without_completed_deserializes_as_none() {
        let card = Card::new("101".into(), "Pending card".into());
        let serialized = serialize_card(&card);
        assert!(!serialized.contains("completed"));

        let (fm, _body) = parse_frontmatter(&serialized).unwrap();
        let deserialized: Card = toml::from_str(&fm).unwrap();
        assert!(deserialized.completed.is_none());
    }

    #[test]
    fn test_card_started_roundtrip() {
        let mut card = Card::new("102".into(), "Started card".into());
        let started_time = Utc::now();
        card.started = Some(started_time);

        let serialized = serialize_card(&card);
        assert!(serialized.contains("started = \""));

        let (fm, _body) = parse_frontmatter(&serialized).unwrap();
        let deserialized: Card = toml::from_str(&fm).unwrap();
        assert!(deserialized.started.is_some());
        let delta = (deserialized.started.unwrap() - started_time).num_seconds().abs();
        assert!(delta <= 1, "started timestamp should roundtrip within 1 second");
    }

    #[test]
    fn test_card_without_started_deserializes_as_none() {
        let card = Card::new("103".into(), "New card".into());
        let serialized = serialize_card(&card);
        assert!(!serialized.contains("started"));

        let (fm, _body) = parse_frontmatter(&serialized).unwrap();
        let deserialized: Card = toml::from_str(&fm).unwrap();
        assert!(deserialized.started.is_none());
    }

    #[test]
    fn test_purge_trash_keeps_entry_with_unparseable_date() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        // Create a trash entry with a garbage date
        let trash = kando_dir.join(".trash");
        fs::create_dir_all(&trash).unwrap();
        fs::write(trash.join("bad.md"), "---\nid = \"bad\"\ntitle = \"Bad date\"\n---\n").unwrap();
        let meta = TrashMeta {
            entries: vec![TrashEntry {
                id: "bad".into(),
                deleted: "not-a-date".into(),
                from_column: "backlog".into(),
                title: "Bad date".into(),
            }],
        };
        save_trash_meta(&kando_dir, &meta).unwrap();

        // Purge should keep the entry (unparseable date ⇒ keep, not delete)
        let purged = purge_trash(&kando_dir, 1).unwrap();
        assert!(purged.is_empty());
        assert_eq!(load_trash(&kando_dir).len(), 1);
    }

    #[test]
    fn test_trash_two_cards_restore_one() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");

        let mut board = load_board(&kando_dir).unwrap();
        board.columns[0].cards.push(Card::new("001".into(), "First".into()));
        board.columns[0].cards.push(Card::new("002".into(), "Second".into()));
        save_board(&kando_dir, &board).unwrap();

        let col_slug = board.columns[0].slug.clone();
        trash_card(&kando_dir, &col_slug, "001", "First").unwrap();
        trash_card(&kando_dir, &col_slug, "002", "Second").unwrap();

        // Both in trash
        assert_eq!(load_trash(&kando_dir).len(), 2);

        // Restore only the first
        restore_card(&kando_dir, "001", &col_slug).unwrap();

        // First is back, second still in trash
        let entries = load_trash(&kando_dir);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].id, "002");

        let restored = kando_dir.join("columns").join(&col_slug).join("001.md");
        assert!(restored.exists());
        let still_trashed = kando_dir.join(".trash/002.md");
        assert!(still_trashed.exists());
    }

    // ── validate_slug tests ──

    #[test]
    fn validate_slug_valid_simple() {
        assert!(validate_slug("backlog").is_ok());
    }

    #[test]
    fn validate_slug_valid_with_hyphens() {
        assert!(validate_slug("in-progress").is_ok());
    }

    #[test]
    fn validate_slug_valid_with_digits() {
        assert!(validate_slug("col1").is_ok());
        assert!(validate_slug("1col").is_ok());
    }

    #[test]
    fn validate_slug_empty_returns_err() {
        assert!(validate_slug("").is_err());
    }

    #[test]
    fn validate_slug_uppercase_returns_err() {
        assert!(validate_slug("MyColumn").is_err());
    }

    #[test]
    fn validate_slug_spaces_returns_err() {
        assert!(validate_slug("my column").is_err());
    }

    #[test]
    fn validate_slug_special_chars_returns_err() {
        assert!(validate_slug("col@name").is_err());
    }

    #[test]
    fn validate_slug_leading_hyphen_returns_err() {
        assert!(validate_slug("-col").is_err());
    }

    #[test]
    fn validate_slug_underscore_returns_err() {
        assert!(validate_slug("my_col").is_err());
    }

    // ── parse_frontmatter edge cases ──

    #[test]
    fn parse_frontmatter_empty_returns_none() {
        assert!(parse_frontmatter("").is_none());
    }

    #[test]
    fn parse_frontmatter_no_closing_delimiter() {
        assert!(parse_frontmatter("---\nid = \"1\"\ntitle = \"X\"\n").is_none());
    }

    #[test]
    fn parse_frontmatter_only_delimiters_returns_none() {
        // "---\n---\n" has no content between delimiters; parse requires "\n---" which
        // needs at least one line of frontmatter content.
        assert!(parse_frontmatter("---\n---\n").is_none());
    }

    #[test]
    fn parse_frontmatter_minimal_content() {
        let result = parse_frontmatter("---\nkey = \"val\"\n---\n");
        assert!(result.is_some());
        let (fm, body) = result.unwrap();
        assert!(fm.contains("key"));
        assert!(body.is_empty());
    }

    #[test]
    fn parse_frontmatter_no_opening_delimiter() {
        assert!(parse_frontmatter("id = \"1\"\n---\n").is_none());
    }

    // ── serialize_card edge cases ──

    #[test]
    fn serialize_card_blocked_roundtrip() {
        let mut card = Card::new("001".into(), "Blocked".into());
        card.blocked = true;
        let text = serialize_card(&card);
        assert!(text.contains("blocked = true"));
        // Roundtrip
        let (fm, _body) = parse_frontmatter(&text).unwrap();
        let loaded: Card = toml::from_str(&fm).unwrap();
        assert!(loaded.blocked);
    }

    #[test]
    fn serialize_card_with_assignees_roundtrip() {
        let mut card = Card::new("001".into(), "Team".into());
        card.assignees = vec!["alice".into(), "bob".into()];
        let text = serialize_card(&card);
        assert!(text.contains("assignees = "));
        let (fm, _body) = parse_frontmatter(&text).unwrap();
        let loaded: Card = toml::from_str(&fm).unwrap();
        assert_eq!(loaded.assignees, vec!["alice", "bob"]);
    }

    #[test]
    fn serialize_card_with_started_completed_roundtrip() {
        use chrono::TimeZone;
        let mut card = Card::new("001".into(), "Done".into());
        card.started = Some(Utc.with_ymd_and_hms(2025, 6, 1, 10, 0, 0).unwrap());
        card.completed = Some(Utc.with_ymd_and_hms(2025, 6, 10, 10, 0, 0).unwrap());
        let text = serialize_card(&card);
        assert!(text.contains("started = "));
        assert!(text.contains("completed = "));
        let (fm, _body) = parse_frontmatter(&text).unwrap();
        let loaded: Card = toml::from_str(&fm).unwrap();
        assert!(loaded.started.is_some());
        assert!(loaded.completed.is_some());
    }

    #[test]
    fn serialize_card_empty_body_no_trailing_content() {
        let card = Card::new("001".into(), "No body".into());
        let text = serialize_card(&card);
        // Should end right after the closing ---
        assert!(text.ends_with("---\n"));
    }

    #[test]
    fn serialize_card_body_gets_trailing_newline() {
        let mut card = Card::new("001".into(), "Has body".into());
        card.body = "Some text".into();
        let text = serialize_card(&card);
        assert!(text.ends_with("Some text\n"));
    }

    #[test]
    fn serialize_card_all_fields_roundtrip() {
        use chrono::TimeZone;
        let mut card = Card::new("999".into(), "Full card".into());
        card.priority = Priority::Urgent;
        card.tags = vec!["bug".into(), "critical".into()];
        card.assignees = vec!["alice".into()];
        card.blocked = true;
        card.started = Some(Utc.with_ymd_and_hms(2025, 5, 1, 0, 0, 0).unwrap());
        card.completed = Some(Utc.with_ymd_and_hms(2025, 6, 1, 0, 0, 0).unwrap());
        card.body = "Full description.\n\nMultiple paragraphs.".into();

        let text = serialize_card(&card);
        let (fm, body) = parse_frontmatter(&text).unwrap();
        let loaded: Card = toml::from_str(&fm).unwrap();
        assert_eq!(loaded.id, "999");
        assert_eq!(loaded.title, "Full card");
        assert_eq!(loaded.priority, Priority::Urgent);
        assert_eq!(loaded.tags, vec!["bug", "critical"]);
        assert_eq!(loaded.assignees, vec!["alice"]);
        assert!(loaded.blocked);
        assert!(loaded.started.is_some());
        assert!(loaded.completed.is_some());
        assert_eq!(body, card.body);
    }

    // -----------------------------------------------------------------------
    // LocalConfig / local.toml
    // -----------------------------------------------------------------------

    #[test]
    fn load_local_config_absent_file_returns_default() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(!cfg.focus_mode);
    }

    #[test]
    fn load_local_config_focus_mode_true() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "focus_mode = true\n").unwrap();
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(cfg.focus_mode);
    }

    #[test]
    fn load_local_config_focus_mode_false_explicit() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "focus_mode = false\n").unwrap();
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(!cfg.focus_mode);
    }

    #[test]
    fn load_local_config_invalid_toml_returns_err() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "focus_mode = !!!\n").unwrap();
        assert!(load_local_config(&kando_dir).is_err());
    }

    #[test]
    fn load_local_config_empty_file_uses_serde_default() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "").unwrap();
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(!cfg.focus_mode);
    }

    #[test]
    fn load_local_config_extra_fields_are_ignored() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "focus_mode = true\nunknown_future_key = 42\n").unwrap();
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(cfg.focus_mode);
    }

    #[test]
    fn load_local_config_nonexistent_kando_dir_returns_default() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando"); // never created
        let cfg = load_local_config(&kando_dir).unwrap();
        assert!(!cfg.focus_mode);
    }

    #[test]
    fn save_local_config_creates_file() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let cfg = crate::config::LocalConfig { focus_mode: true };
        save_local_config(&kando_dir, &cfg).unwrap();
        assert!(kando_dir.join("local.toml").exists());
        let loaded = load_local_config(&kando_dir).unwrap();
        assert!(loaded.focus_mode);
    }

    #[test]
    fn save_local_config_overwrites_existing() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join("local.toml"), "focus_mode = false\n").unwrap();
        save_local_config(&kando_dir, &crate::config::LocalConfig { focus_mode: true }).unwrap();
        assert!(load_local_config(&kando_dir).unwrap().focus_mode);
    }

    #[test]
    fn save_local_config_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let original = crate::config::LocalConfig { focus_mode: true };
        save_local_config(&kando_dir, &original).unwrap();
        let loaded = load_local_config(&kando_dir).unwrap();
        assert_eq!(loaded.focus_mode, original.focus_mode);
    }

    #[test]
    fn save_local_config_creates_gitignore_with_entry() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        save_local_config(&kando_dir, &crate::config::LocalConfig::default()).unwrap();
        let gitignore = fs::read_to_string(kando_dir.join(".gitignore")).unwrap();
        assert!(gitignore.lines().any(|l| l.trim() == "local.toml"));
    }

    #[test]
    fn save_local_config_gitignore_entry_not_duplicated() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        let cfg = crate::config::LocalConfig::default();
        save_local_config(&kando_dir, &cfg).unwrap();
        save_local_config(&kando_dir, &cfg).unwrap();
        let gitignore = fs::read_to_string(kando_dir.join(".gitignore")).unwrap();
        let count = gitignore.lines().filter(|l| l.trim() == "local.toml").count();
        assert_eq!(count, 1);
    }

    #[test]
    fn save_local_config_gitignore_entry_appended_to_existing_content() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::write(kando_dir.join(".gitignore"), "*.log\n").unwrap();
        save_local_config(&kando_dir, &crate::config::LocalConfig::default()).unwrap();
        let gitignore = fs::read_to_string(kando_dir.join(".gitignore")).unwrap();
        assert_eq!(gitignore, "*.log\nlocal.toml\n");
    }

    #[test]
    fn save_local_config_gitignore_separator_added_when_no_trailing_newline() {
        let dir = tempfile::tempdir().unwrap();
        init_board(dir.path(), "Test", None).unwrap();
        let kando_dir = dir.path().join(".kando");
        // Write without trailing newline
        fs::write(kando_dir.join(".gitignore"), "*.log").unwrap();
        save_local_config(&kando_dir, &crate::config::LocalConfig::default()).unwrap();
        let gitignore = fs::read_to_string(kando_dir.join(".gitignore")).unwrap();
        assert_eq!(gitignore, "*.log\nlocal.toml\n");
    }

    // ── json_escape tests ──

    #[test]
    fn json_escape_empty_string() {
        assert_eq!(super::json_escape(""), "\"\"");
    }

    #[test]
    fn json_escape_plain_ascii() {
        assert_eq!(super::json_escape("hello"), "\"hello\"");
    }

    #[test]
    fn json_escape_double_quote() {
        assert_eq!(super::json_escape("say \"hi\""), "\"say \\\"hi\\\"\"");
    }

    #[test]
    fn json_escape_backslash() {
        // One backslash → JSON-escaped as two backslashes, wrapped in quotes
        assert_eq!(super::json_escape("\\"), "\"\\\\\"");
    }

    #[test]
    fn json_escape_newline_cr_tab() {
        assert_eq!(super::json_escape("a\nb\rc\td"), "\"a\\nb\\rc\\td\"");
    }

    #[test]
    fn json_escape_control_char_x01() {
        assert_eq!(super::json_escape("\x01"), "\"\\u0001\"");
    }

    #[test]
    fn json_escape_nul_byte() {
        assert_eq!(super::json_escape("\x00"), "\"\\u0000\"");
    }

    #[test]
    fn json_escape_non_ascii_unicode_passthrough() {
        assert_eq!(super::json_escape("héllo"), "\"héllo\"");
        assert_eq!(super::json_escape("日本語"), "\"日本語\"");
        assert_eq!(super::json_escape("🎉"), "\"🎉\"");
    }

    #[test]
    fn json_escape_mixed_special_chars() {
        // tab + double-quote + backslash + newline
        assert_eq!(
            super::json_escape("\t\"a\\b\"\n"),
            "\"\\t\\\"a\\\\b\\\"\\n\""
        );
    }

    // ── append_activity / try_append_activity tests ──

    #[test]
    fn append_activity_creates_log_file() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "create", "42", "My Task", &[]);

        assert!(kando_dir.join("activity.log").exists());
    }

    #[test]
    fn append_activity_line_contains_required_fields() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "create", "42", "My Task", &[]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        let line = content.trim();
        assert!(line.starts_with('{'), "line should be a JSON object");
        assert!(line.ends_with('}'), "line should be a JSON object");
        assert!(line.contains("\"action\":\"create\""), "missing action");
        assert!(line.contains("\"id\":\"42\""), "missing id");
        assert!(line.contains("\"title\":\"My Task\""), "missing title");
        assert!(line.contains("\"ts\":\""), "missing ts");
    }

    #[test]
    fn append_activity_second_call_appends_new_line() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "create", "1", "First", &[]);
        append_activity(&kando_dir, "delete", "2", "Second", &[]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        let lines: Vec<&str> = content.lines().collect();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].contains("\"action\":\"create\""));
        assert!(lines[1].contains("\"action\":\"delete\""));
    }

    #[test]
    fn append_activity_extras_appear_in_output() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "move", "1", "Task", &[("from", "Backlog"), ("to", "Done")]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        assert!(content.contains("\"from\":\"Backlog\""));
        assert!(content.contains("\"to\":\"Done\""));
    }

    #[test]
    fn append_activity_special_chars_in_title_escaped() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "create", "1", "Task \"with\" quotes", &[]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        assert!(content.contains("\"title\":\"Task \\\"with\\\" quotes\""));
    }

    #[test]
    fn append_activity_io_error_is_silently_swallowed() {
        // Non-existent parent directory — should not panic
        let dir = tempfile::tempdir().unwrap();
        let nonexistent = dir.path().join("no-such-dir").join(".kando");
        append_activity(&nonexistent, "create", "1", "Task", &[]);
        // If we reach here without panicking the test passes
    }

    #[test]
    fn append_activity_empty_extras_produces_four_keys() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "action", "id-val", "title-val", &[]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        let line = content.trim();
        let key_count = ["\"ts\":", "\"action\":", "\"id\":", "\"title\":"]
            .iter()
            .filter(|&&k| line.contains(k))
            .count();
        assert_eq!(key_count, 4, "all four required fields (ts, action, id, title) should be present");
    }

    #[test]
    fn append_activity_timestamp_is_iso8601() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        append_activity(&kando_dir, "test", "1", "T", &[]);

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        // Extract the value of "ts":"..."
        let prefix = "\"ts\":\"";
        let ts_start = content.find(prefix).unwrap() + prefix.len();
        let ts_end = ts_start + content[ts_start..].find('"').unwrap();
        let ts = &content[ts_start..ts_end];
        assert_eq!(ts.len(), 20, "timestamp should be YYYY-MM-DDTHH:MM:SSZ");
        assert!(ts.contains('T'), "timestamp should contain T separator");
        assert!(ts.ends_with('Z'), "timestamp should end with Z");
    }

    // ── append_activity integration: one entry per auto-closed card ──

    #[test]
    fn append_activity_auto_close_entries_per_card() {
        let dir = tempfile::tempdir().unwrap();
        let kando_dir = dir.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();

        // Simulate what handle_auto_close does: one append_activity per closed card
        let cards = [("1", "Alpha", "backlog"), ("2", "Beta", "in-progress")];
        for (id, title, from) in &cards {
            append_activity(&kando_dir, "auto-close", id, title, &[("from", from)]);
        }

        let content = fs::read_to_string(kando_dir.join("activity.log")).unwrap();
        let lines: Vec<&str> = content.lines().collect();
        assert_eq!(lines.len(), 2, "one log entry per auto-closed card");
        assert!(lines[0].contains("\"action\":\"auto-close\""));
        assert!(lines[0].contains("\"id\":\"1\""));
        assert!(lines[0].contains("\"from\":\"backlog\""));
        assert!(lines[1].contains("\"action\":\"auto-close\""));
        assert!(lines[1].contains("\"id\":\"2\""));
        assert!(lines[1].contains("\"from\":\"in-progress\""));
    }
}
