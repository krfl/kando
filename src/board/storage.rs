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

    Ok(Board {
        name: config.board.name,
        next_card_id: config.board.next_card_id,
        policies: config.board.policies,
        sync_branch: config.board.sync_branch,
        tutorial_shown: config.board.tutorial_shown,
        nerd_font: config.board.nerd_font,
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
}
