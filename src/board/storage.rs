use std::fs;
use std::path::{Path, PathBuf};

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
        },
        columns: column_configs,
    };
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
}
