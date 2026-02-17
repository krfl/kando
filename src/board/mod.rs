pub mod age;
pub mod storage;
pub mod sync;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// The top-level board containing all columns and cards.
#[derive(Debug, Clone)]
pub struct Board {
    pub name: String,
    pub next_card_id: u32,
    pub policies: Policies,
    pub sync_branch: Option<String>,
    pub tutorial_shown: bool,
    pub columns: Vec<Column>,
}

/// Board-level policies for hygiene automation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policies {
    /// Cards untouched for this many days get auto-closed. 0 = disabled.
    #[serde(default = "default_auto_close_days")]
    pub auto_close_days: u32,
    /// Which column auto-closed cards move to.
    #[serde(default = "default_auto_close_target")]
    pub auto_close_target: String,
    /// Cards untouched for this many days get a visual warning.
    #[serde(default = "default_bubble_up_days")]
    pub bubble_up_days: u32,
}

fn default_auto_close_days() -> u32 {
    30
}
fn default_auto_close_target() -> String {
    "archive".to_string()
}
fn default_bubble_up_days() -> u32 {
    7
}

impl Default for Policies {
    fn default() -> Self {
        Self {
            auto_close_days: default_auto_close_days(),
            auto_close_target: default_auto_close_target(),
            bubble_up_days: default_bubble_up_days(),
        }
    }
}

/// A single kanban column (not "list").
#[derive(Debug, Clone)]
pub struct Column {
    pub slug: String,
    pub name: String,
    pub order: u32,
    pub wip_limit: Option<u32>,
    pub hidden: bool,
    pub cards: Vec<Card>,
}

/// Priority levels for cards.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum Priority {
    Low,
    #[default]
    Normal,
    High,
    Urgent,
}

impl Priority {
    pub const ALL: [Priority; 4] = [Self::Low, Self::Normal, Self::High, Self::Urgent];

    pub fn next(self) -> Self {
        match self {
            Self::Low => Self::Normal,
            Self::Normal => Self::High,
            Self::High => Self::Urgent,
            Self::Urgent => Self::Low,
        }
    }

    /// Sort key: lower = higher priority (sorts first).
    pub fn sort_key(self) -> u8 {
        match self {
            Self::Urgent => 0,
            Self::High => 1,
            Self::Normal => 2,
            Self::Low => 3,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Low => "low",
            Self::Normal => "normal",
            Self::High => "high",
            Self::Urgent => "urgent",
        }
    }

    pub fn symbol(&self) -> Option<&'static str> {
        match self {
            Self::Low => Some("\u{f063}"),      // nf-fa-arrow_down
            Self::Normal => None,
            Self::High => Some("\u{f0e7}"),     // nf-fa-bolt
            Self::Urgent => Some("\u{f06d}"),   // nf-fa-fire
        }
    }
}

impl std::str::FromStr for Priority {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "low" => Ok(Self::Low),
            "normal" => Ok(Self::Normal),
            "high" => Ok(Self::High),
            "urgent" => Ok(Self::Urgent),
            other => Err(format!("unknown priority '{other}': use low, normal, high, urgent")),
        }
    }
}

impl std::fmt::Display for Priority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A single kanban card (not "task" or "item").
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Card {
    pub id: String,
    pub title: String,
    pub created: DateTime<Utc>,
    pub updated: DateTime<Utc>,
    #[serde(default)]
    pub priority: Priority,
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(default)]
    pub assignees: Vec<String>,
    #[serde(default)]
    pub blocked: bool,
    /// The markdown body (not serialized into frontmatter).
    #[serde(skip)]
    pub body: String,
}

impl Card {
    pub fn new(id: String, title: String) -> Self {
        let now = Utc::now();
        Self {
            id,
            title,
            created: now,
            updated: now,
            priority: Priority::default(),
            tags: Vec::new(),
            assignees: Vec::new(),
            blocked: false,
            body: String::new(),
        }
    }

    /// Touch the card, updating its `updated` timestamp.
    pub fn touch(&mut self) {
        self.updated = Utc::now();
    }
}

impl Board {
    /// Generate the next card ID and increment the counter.
    pub fn next_card_id(&mut self) -> String {
        let n = self.next_card_id;
        self.next_card_id += 1;
        n.to_string()
    }

    /// Find which column a card is in and its index.
    pub fn find_card(&self, card_id: &str) -> Option<(usize, usize)> {
        for (col_idx, col) in self.columns.iter().enumerate() {
            for (card_idx, card) in col.cards.iter().enumerate() {
                if card.id == card_id {
                    return Some((col_idx, card_idx));
                }
            }
        }
        None
    }

    /// Move a card from one column to another.
    pub fn move_card(&mut self, from_col: usize, card_idx: usize, to_col: usize) {
        if from_col >= self.columns.len() || to_col >= self.columns.len() {
            return;
        }
        if card_idx >= self.columns[from_col].cards.len() {
            return;
        }
        let mut card = self.columns[from_col].cards.remove(card_idx);
        card.touch();
        self.columns[to_col].cards.push(card);
    }

    /// Collect all unique tags across the board with counts.
    pub fn all_tags(&self) -> Vec<(String, usize)> {
        let mut counts: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
        for col in &self.columns {
            for card in &col.cards {
                for tag in &card.tags {
                    *counts.entry(tag.as_str()).or_insert(0) += 1;
                }
            }
        }
        let mut tags: Vec<_> = counts
            .into_iter()
            .map(|(tag, count)| (tag.to_string(), count))
            .collect();
        tags.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        tags
    }

    /// Collect all unique assignees across all cards, with counts.
    pub fn all_assignees(&self) -> Vec<(String, usize)> {
        let mut counts: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
        for col in &self.columns {
            for card in &col.cards {
                for assignee in &card.assignees {
                    *counts.entry(assignee.as_str()).or_insert(0) += 1;
                }
            }
        }
        let mut assignees: Vec<_> = counts
            .into_iter()
            .map(|(name, count)| (name.to_string(), count))
            .collect();
        assignees.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        assignees
    }
}

impl Column {
    /// Sort cards by priority (urgent first) then by updated timestamp (most recent first).
    pub fn sort_cards(&mut self) {
        self.cards
            .sort_by(|a, b| {
                a.priority
                    .sort_key()
                    .cmp(&b.priority.sort_key())
                    .then(b.updated.cmp(&a.updated))
            });
    }

    /// Whether this column is at or over its WIP limit.
    pub fn is_over_wip_limit(&self) -> bool {
        self.wip_limit
            .is_some_and(|limit| self.cards.len() as u32 >= limit)
    }

}
