pub mod age;
pub mod metrics;
pub mod storage;
pub mod sync;

use chrono::{DateTime, Utc};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use serde::{Deserialize, Serialize};

/// The top-level board containing all columns and cards.
#[derive(Debug, Clone)]
pub struct Board {
    pub name: String,
    pub next_card_id: u32,
    pub policies: Policies,
    pub sync_branch: Option<String>,
    pub tutorial_shown: bool,
    pub nerd_font: bool,
    /// When the board was created. None for legacy boards.
    pub created_at: Option<DateTime<Utc>>,
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
    /// Trash entries older than this many days are permanently purged. 0 = never.
    #[serde(default = "default_trash_purge_days")]
    pub trash_purge_days: u32,
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
fn default_trash_purge_days() -> u32 {
    30
}

impl Default for Policies {
    fn default() -> Self {
        Self {
            auto_close_days: default_auto_close_days(),
            auto_close_target: default_auto_close_target(),
            bubble_up_days: default_bubble_up_days(),
            trash_purge_days: default_trash_purge_days(),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
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
    /// When the card first moved past backlog (commitment point). `None` for cards still in backlog.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub started: Option<DateTime<Utc>>,
    /// When the card was moved to the "done" column. `None` if not yet completed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub completed: Option<DateTime<Utc>>,
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
            started: None,
            completed: None,
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
        // Set started when card first moves past backlog (column 0).
        // Commitment is a one-time event: never cleared once set.
        if card.started.is_none() && to_col > 0 {
            card.started = Some(Utc::now());
        }
        // Set or clear completed based on target column
        if self.columns[to_col].slug == "done" {
            if card.completed.is_none() {
                card.completed = Some(Utc::now());
            }
        } else {
            card.completed = None;
        }
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

// ---------------------------------------------------------------------------
// Card filtering
// ---------------------------------------------------------------------------

/// Check whether a card is visible under the current set of filters.
///
/// `active_filter` is the text/fuzzy search (from `/`).
/// `tag_filters` / `assignee_filters` are the picker-based filters.
/// Text search takes precedence over picker filters.
pub fn card_is_visible(
    card: &Card,
    active_filter: Option<&str>,
    tag_filters: &[String],
    assignee_filters: &[String],
    matcher: &SkimMatcherV2,
) -> bool {
    if let Some(filter) = active_filter {
        card_matches_filter(card, filter, matcher)
    } else {
        let tag_ok =
            tag_filters.is_empty() || card.tags.iter().any(|t| tag_filters.contains(t));
        let assignee_ok = assignee_filters.is_empty()
            || card.assignees.iter().any(|a| assignee_filters.contains(a));
        tag_ok && assignee_ok
    }
}

/// Check whether a card matches a multi-term fuzzy filter.
///
/// The filter string is split on whitespace into individual terms.
/// Each term must fuzzy-match at least one card field (title, any tag,
/// or any assignee) — OR across fields, AND across terms.
///
/// Special prefixes:
/// - `@` is stripped so `@alice` matches the assignee `alice`
/// - `!` negates: the card must NOT match that term in any field
pub fn card_matches_filter(card: &Card, filter: &str, matcher: &SkimMatcherV2) -> bool {
    let terms: Vec<&str> = filter.split_whitespace().collect();
    if terms.is_empty() {
        return true;
    }

    for term in &terms {
        let (negated, pattern) = if let Some(rest) = term.strip_prefix('!') {
            (true, rest)
        } else {
            (false, *term)
        };

        // Strip leading @ for assignee-friendly matching
        let bare = pattern.trim_start_matches('@');
        if bare.is_empty() {
            continue;
        }

        let matches_any_field = matcher.fuzzy_match(&card.title, bare).is_some()
            || card
                .tags
                .iter()
                .any(|t| matcher.fuzzy_match(t, bare).is_some())
            || card
                .assignees
                .iter()
                .any(|a| matcher.fuzzy_match(a, bare).is_some());

        if negated && matches_any_field {
            return false;
        }
        if !negated && !matches_any_field {
            return false;
        }
    }

    true
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_board_with_done() -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 10,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            nerd_font: false,
            created_at: None,
            columns: vec![
                Column {
                    slug: "backlog".into(),
                    name: "Backlog".into(),
                    order: 0,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![Card::new("1".into(), "Card A".into())],
                },
                Column {
                    slug: "done".into(),
                    name: "Done".into(),
                    order: 1,
                    wip_limit: None,
                    hidden: false,
                    cards: Vec::new(),
                },
            ],
        }
    }

    #[test]
    fn move_to_done_sets_completed() {
        let mut board = test_board_with_done();
        assert!(board.columns[0].cards[0].completed.is_none());

        board.move_card(0, 0, 1);
        let card = &board.columns[1].cards[0];
        assert!(card.completed.is_some(), "completed should be set when moving to done");
    }

    #[test]
    fn move_away_from_done_clears_completed() {
        let mut board = test_board_with_done();
        // First move to done
        board.move_card(0, 0, 1);
        assert!(board.columns[1].cards[0].completed.is_some());

        // Move back to backlog
        board.move_card(1, 0, 0);
        let card = &board.columns[0].cards[0];
        assert!(card.completed.is_none(), "completed should be cleared when moving away from done");
    }

    #[test]
    fn move_to_done_preserves_existing_completed() {
        let mut board = test_board_with_done();
        // Manually set completed to a specific time
        let original_time = Utc::now() - chrono::TimeDelta::days(5);
        board.columns[0].cards[0].completed = Some(original_time);

        board.move_card(0, 0, 1);
        let card = &board.columns[1].cards[0];
        assert_eq!(
            card.completed, Some(original_time),
            "moving to done when already completed should keep original timestamp"
        );
    }

    #[test]
    fn move_between_non_done_columns_leaves_completed_none() {
        let mut board = Board {
            name: "Test".into(),
            next_card_id: 10,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            nerd_font: false,
            created_at: None,
            columns: vec![
                Column {
                    slug: "backlog".into(),
                    name: "Backlog".into(),
                    order: 0,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![Card::new("1".into(), "Card A".into())],
                },
                Column {
                    slug: "in-progress".into(),
                    name: "In Progress".into(),
                    order: 1,
                    wip_limit: None,
                    hidden: false,
                    cards: Vec::new(),
                },
                Column {
                    slug: "done".into(),
                    name: "Done".into(),
                    order: 2,
                    wip_limit: None,
                    hidden: false,
                    cards: Vec::new(),
                },
            ],
        };

        // Move backlog → in-progress (neither is "done")
        board.move_card(0, 0, 1);
        let card = &board.columns[1].cards[0];
        assert!(
            card.completed.is_none(),
            "moving between non-done columns should not set completed"
        );
    }

    // ── Started (commitment point) tests ──

    fn three_column_board() -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 10,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            nerd_font: false,
            created_at: None,
            columns: vec![
                Column {
                    slug: "backlog".into(),
                    name: "Backlog".into(),
                    order: 0,
                    wip_limit: None,
                    hidden: false,
                    cards: vec![Card::new("1".into(), "Card A".into())],
                },
                Column {
                    slug: "in-progress".into(),
                    name: "In Progress".into(),
                    order: 1,
                    wip_limit: None,
                    hidden: false,
                    cards: Vec::new(),
                },
                Column {
                    slug: "done".into(),
                    name: "Done".into(),
                    order: 2,
                    wip_limit: None,
                    hidden: false,
                    cards: Vec::new(),
                },
            ],
        }
    }

    #[test]
    fn move_past_backlog_sets_started() {
        let mut board = three_column_board();
        assert!(board.columns[0].cards[0].started.is_none());

        board.move_card(0, 0, 1); // backlog → in-progress
        let card = &board.columns[1].cards[0];
        assert!(card.started.is_some(), "started should be set when moving past backlog");
    }

    #[test]
    fn move_within_active_preserves_started() {
        let mut board = three_column_board();
        board.move_card(0, 0, 1); // backlog → in-progress
        let original_started = board.columns[1].cards[0].started;

        // Add a review column and move there
        board.columns.insert(2, Column {
            slug: "review".into(),
            name: "Review".into(),
            order: 2,
            wip_limit: None,
            hidden: false,
            cards: Vec::new(),
        });
        board.move_card(1, 0, 2); // in-progress → review
        let card = &board.columns[2].cards[0];
        assert_eq!(card.started, original_started, "started should be preserved when moving within active columns");
    }

    #[test]
    fn move_back_to_backlog_preserves_started() {
        let mut board = three_column_board();
        board.move_card(0, 0, 1); // backlog → in-progress
        assert!(board.columns[1].cards[0].started.is_some());

        board.move_card(1, 0, 0); // in-progress → backlog
        let card = &board.columns[0].cards[0];
        assert!(card.started.is_some(), "started should NOT be cleared when moving back to backlog");
    }

    #[test]
    fn move_to_done_sets_started_if_none() {
        let mut board = test_board_with_done();
        assert!(board.columns[0].cards[0].started.is_none());

        board.move_card(0, 0, 1); // backlog → done
        let card = &board.columns[1].cards[0];
        assert!(card.started.is_some(), "started should be set when moving from backlog to done");
        assert!(card.completed.is_some(), "completed should also be set");
    }

    #[test]
    fn card_in_backlog_has_no_started() {
        let card = Card::new("1".into(), "Test".into());
        assert!(card.started.is_none());
    }
}
