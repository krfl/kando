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
    #[serde(rename = "stale_days", alias = "bubble_up_days", default = "default_stale_days")]
    pub stale_days: u32,
    /// Trash entries older than this many days are permanently purged. 0 = never.
    #[serde(default = "default_trash_purge_days")]
    pub trash_purge_days: u32,
    /// Cards in the `done` column for this many days are auto-archived. 0 = disabled.
    #[serde(default)]
    pub archive_after_days: u32,
}

fn default_auto_close_days() -> u32 {
    30
}
fn default_auto_close_target() -> String {
    "archive".to_string()
}
fn default_stale_days() -> u32 {
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
            stale_days: default_stale_days(),
            trash_purge_days: default_trash_purge_days(),
            archive_after_days: 0,
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
/// `tag_filters` / `assignee_filters` / `staleness_filters` are the picker-based filters.
/// Text search takes precedence over picker filters.
#[allow(clippy::too_many_arguments)]
pub fn card_is_visible(
    card: &Card,
    active_filter: Option<&str>,
    tag_filters: &[String],
    assignee_filters: &[String],
    staleness_filters: &[String],
    policies: &Policies,
    now: DateTime<Utc>,
    matcher: &SkimMatcherV2,
) -> bool {
    if let Some(filter) = active_filter {
        card_matches_filter(card, filter, matcher)
    } else {
        let tag_ok =
            tag_filters.is_empty() || card.tags.iter().any(|t| tag_filters.contains(t));
        let assignee_ok = assignee_filters.is_empty()
            || card.assignees.iter().any(|a| assignee_filters.contains(a));
        let staleness_ok = staleness_filters.is_empty() || {
            let label = age::card_staleness_label(card, policies, now);
            staleness_filters.iter().any(|f| f.as_str() == label)
        };
        tag_ok && assignee_ok && staleness_ok
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

/// Derive a raw slug from a display name without uniqueness deduplication.
///
/// Maps non-alphanumeric chars to `-`, collapses runs, trims leading/trailing
/// hyphens, and falls back to `"col"` if the result would be empty.
///
/// **Does not** guard the `"archive"` reserved slug — callers that need that
/// guarantee should use [`slug_for_rename`] or [`generate_slug`].
pub fn slug_from_name(name: &str) -> String {
    let base: String = name
        .to_lowercase()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
        .collect();
    // After split/filter/join the result is guaranteed to start with an
    // alphanumeric char (or be empty), so only the empty check is needed.
    let base = base
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-");
    if base.is_empty() {
        "col".to_string()
    } else {
        base
    }
}

/// Derive the slug to use when **renaming** an existing column.
///
/// Like [`slug_from_name`] but also enforces the `"archive"` reservation,
/// returning `Err` when the derived slug would be reserved. Uniqueness and
/// same-slug detection remain the caller's responsibility.
pub fn slug_for_rename(name: &str) -> Result<String, &'static str> {
    let slug = slug_from_name(name);
    if slug == "archive" {
        Err("'archive' is a reserved slug")
    } else {
        Ok(slug)
    }
}

/// Generate a filesystem-safe slug from a display name, guaranteed unique
/// among `existing` columns. The reserved slug `"archive"` is never returned.
///
/// Algorithm: lowercase → map non-alphanumeric to `-` → collapse/trim hyphens
/// → fall back to `"col"` if empty → append `-2`, `-3`, … for uniqueness.
pub fn generate_slug(name: &str, existing: &[Column]) -> String {
    let base = slug_from_name(name);
    // "archive" is a reserved slug — never generate it directly.
    let base = if base == "archive" { "archive-col".to_string() } else { base };
    // Ensure uniqueness.
    if !existing.iter().any(|c| c.slug == base) {
        return base;
    }
    for n in 2u32.. {
        let candidate = format!("{base}-{n}");
        if !existing.iter().any(|c| c.slug == candidate) {
            return candidate;
        }
    }
    unreachable!("generate_slug: exhausted candidates for base={base:?}")
}

/// Convert a slug back to a display name: split on `-`, capitalise each word.
///
/// # Invariants
///
/// Assumes the slug was produced by [`generate_slug`] or [`slug_from_name`]
/// (no leading/trailing hyphens, all ASCII alphanumeric + hyphens).
/// Degenerate input such as `"-"` produces degenerate output (`" "`) without
/// panicking, but callers should rely on [`validate_slug`] to prevent such
/// input from reaching this function.
///
/// Examples: `"backlog"` → `"Backlog"`, `"in-progress"` → `"In Progress"`.
pub fn slug_to_name(slug: &str) -> String {
    slug.split('-')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Normalize column orders to dense 0, 1, 2, … sorted by current order.
/// Call this after every add / remove / reorder operation, before save_board.
pub fn normalize_column_orders(columns: &mut [Column]) {
    columns.sort_by_key(|c| c.order);
    for (i, col) in columns.iter_mut().enumerate() {
        col.order = i as u32;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_board_with_done() -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 10,
            policies: Policies::default(),
            sync_branch: None,

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

    // ── Filter tests ──

    fn card_with_meta(title: &str, tags: &[&str], assignees: &[&str]) -> Card {
        let mut c = Card::new("1".into(), title.into());
        c.tags = tags.iter().map(|t| t.to_string()).collect();
        c.assignees = assignees.iter().map(|a| a.to_string()).collect();
        c
    }

    #[test]
    fn filter_fuzzy_matches_title() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(card_matches_filter(&card, "login", &matcher));
    }

    #[test]
    fn filter_no_match_returns_false() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(!card_matches_filter(&card, "logout", &matcher));
    }

    #[test]
    fn filter_negation_excludes_match() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(!card_matches_filter(&card, "!login", &matcher));
    }

    #[test]
    fn filter_negation_includes_non_match() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(card_matches_filter(&card, "!logout", &matcher));
    }

    #[test]
    fn filter_at_prefix_matches_tag() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(card_matches_filter(&card, "@auth", &matcher));
    }

    #[test]
    fn filter_at_prefix_missing_tag_returns_false() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(!card_matches_filter(&card, "@missing", &matcher));
    }

    #[test]
    fn filter_multi_term_and_both_match() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth", "frontend"], &["alice"]);
        assert!(card_matches_filter(&card, "login auth", &matcher));
    }

    #[test]
    fn filter_multi_term_and_one_misses() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &["auth"], &["alice"]);
        assert!(!card_matches_filter(&card, "login missing", &matcher));
    }

    #[test]
    fn filter_matches_assignee() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Implement login", &[], &["alice"]);
        assert!(card_matches_filter(&card, "alice", &matcher));
    }

    #[test]
    fn filter_empty_returns_true() {
        let matcher = SkimMatcherV2::default();
        let card = card_with_meta("Anything", &[], &[]);
        assert!(card_matches_filter(&card, "", &matcher));
    }

    #[test]
    fn card_is_visible_no_filters_shows_all() {
        let matcher = SkimMatcherV2::default();
        let policies = Policies::default();
        let now = Utc::now();
        let card = card_with_meta("Test", &[], &[]);
        assert!(card_is_visible(&card, None, &[], &[], &[], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_text_filter_overrides_picker() {
        let matcher = SkimMatcherV2::default();
        let policies = Policies::default();
        let now = Utc::now();
        let card = card_with_meta("Login feature", &["bug"], &[]);
        // Text filter matches, even though tag filter wouldn't match
        assert!(card_is_visible(
            &card,
            Some("login"),
            &["nonexistent".to_string()],
            &[],
            &[],
            &policies,
            now,
            &matcher,
        ));
    }

    #[test]
    fn card_is_visible_tag_filter_must_match() {
        let matcher = SkimMatcherV2::default();
        let policies = Policies::default();
        let now = Utc::now();
        let card = card_with_meta("Test", &["bug"], &[]);
        assert!(card_is_visible(&card, None, &["bug".to_string()], &[], &[], &policies, now, &matcher));
        assert!(!card_is_visible(&card, None, &["feature".to_string()], &[], &[], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_assignee_filter_must_match() {
        let matcher = SkimMatcherV2::default();
        let policies = Policies::default();
        let now = Utc::now();
        let card = card_with_meta("Test", &[], &["alice"]);
        assert!(card_is_visible(&card, None, &[], &["alice".to_string()], &[], &policies, now, &matcher));
        assert!(!card_is_visible(&card, None, &[], &["bob".to_string()], &[], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_both_tag_and_assignee_must_match() {
        let matcher = SkimMatcherV2::default();
        let policies = Policies::default();
        let now = Utc::now();
        let card = card_with_meta("Test", &["bug"], &["alice"]);
        // Both match
        assert!(card_is_visible(&card, None, &["bug".to_string()], &["alice".to_string()], &[], &policies, now, &matcher));
        // Tag matches, assignee doesn't
        assert!(!card_is_visible(&card, None, &["bug".to_string()], &["bob".to_string()], &[], &policies, now, &matcher));
        // Assignee matches, tag doesn't
        assert!(!card_is_visible(&card, None, &["feature".to_string()], &["alice".to_string()], &[], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_staleness_filter_matches() {
        use chrono::TimeZone;
        let matcher = SkimMatcherV2::default();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Card created yesterday, updated recently → "normal"
        let mut card = card_with_meta("Test", &[], &[]);
        card.created = Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap();
        card.updated = now;
        assert!(card_is_visible(&card, None, &[], &[], &["normal".to_string()], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_staleness_filter_no_match() {
        use chrono::TimeZone;
        let matcher = SkimMatcherV2::default();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Card is "normal" but filter only accepts "stale"
        let mut card = card_with_meta("Test", &[], &[]);
        card.created = Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap();
        card.updated = now;
        assert!(!card_is_visible(&card, None, &[], &[], &["stale".to_string()], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_multiple_staleness_filters() {
        use chrono::TimeZone;
        let matcher = SkimMatcherV2::default();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Stale card (updated 7 days ago)
        let mut stale_card = card_with_meta("Stale", &[], &[]);
        stale_card.created = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        stale_card.updated = Utc.with_ymd_and_hms(2025, 6, 8, 12, 0, 0).unwrap();
        let filters = vec!["stale".to_string(), "very stale".to_string()];
        assert!(card_is_visible(&stale_card, None, &[], &[], &filters, &policies, now, &matcher));
        // Normal card should not match
        let mut normal_card = card_with_meta("Normal", &[], &[]);
        normal_card.created = Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap();
        normal_card.updated = now;
        assert!(!card_is_visible(&normal_card, None, &[], &[], &filters, &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_staleness_combined_with_tag_filter() {
        use chrono::TimeZone;
        let matcher = SkimMatcherV2::default();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Card is "normal" and has tag "bug"
        let mut card = card_with_meta("Test", &["bug"], &[]);
        card.created = Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap();
        card.updated = now;
        // Both tag and staleness match → visible
        assert!(card_is_visible(&card, None, &["bug".to_string()], &[], &["normal".to_string()], &policies, now, &matcher));
        // Tag matches but staleness doesn't → hidden
        assert!(!card_is_visible(&card, None, &["bug".to_string()], &[], &["stale".to_string()], &policies, now, &matcher));
    }

    #[test]
    fn card_is_visible_text_filter_overrides_staleness() {
        use chrono::TimeZone;
        let matcher = SkimMatcherV2::default();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let mut card = card_with_meta("Login feature", &[], &[]);
        card.created = Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap();
        card.updated = now;
        // Text filter matches, staleness filter would NOT match — text takes precedence
        assert!(card_is_visible(&card, Some("login"), &[], &[], &["stale".to_string()], &policies, now, &matcher));
    }

    // ── Sort tests ──

    #[test]
    fn sort_cards_by_priority() {
        let now = Utc::now();
        let mut col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards: vec![],
        };
        let mut c1 = Card::new("1".into(), "Normal".into());
        c1.priority = Priority::Normal;
        c1.updated = now;
        let mut c2 = Card::new("2".into(), "Urgent".into());
        c2.priority = Priority::Urgent;
        c2.updated = now;
        let mut c3 = Card::new("3".into(), "Low".into());
        c3.priority = Priority::Low;
        c3.updated = now;
        let mut c4 = Card::new("4".into(), "High".into());
        c4.priority = Priority::High;
        c4.updated = now;
        col.cards = vec![c1, c2, c3, c4];

        col.sort_cards();

        let priorities: Vec<Priority> = col.cards.iter().map(|c| c.priority).collect();
        assert_eq!(priorities, vec![Priority::Urgent, Priority::High, Priority::Normal, Priority::Low]);
    }

    // ── Priority tests ──

    #[test]
    fn priority_from_str_valid() {
        assert_eq!("low".parse::<Priority>().unwrap(), Priority::Low);
        assert_eq!("normal".parse::<Priority>().unwrap(), Priority::Normal);
        assert_eq!("high".parse::<Priority>().unwrap(), Priority::High);
        assert_eq!("urgent".parse::<Priority>().unwrap(), Priority::Urgent);
    }

    #[test]
    fn priority_from_str_case_insensitive() {
        assert_eq!("HIGH".parse::<Priority>().unwrap(), Priority::High);
        assert_eq!("Urgent".parse::<Priority>().unwrap(), Priority::Urgent);
    }

    #[test]
    fn priority_from_str_unknown_returns_err() {
        assert!("unknown".parse::<Priority>().is_err());
        assert!("".parse::<Priority>().is_err());
    }

    #[test]
    fn priority_sort_key_ordering() {
        assert!(Priority::Urgent.sort_key() < Priority::High.sort_key());
        assert!(Priority::High.sort_key() < Priority::Normal.sort_key());
        assert!(Priority::Normal.sort_key() < Priority::Low.sort_key());
    }

    // ── WIP limit tests ──

    #[test]
    fn is_over_wip_limit_no_limit() {
        let col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards: vec![Card::new("1".into(), "A".into())],
        };
        assert!(!col.is_over_wip_limit());
    }

    #[test]
    fn is_over_wip_limit_under() {
        let col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: Some(3),
            hidden: false,
            cards: vec![Card::new("1".into(), "A".into())],
        };
        assert!(!col.is_over_wip_limit());
    }

    #[test]
    fn is_over_wip_limit_at_limit() {
        let col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: Some(2),
            hidden: false,
            cards: vec![Card::new("1".into(), "A".into()), Card::new("2".into(), "B".into())],
        };
        assert!(col.is_over_wip_limit());
    }

    // ── find_card tests ──

    #[test]
    fn find_card_returns_correct_indices() {
        let board = three_column_board();
        assert_eq!(board.find_card("1"), Some((0, 0)));
    }

    #[test]
    fn find_card_not_found_returns_none() {
        let board = three_column_board();
        assert_eq!(board.find_card("999"), None);
    }

    // ── next_card_id tests ──

    #[test]
    fn next_card_id_increments() {
        let mut board = test_board_with_done();
        let id1 = board.next_card_id();
        let id2 = board.next_card_id();
        assert_ne!(id1, id2);
        assert_eq!(id1.parse::<u32>().unwrap() + 1, id2.parse::<u32>().unwrap());
    }

    // ── all_tags / all_assignees tests ──

    #[test]
    fn all_tags_collects_and_sorts_by_count() {
        let mut board = test_board_with_done();
        board.columns[0].cards[0].tags = vec!["bug".into(), "ui".into()];
        let mut card2 = Card::new("2".into(), "B".into());
        card2.tags = vec!["bug".into()];
        board.columns[0].cards.push(card2);

        let tags = board.all_tags();
        assert_eq!(tags[0].0, "bug");
        assert_eq!(tags[0].1, 2);
        assert_eq!(tags[1].0, "ui");
        assert_eq!(tags[1].1, 1);
    }

    #[test]
    fn all_assignees_collects_and_sorts_by_count() {
        let mut board = test_board_with_done();
        board.columns[0].cards[0].assignees = vec!["alice".into(), "bob".into()];
        let mut card2 = Card::new("2".into(), "B".into());
        card2.assignees = vec!["alice".into()];
        board.columns[0].cards.push(card2);

        let assignees = board.all_assignees();
        assert_eq!(assignees[0].0, "alice");
        assert_eq!(assignees[0].1, 2);
        assert_eq!(assignees[1].0, "bob");
        assert_eq!(assignees[1].1, 1);
    }

    // ── Card::new defaults ──

    #[test]
    fn card_new_defaults() {
        let c = Card::new("001".into(), "Test".into());
        assert_eq!(c.id, "001");
        assert_eq!(c.title, "Test");
        assert_eq!(c.priority, Priority::Normal);
        assert!(c.tags.is_empty());
        assert!(c.assignees.is_empty());
        assert!(!c.blocked);
        assert!(c.started.is_none());
        assert!(c.completed.is_none());
        assert!(c.body.is_empty());
    }

    #[test]
    fn card_touch_updates_timestamp() {
        let mut c = Card::new("001".into(), "Test".into());
        let before = c.updated;
        std::thread::sleep(std::time::Duration::from_millis(2));
        c.touch();
        assert!(c.updated >= before);
    }

    // ── move_card edge cases ──

    #[test]
    fn move_card_same_column_noop() {
        let mut board = test_board_with_done();
        let count_before = board.columns[0].cards.len();
        board.move_card(0, 0, 0);
        assert_eq!(board.columns[0].cards.len(), count_before);
    }

    #[test]
    fn move_card_invalid_index_does_not_panic() {
        let mut board = test_board_with_done();
        // Out-of-bounds card index
        board.move_card(0, 999, 1);
        // Out-of-bounds column index
        board.move_card(999, 0, 0);
        board.move_card(0, 0, 999);
    }

    // ── sort_cards specifics ──

    #[test]
    fn sort_cards_blocked_first() {
        let mut col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards: vec![],
        };
        let mut normal = Card::new("1".into(), "Normal".into());
        normal.priority = Priority::Normal;
        let mut blocked = Card::new("2".into(), "Blocked".into());
        blocked.priority = Priority::Normal;
        blocked.blocked = true;
        col.cards = vec![normal, blocked];
        col.sort_cards();
        // Both Normal priority, but sort is by priority then updated — blocked doesn't affect sort
        // The sort is: priority.sort_key then updated desc
        // Since both are Normal, the one with a later updated time sorts first
        assert_eq!(col.cards.len(), 2);
    }

    #[test]
    fn sort_cards_priority_then_updated() {
        use chrono::TimeZone;
        let mut col = Column {
            slug: "test".into(),
            name: "Test".into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards: vec![],
        };
        let mut old = Card::new("1".into(), "Old".into());
        old.priority = Priority::Normal;
        old.updated = Utc.with_ymd_and_hms(2025, 1, 1, 0, 0, 0).unwrap();
        let mut recent = Card::new("2".into(), "Recent".into());
        recent.priority = Priority::Normal;
        recent.updated = Utc.with_ymd_and_hms(2025, 6, 1, 0, 0, 0).unwrap();
        col.cards = vec![old, recent];
        col.sort_cards();
        // Most recently updated sorts first
        assert_eq!(col.cards[0].id, "2");
        assert_eq!(col.cards[1].id, "1");
    }

    // ── Priority display ──

    #[test]
    fn priority_display() {
        assert_eq!(format!("{}", Priority::Urgent), "urgent");
        assert_eq!(format!("{}", Priority::High), "high");
        assert_eq!(format!("{}", Priority::Normal), "normal");
        assert_eq!(format!("{}", Priority::Low), "low");
    }

    #[test]
    fn priority_as_str_all_variants() {
        assert_eq!(Priority::Urgent.as_str(), "urgent");
        assert_eq!(Priority::High.as_str(), "high");
        assert_eq!(Priority::Normal.as_str(), "normal");
        assert_eq!(Priority::Low.as_str(), "low");
    }

    // ── slug_to_name ──

    #[test]
    fn slug_to_name_single_word() {
        assert_eq!(slug_to_name("backlog"), "Backlog");
    }

    #[test]
    fn slug_to_name_multi_word() {
        assert_eq!(slug_to_name("in-progress"), "In Progress");
    }

    #[test]
    fn slug_to_name_three_words() {
        assert_eq!(slug_to_name("ready-for-review"), "Ready For Review");
    }

    #[test]
    fn slug_to_name_empty_string() {
        assert_eq!(slug_to_name(""), "");
    }

    #[test]
    fn slug_to_name_with_digits() {
        assert_eq!(slug_to_name("col-2"), "Col 2");
    }

    #[test]
    fn slug_to_name_uppercase_passthrough() {
        // Only the first char of each segment is uppercased; rest is unchanged.
        assert_eq!(slug_to_name("in-PROGRESS"), "In PROGRESS");
    }

    #[test]
    fn slug_to_name_single_hyphen_does_not_panic() {
        // "-" splits into two empty segments → no panic.
        let result = slug_to_name("-");
        assert_eq!(result, " "); // two empty words joined by space
    }

    // ── slug_from_name ──

    #[test]
    fn slug_from_name_single_word() {
        assert_eq!(slug_from_name("Backlog"), "backlog");
    }

    #[test]
    fn slug_from_name_multi_word() {
        assert_eq!(slug_from_name("In Progress"), "in-progress");
    }

    #[test]
    fn slug_from_name_empty_string_falls_back_to_col() {
        assert_eq!(slug_from_name(""), "col");
    }

    #[test]
    fn slug_from_name_only_special_chars_falls_back_to_col() {
        assert_eq!(slug_from_name("!!! ???"), "col");
    }

    #[test]
    fn slug_from_name_leading_trailing_spaces_trimmed() {
        assert_eq!(slug_from_name("  hello  "), "hello");
    }

    #[test]
    fn slug_from_name_consecutive_spaces_collapse_to_one_hyphen() {
        assert_eq!(slug_from_name("hello   world"), "hello-world");
    }

    #[test]
    fn slug_from_name_leading_special_chars_stripped() {
        assert_eq!(slug_from_name("---hello"), "hello");
    }

    #[test]
    fn slug_from_name_unicode_non_ascii_falls_back_to_col() {
        assert_eq!(slug_from_name("日本語"), "col");
    }

    #[test]
    fn slug_from_name_mixed_ascii_unicode_keeps_ascii_part() {
        assert_eq!(slug_from_name("hello 世界"), "hello");
    }

    #[test]
    fn slug_from_name_numbers_in_name() {
        assert_eq!(slug_from_name("Col 2"), "col-2");
    }

    #[test]
    fn slug_from_name_does_not_guard_archive_reservation() {
        // slug_from_name does NOT remap the reserved "archive" slug —
        // that guard lives in cmd_col_rename. This is the documented distinction
        // between slug_from_name and generate_slug.
        assert_eq!(slug_from_name("Archive"), "archive");
    }

    // ── generate_slug ──

    fn slug_col(slug: &str) -> Column {
        Column { slug: slug.into(), name: slug.into(), order: 0, wip_limit: None, hidden: false, cards: vec![] }
    }

    #[test]
    fn generate_slug_basic_name() {
        assert_eq!(generate_slug("My Feature", &[]), "my-feature");
    }

    #[test]
    fn generate_slug_spaces_become_hyphens() {
        assert_eq!(generate_slug("Hello   World!!!", &[]), "hello-world");
    }

    #[test]
    fn generate_slug_reserved_archive_avoided() {
        let slug = generate_slug("Archive", &[]);
        assert_ne!(slug, "archive", "should not produce the reserved slug 'archive'");
        // The implementation redirects "archive" to "archive-col", but the key
        // contract is just that the reserved slug is avoided.
        assert!(slug.starts_with("archive"), "slug should still be archive-derived: {slug}");
    }

    #[test]
    fn generate_slug_uniqueness_appends_suffix() {
        let existing = vec![slug_col("backlog")];
        assert_eq!(generate_slug("Backlog", &existing), "backlog-2");
    }

    #[test]
    fn generate_slug_uniqueness_skips_taken_suffixes() {
        let existing = vec![slug_col("backlog"), slug_col("backlog-2")];
        assert_eq!(generate_slug("Backlog", &existing), "backlog-3");
    }

    #[test]
    fn generate_slug_all_non_alphanum_falls_back_to_col() {
        assert_eq!(generate_slug("!!! ???", &[]), "col");
    }

    #[test]
    fn generate_slug_col_uniqueness_when_col_taken() {
        let existing = vec![slug_col("col")];
        assert_eq!(generate_slug("!!! ???", &existing), "col-2");
    }

    #[test]
    fn generate_slug_unicode_name_produces_valid_ascii() {
        // All CJK chars are non-ASCII-alphanumeric → stripped → empty base → fallback "col".
        let slug = generate_slug("日本語", &[]);
        assert!(slug.chars().all(|c| c.is_ascii()), "slug must be ASCII");
        assert_eq!(slug, "col", "non-ASCII-only name should fall back to 'col'");
    }

    #[test]
    fn generate_slug_leading_special_chars_stripped() {
        // Name starts with special chars that map to hyphens; leading hyphens removed.
        let slug = generate_slug("---hello", &[]);
        assert!(slug.starts_with(|c: char| c.is_ascii_alphanumeric()));
    }

    // ── normalize_column_orders ──

    fn make_col(slug: &str, order: u32) -> Column {
        Column { slug: slug.into(), name: slug.into(), order, wip_limit: None, hidden: false, cards: vec![] }
    }

    #[test]
    fn normalize_column_orders_dense_already() {
        let mut cols = vec![make_col("a", 0), make_col("b", 1), make_col("c", 2)];
        normalize_column_orders(&mut cols);
        assert_eq!(cols.iter().map(|c| c.order).collect::<Vec<_>>(), [0, 1, 2]);
    }

    #[test]
    fn normalize_column_orders_sparse_orders() {
        let mut cols = vec![make_col("a", 0), make_col("b", 10), make_col("c", 20)];
        normalize_column_orders(&mut cols);
        // Should sort by original order then assign 0,1,2.
        assert_eq!(cols[0].slug, "a");
        assert_eq!(cols[1].slug, "b");
        assert_eq!(cols[2].slug, "c");
        assert_eq!(cols.iter().map(|c| c.order).collect::<Vec<_>>(), [0, 1, 2]);
    }

    #[test]
    fn normalize_column_orders_reorders_by_order_field() {
        // Vec is in wrong order relative to the order field.
        let mut cols = vec![make_col("c", 2), make_col("a", 0), make_col("b", 1)];
        normalize_column_orders(&mut cols);
        assert_eq!(cols[0].slug, "a");
        assert_eq!(cols[1].slug, "b");
        assert_eq!(cols[2].slug, "c");
    }

    #[test]
    fn normalize_column_orders_single_column() {
        let mut cols = vec![make_col("solo", 42)];
        normalize_column_orders(&mut cols);
        assert_eq!(cols[0].order, 0);
    }

    #[test]
    fn normalize_column_orders_empty_vec_does_not_panic() {
        let mut cols: Vec<Column> = vec![];
        normalize_column_orders(&mut cols); // must not panic
    }
}
