use chrono::{DateTime, Utc};

use super::{Board, Card, Policies};

/// Format a card's age as a human-readable string.
pub fn format_age(created: DateTime<Utc>, now: DateTime<Utc>) -> String {
    let days = (now - created).num_days().max(0) as u64;
    if days == 0 {
        "new".to_string()
    } else if days < 14 {
        format!("{days}d")
    } else if days < 60 {
        format!("{}w", days / 7)
    } else {
        format!("{}mo", days / 30)
    }
}

/// How stale a card is, based on `updated` and bubble-up policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Staleness {
    /// Card was recently updated, no warning needed.
    Fresh,
    /// Card has passed the bubble-up threshold.
    Stale,
    /// Card is very stale (2x the bubble-up threshold).
    VeryStale,
}

/// Determine how stale a card is relative to the bubble-up policy.
pub fn staleness(card: &Card, policies: &Policies, now: DateTime<Utc>) -> Staleness {
    if policies.stale_days == 0 {
        return Staleness::Fresh;
    }
    let days_since_update = (now - card.updated).num_days().max(0) as u32;
    if days_since_update >= policies.stale_days * 2 {
        Staleness::VeryStale
    } else if days_since_update >= policies.stale_days {
        Staleness::Stale
    } else {
        Staleness::Fresh
    }
}

/// Classify a card's staleness as a human-readable label for the filter picker.
///
/// - `"new"` — created today (same UTC day)
/// - `"normal"` — fresh but not new
/// - `"stale"` — past the bubble-up threshold
/// - `"very stale"` — past 2× the bubble-up threshold
pub fn card_staleness_label(card: &Card, policies: &Policies, now: DateTime<Utc>) -> &'static str {
    match staleness(card, policies, now) {
        Staleness::VeryStale => "very stale",
        Staleness::Stale => "stale",
        Staleness::Fresh => {
            if card.created.date_naive() == now.date_naive() {
                "new"
            } else {
                "normal"
            }
        }
    }
}

/// Check if a card should be auto-closed.
pub fn should_auto_close(card: &Card, policies: &Policies, now: DateTime<Utc>) -> bool {
    if policies.auto_close_days == 0 {
        return false;
    }
    let days_since_update = (now - card.updated).num_days().max(0) as u32;
    days_since_update >= policies.auto_close_days
}

/// A card that was moved to the auto-close target column.
#[derive(Debug)]
pub struct ClosedCard {
    pub id: String,
    pub title: String,
    pub from_col_slug: String,
}

/// Run the auto-close policy on the board.
///
/// Returns one [`ClosedCard`] per card that was moved to the auto-close target.
pub fn run_auto_close(board: &mut Board, now: DateTime<Utc>) -> Vec<ClosedCard> {
    let auto_close_days = board.policies.auto_close_days;
    if auto_close_days == 0 {
        return Vec::new();
    }

    // Find the target column index
    let target_col = match board
        .columns
        .iter()
        .position(|c| c.slug == board.policies.auto_close_target)
    {
        Some(idx) => idx,
        None => return Vec::new(),
    };

    // Collect cards to auto-close (skip cards already in the target column)
    let mut to_move: Vec<(usize, usize)> = Vec::new();
    for (col_idx, col) in board.columns.iter().enumerate() {
        if col_idx == target_col {
            continue;
        }
        for (card_idx, card) in col.cards.iter().enumerate() {
            if should_auto_close(card, &board.policies, now) {
                to_move.push((col_idx, card_idx));
            }
        }
    }

    // Move in reverse order to preserve indices
    let mut closed_ids = Vec::new();
    for &(col_idx, card_idx) in to_move.iter().rev() {
        let from_col_slug = board.columns[col_idx].slug.clone();
        let card = board.columns[col_idx].cards.remove(card_idx);
        closed_ids.push(ClosedCard { id: card.id.clone(), title: card.title.clone(), from_col_slug });
        board.columns[target_col].cards.push(card);
    }

    // Re-sort affected columns
    for col in &mut board.columns {
        col.sort_cards();
    }

    closed_ids
}

/// A card that was moved to the archive column by the auto-archive policy.
#[derive(Debug)]
pub struct ArchivedCard {
    pub id: String,
    pub title: String,
    /// Slug of the source column (e.g. `"done"`). Use this to look up the display
    /// name at the call site, consistent with [`ClosedCard::from_col_slug`].
    pub from_col_slug: String,
}

/// Run the auto-archive policy on the board.
///
/// Moves cards that have been in the `done` column for at least
/// `policies.archive_after_days` days to the `archive` column.
/// Uses `card.completed` as the age reference, falling back to `card.updated`.
///
/// Returns one [`ArchivedCard`] per card moved.
pub fn run_auto_archive(board: &mut Board, now: DateTime<Utc>) -> Vec<ArchivedCard> {
    let days = board.policies.archive_after_days;
    if days == 0 {
        return Vec::new();
    }
    let threshold = chrono::Duration::days(days as i64);

    let done_idx = board.columns.iter().position(|c| c.slug == "done");
    let archive_idx = board.columns.iter().position(|c| c.slug == "archive");
    let (Some(done_idx), Some(archive_idx)) = (done_idx, archive_idx) else {
        return Vec::new();
    };

    // Collect indices of cards old enough to archive, in reverse order so that
    // removing by index doesn't shift subsequent indices.
    let to_archive: Vec<usize> = board.columns[done_idx]
        .cards
        .iter()
        .enumerate()
        .filter(|(_, card)| {
            let age_start = card.completed.unwrap_or(card.updated);
            now.signed_duration_since(age_start) >= threshold
        })
        .map(|(i, _)| i)
        .rev()
        .collect();

    let done_slug = board.columns[done_idx].slug.clone();
    let mut archived = Vec::new();
    for idx in to_archive {
        let card = board.columns[done_idx].cards.remove(idx);
        archived.push(ArchivedCard {
            id: card.id.clone(),
            title: card.title.clone(),
            from_col_slug: done_slug.clone(),
        });
        // Direct push bypasses move_card() to preserve the completed timestamp.
        board.columns[archive_idx].cards.push(card);
    }

    board.columns[done_idx].sort_cards();
    board.columns[archive_idx].sort_cards();

    archived
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    #[test]
    fn test_format_age() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();

        let created_today = now;
        assert_eq!(format_age(created_today, now), "new");

        let created_3d = Utc.with_ymd_and_hms(2025, 6, 12, 12, 0, 0).unwrap();
        assert_eq!(format_age(created_3d, now), "3d");

        let created_2w = Utc.with_ymd_and_hms(2025, 6, 1, 12, 0, 0).unwrap();
        assert_eq!(format_age(created_2w, now), "2w");

        let created_3mo = Utc.with_ymd_and_hms(2025, 3, 15, 12, 0, 0).unwrap();
        assert_eq!(format_age(created_3mo, now), "3mo");
    }

    #[test]
    fn test_staleness() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies {
            stale_days: 7,
            ..Default::default()
        };

        let fresh_card = Card {
            updated: Utc.with_ymd_and_hms(2025, 6, 12, 12, 0, 0).unwrap(),
            ..Card::new("001".into(), "Test".into())
        };
        assert_eq!(staleness(&fresh_card, &policies, now), Staleness::Fresh);

        let stale_card = Card {
            updated: Utc.with_ymd_and_hms(2025, 6, 5, 12, 0, 0).unwrap(),
            ..Card::new("002".into(), "Test".into())
        };
        assert_eq!(staleness(&stale_card, &policies, now), Staleness::Stale);

        let very_stale_card = Card {
            updated: Utc.with_ymd_and_hms(2025, 5, 28, 12, 0, 0).unwrap(),
            ..Card::new("003".into(), "Test".into())
        };
        assert_eq!(
            staleness(&very_stale_card, &policies, now),
            Staleness::VeryStale
        );
    }

    #[test]
    fn test_should_auto_close() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies {
            auto_close_days: 30,
            ..Default::default()
        };

        let recent = Card {
            updated: Utc.with_ymd_and_hms(2025, 6, 1, 12, 0, 0).unwrap(),
            ..Card::new("001".into(), "Recent".into())
        };
        assert!(!should_auto_close(&recent, &policies, now));

        let old = Card {
            updated: Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap(),
            ..Card::new("002".into(), "Old".into())
        };
        assert!(should_auto_close(&old, &policies, now));

        let disabled = Policies {
            auto_close_days: 0,
            ..Default::default()
        };
        assert!(!should_auto_close(&old, &disabled, now));
    }

    // ── run_auto_close tests ──

    fn make_column(slug: &str, name: &str, cards: Vec<Card>) -> super::super::Column {
        super::super::Column {
            slug: slug.into(),
            name: name.into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards,
        }
    }

    fn make_board(columns: Vec<super::super::Column>, policies: Policies) -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 100,
            policies,
            sync_branch: None,

            nerd_font: false,
            created_at: None,
            columns,
        }
    }

    #[test]
    fn run_auto_close_moves_stale_cards() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut stale_card = Card::new("1".into(), "Stale".into());
        stale_card.updated = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap(); // 45 days ago

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![stale_card]),
            make_column("in-progress", "In Progress", vec![]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 1);
        assert_eq!(closed[0].id, "1");
        assert_eq!(closed[0].from_col_slug, "backlog");
        assert_eq!(board.columns[0].cards.len(), 0); // removed from backlog
        assert_eq!(board.columns[2].cards.len(), 1); // moved to archive
        assert_eq!(board.columns[2].cards[0].id, "1");
    }

    #[test]
    fn run_auto_close_skips_fresh_cards() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut fresh_card = Card::new("1".into(), "Fresh".into());
        fresh_card.updated = Utc.with_ymd_and_hms(2025, 6, 10, 12, 0, 0).unwrap(); // 5 days ago

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![fresh_card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty());
        assert_eq!(board.columns[0].cards.len(), 1); // still in backlog
    }

    #[test]
    fn run_auto_close_skips_cards_in_target_column() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut old_card = Card::new("1".into(), "Old".into());
        old_card.updated = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("archive", "Archive", vec![old_card]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty());
        assert_eq!(board.columns[1].cards.len(), 1); // stays in archive
    }

    #[test]
    fn run_auto_close_disabled_when_zero() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut stale_card = Card::new("1".into(), "Stale".into());
        stale_card.updated = Utc.with_ymd_and_hms(2025, 1, 1, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 0,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![stale_card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty());
        assert_eq!(board.columns[0].cards.len(), 1); // untouched
    }

    #[test]
    fn run_auto_close_no_target_column_returns_empty() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut stale_card = Card::new("1".into(), "Stale".into());
        stale_card.updated = Utc.with_ymd_and_hms(2025, 1, 1, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "nonexistent".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![stale_card]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty());
    }

    #[test]
    fn run_auto_close_multiple_stale_across_columns() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut stale1 = Card::new("1".into(), "Stale1".into());
        stale1.updated = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();
        let mut stale2 = Card::new("2".into(), "Stale2".into());
        stale2.updated = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };

        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![stale1]),
            make_column("in-progress", "In Progress", vec![stale2]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 2);
        assert_eq!(board.columns[2].cards.len(), 2); // both in archive
    }

    // ── format_age boundary tests ──

    #[test]
    fn format_age_boundary_14_days() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // 13 days → still "Xd"
        let d13 = Utc.with_ymd_and_hms(2025, 6, 2, 12, 0, 0).unwrap();
        assert_eq!(format_age(d13, now), "13d");
        // 14 days → switches to weeks
        let d14 = Utc.with_ymd_and_hms(2025, 6, 1, 12, 0, 0).unwrap();
        assert_eq!(format_age(d14, now), "2w");
    }

    #[test]
    fn format_age_boundary_60_days() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // 59 days → still weeks
        let d59 = Utc.with_ymd_and_hms(2025, 4, 17, 12, 0, 0).unwrap();
        assert_eq!(format_age(d59, now), "8w");
        // 60 days → switches to months
        let d60 = Utc.with_ymd_and_hms(2025, 4, 16, 12, 0, 0).unwrap();
        assert_eq!(format_age(d60, now), "2mo");
    }

    #[test]
    fn format_age_future_date_returns_new() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let future = Utc.with_ymd_and_hms(2025, 7, 1, 12, 0, 0).unwrap();
        assert_eq!(format_age(future, now), "new");
    }

    // ── staleness boundary tests ──

    #[test]
    fn staleness_disabled_when_zero() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 0, ..Default::default() };
        let old = Card {
            updated: Utc.with_ymd_and_hms(2024, 1, 1, 0, 0, 0).unwrap(),
            ..Card::new("1".into(), "Ancient".into())
        };
        assert_eq!(staleness(&old, &policies, now), Staleness::Fresh);
    }

    #[test]
    fn staleness_boundary_exact_threshold() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Exactly 7 days ago → Stale (>=)
        let card = Card {
            updated: Utc.with_ymd_and_hms(2025, 6, 8, 12, 0, 0).unwrap(),
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(staleness(&card, &policies, now), Staleness::Stale);
    }

    #[test]
    fn staleness_boundary_exact_double() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        // Exactly 14 days ago → VeryStale (>= 2x)
        let card = Card {
            updated: Utc.with_ymd_and_hms(2025, 6, 1, 12, 0, 0).unwrap(),
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(staleness(&card, &policies, now), Staleness::VeryStale);
    }

    // ── ClosedCard / run_auto_close additions ──

    #[test]
    fn run_auto_close_closed_card_title_is_correct() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let mut stale = Card::new("7".into(), "Important Work".into());
        stale.updated = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![stale]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 1);
        assert_eq!(closed[0].title, "Important Work");
    }

    #[test]
    fn run_auto_close_at_exact_boundary_is_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // Exactly 30 days ago — should meet the >= threshold
        let mut card = Card::new("1".into(), "Boundary".into());
        card.updated = Utc.with_ymd_and_hms(2025, 5, 16, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 1, "card at exact threshold should be moved");
    }

    #[test]
    fn run_auto_close_one_day_short_not_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // 29 days ago — one day short of the 30-day threshold
        let mut card = Card::new("1".into(), "Almost Stale".into());
        card.updated = Utc.with_ymd_and_hms(2025, 5, 17, 12, 0, 0).unwrap();

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty(), "card 29 days old should not be moved at 30-day threshold");
        assert_eq!(board.columns[0].cards.len(), 1);
    }

    #[test]
    fn run_auto_close_multiple_stale_same_column_all_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();
        let mut c1 = Card::new("1".into(), "First".into());
        c1.updated = old;
        let mut c2 = Card::new("2".into(), "Second".into());
        c2.updated = old;
        let mut c3 = Card::new("3".into(), "Third".into());
        c3.updated = old;

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![c1, c2, c3]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 3);
        assert_eq!(board.columns[0].cards.len(), 0);
        assert_eq!(board.columns[1].cards.len(), 3);
    }

    #[test]
    fn run_auto_close_from_col_slug_reflects_source_column() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();
        let mut card_a = Card::new("1".into(), "A".into());
        card_a.updated = old;
        let mut card_b = Card::new("2".into(), "B".into());
        card_b.updated = old;

        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![card_a]),
            make_column("in-progress", "In Progress", vec![card_b]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let mut closed = run_auto_close(&mut board, now);
        assert_eq!(closed.len(), 2);
        closed.sort_by(|a, b| a.id.cmp(&b.id));
        assert_eq!(closed[0].from_col_slug, "backlog");
        assert_eq!(closed[1].from_col_slug, "in-progress");
    }

    #[test]
    fn run_auto_close_empty_board_no_panic() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies {
            auto_close_days: 30,
            auto_close_target: "archive".to_string(),
            ..Default::default()
        };
        let mut board = make_board(vec![], policies);
        let closed = run_auto_close(&mut board, now);
        assert!(closed.is_empty());
    }

    // ── run_auto_archive tests ──

    #[test]
    fn run_auto_archive_moves_card_from_done_to_archive() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let completed = Utc.with_ymd_and_hms(2025, 6, 5, 12, 0, 0).unwrap(); // 10 days ago
        let mut card = Card::new("1".into(), "Done task".into());
        card.completed = Some(completed);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1);
        assert_eq!(archived[0].id, "1");
        assert_eq!(board.columns[0].cards.len(), 0, "done column should be empty");
        assert_eq!(board.columns[1].cards.len(), 1, "archive column should have one card");
        assert_eq!(board.columns[1].cards[0].id, "1");
    }

    #[test]
    fn run_auto_archive_disabled_when_zero() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 1, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Old task".into());
        card.completed = Some(old);

        let policies = Policies { archive_after_days: 0, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty());
        assert_eq!(board.columns[0].cards.len(), 1, "card should remain in done");
    }

    #[test]
    fn run_auto_archive_at_exact_threshold_is_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // Exactly 7 days ago — should meet the >= threshold
        let completed = Utc.with_ymd_and_hms(2025, 6, 8, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Boundary".into());
        card.completed = Some(completed);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1, "card at exact 7-day threshold should be archived");
    }

    #[test]
    fn run_auto_archive_one_day_short_not_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // 6 days ago — one short of 7-day threshold
        let completed = Utc.with_ymd_and_hms(2025, 6, 9, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Almost ready".into());
        card.completed = Some(completed);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty(), "card 6 days old should not be archived at 7-day threshold");
        assert_eq!(board.columns[0].cards.len(), 1, "card should still be in done");
    }

    #[test]
    fn run_auto_archive_preserves_completed_timestamp() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let completed = Utc.with_ymd_and_hms(2025, 6, 5, 12, 0, 0).unwrap(); // 10 days ago
        let mut card = Card::new("1".into(), "Task".into());
        card.completed = Some(completed);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        run_auto_archive(&mut board, now);

        let archived_card = &board.columns[1].cards[0];
        assert_eq!(
            archived_card.completed,
            Some(completed),
            "completed timestamp must be preserved after auto-archiving"
        );
    }

    #[test]
    fn run_auto_archive_uses_completed_over_updated() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // completed is recent (3 days ago — below 7-day threshold)
        let completed = Utc.with_ymd_and_hms(2025, 6, 12, 12, 0, 0).unwrap();
        // updated is very old (30 days ago — above threshold)
        let updated = Utc.with_ymd_and_hms(2025, 5, 16, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Recently completed".into());
        card.completed = Some(completed);
        card.updated = updated;

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(
            archived.is_empty(),
            "completed (3 days ago) takes priority over updated (30 days ago); must not be archived"
        );
        assert_eq!(board.columns[0].cards.len(), 1, "card should remain in done");
    }

    #[test]
    fn run_auto_archive_falls_back_to_updated_when_no_completed() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // No completed timestamp; updated is 10 days ago (above 7-day threshold)
        let updated = Utc.with_ymd_and_hms(2025, 6, 5, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "No completed".into());
        card.updated = updated;
        // card.completed is None by default

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1, "should fall back to updated when completed is None");
    }

    #[test]
    fn run_auto_archive_only_moves_from_done_not_other_columns() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap(); // 45 days ago
        // Old card in backlog — must NOT be archived (only "done" is in scope)
        let mut backlog_card = Card::new("1".into(), "Backlog old".into());
        backlog_card.updated = old;
        // Old card in done — must be archived
        let mut done_card = Card::new("2".into(), "Done old".into());
        done_card.completed = Some(old);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![backlog_card]),
            make_column("done", "Done", vec![done_card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1, "only done-column cards should be archived");
        assert_eq!(archived[0].id, "2");
        assert_eq!(board.columns[0].cards.len(), 1, "backlog card should be untouched");
        assert_eq!(board.columns[1].cards.len(), 0, "done card should be moved");
        assert_eq!(board.columns[2].cards.len(), 1, "archive should have the done card");
    }

    #[test]
    fn run_auto_archive_no_done_column_returns_empty() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty());
    }

    #[test]
    fn run_auto_archive_no_archive_column_returns_empty() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Old done".into());
        card.completed = Some(old);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty());
        assert_eq!(board.columns[0].cards.len(), 1, "card must stay when no archive column exists");
    }

    #[test]
    fn run_auto_archive_multiple_done_cards_all_old_all_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let mut c1 = Card::new("1".into(), "First".into());
        c1.completed = Some(old);
        let mut c2 = Card::new("2".into(), "Second".into());
        c2.completed = Some(old);
        let mut c3 = Card::new("3".into(), "Third".into());
        c3.completed = Some(old);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![c1, c2, c3]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 3, "all three old cards should be archived");
        assert_eq!(board.columns[0].cards.len(), 0, "done column should be empty");
        assert_eq!(board.columns[1].cards.len(), 3, "archive column should have 3 cards");
    }

    #[test]
    fn run_auto_archive_mixed_old_and_young_only_old_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let recent = Utc.with_ymd_and_hms(2025, 6, 13, 12, 0, 0).unwrap(); // 2 days ago
        let mut old_card = Card::new("1".into(), "Old".into());
        old_card.completed = Some(old);
        let mut new_card = Card::new("2".into(), "New".into());
        new_card.completed = Some(recent);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![old_card, new_card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1, "only the old card should be archived");
        assert_eq!(archived[0].id, "1");
        assert_eq!(board.columns[0].cards.len(), 1, "recent card should remain in done");
        assert_eq!(board.columns[0].cards[0].id, "2", "the remaining card should be the recent one");
    }

    #[test]
    fn run_auto_archive_returns_correct_id_and_title() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("042".into(), "Very Important Task".into());
        card.completed = Some(old);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1);
        assert_eq!(archived[0].id, "042");
        assert_eq!(archived[0].title, "Very Important Task");
    }

    #[test]
    fn run_auto_archive_from_col_slug_is_done_slug() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let old = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Task".into());
        card.completed = Some(old);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert_eq!(archived.len(), 1);
        assert_eq!(archived[0].from_col_slug, "done");
    }

    #[test]
    fn run_auto_archive_does_not_update_updated_field() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let completed = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let updated = Utc.with_ymd_and_hms(2025, 4, 20, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Task".into());
        card.completed = Some(completed);
        card.updated = updated;

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        run_auto_archive(&mut board, now);

        let archived_card = &board.columns[1].cards[0];
        assert_eq!(
            archived_card.updated, updated,
            "archiving must not change updated (touch() is intentionally skipped)"
        );
    }

    #[test]
    fn run_auto_archive_preserves_started_timestamp() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let completed = Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap();
        let started = Utc.with_ymd_and_hms(2025, 4, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Task".into());
        card.completed = Some(completed);
        card.started = Some(started);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        run_auto_archive(&mut board, now);

        let archived_card = &board.columns[1].cards[0];
        assert_eq!(
            archived_card.started,
            Some(started),
            "started timestamp must be preserved after auto-archiving"
        );
    }

    #[test]
    fn run_auto_archive_empty_done_column_returns_empty() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty());
    }

    #[test]
    fn run_auto_archive_future_completed_not_moved() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        // completed is in the future — signed_duration_since < threshold
        let completed = Utc.with_ymd_and_hms(2025, 7, 1, 12, 0, 0).unwrap();
        let mut card = Card::new("1".into(), "Future done".into());
        card.completed = Some(completed);

        let policies = Policies { archive_after_days: 7, ..Default::default() };
        let mut board = make_board(vec![
            make_column("done", "Done", vec![card]),
            make_column("archive", "Archive", vec![]),
        ], policies);

        let archived = run_auto_archive(&mut board, now);
        assert!(archived.is_empty(), "card with future completed timestamp must not be archived");
        assert_eq!(board.columns[0].cards.len(), 1);
    }

    // ── card_staleness_label tests ──

    #[test]
    fn card_staleness_label_returns_new_for_card_created_today() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let card = Card {
            created: now,
            updated: now,
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(card_staleness_label(&card, &policies, now), "new");
    }

    #[test]
    fn card_staleness_label_returns_normal_for_fresh_non_new() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let card = Card {
            created: Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap(),
            updated: Utc.with_ymd_and_hms(2025, 6, 14, 12, 0, 0).unwrap(),
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(card_staleness_label(&card, &policies, now), "normal");
    }

    #[test]
    fn card_staleness_label_returns_stale() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let card = Card {
            created: Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap(),
            updated: Utc.with_ymd_and_hms(2025, 6, 8, 12, 0, 0).unwrap(), // exactly 7 days ago
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(card_staleness_label(&card, &policies, now), "stale");
    }

    #[test]
    fn card_staleness_label_returns_very_stale() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let card = Card {
            created: Utc.with_ymd_and_hms(2025, 5, 1, 12, 0, 0).unwrap(),
            updated: Utc.with_ymd_and_hms(2025, 6, 1, 12, 0, 0).unwrap(), // 14 days ago (2x threshold)
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(card_staleness_label(&card, &policies, now), "very stale");
    }

    #[test]
    fn card_staleness_label_stale_days_zero_new_card() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 0, ..Default::default() };
        let card = Card {
            created: now,
            updated: now,
            ..Card::new("1".into(), "Test".into())
        };
        assert_eq!(card_staleness_label(&card, &policies, now), "new");
    }

    #[test]
    fn card_staleness_label_stale_days_zero_old_card() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { stale_days: 0, ..Default::default() };
        let card = Card {
            created: Utc.with_ymd_and_hms(2024, 1, 1, 0, 0, 0).unwrap(),
            updated: Utc.with_ymd_and_hms(2024, 1, 1, 0, 0, 0).unwrap(),
            ..Card::new("1".into(), "Test".into())
        };
        // stale_days=0 means staleness is always Fresh, but card is old → "normal"
        assert_eq!(card_staleness_label(&card, &policies, now), "normal");
    }

    #[test]
    fn card_staleness_label_utc_day_boundary() {
        // Card created just before midnight, now is just after midnight → different UTC days
        let created = Utc.with_ymd_and_hms(2025, 6, 14, 23, 59, 59).unwrap();
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 0, 0, 1).unwrap();
        let policies = Policies { stale_days: 7, ..Default::default() };
        let card = Card {
            created,
            updated: created,
            ..Card::new("1".into(), "Test".into())
        };
        // Card is only 2 seconds old but created on a different UTC day → "normal" not "new"
        assert_eq!(card_staleness_label(&card, &policies, now), "normal");
    }
}
