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
    if policies.bubble_up_days == 0 {
        return Staleness::Fresh;
    }
    let days_since_update = (now - card.updated).num_days().max(0) as u32;
    if days_since_update >= policies.bubble_up_days * 2 {
        Staleness::VeryStale
    } else if days_since_update >= policies.bubble_up_days {
        Staleness::Stale
    } else {
        Staleness::Fresh
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
            bubble_up_days: 7,
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
            tutorial_shown: true,
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
        let policies = Policies { bubble_up_days: 0, ..Default::default() };
        let old = Card {
            updated: Utc.with_ymd_and_hms(2024, 1, 1, 0, 0, 0).unwrap(),
            ..Card::new("1".into(), "Ancient".into())
        };
        assert_eq!(staleness(&old, &policies, now), Staleness::Fresh);
    }

    #[test]
    fn staleness_boundary_exact_threshold() {
        let now = Utc.with_ymd_and_hms(2025, 6, 15, 12, 0, 0).unwrap();
        let policies = Policies { bubble_up_days: 7, ..Default::default() };
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
        let policies = Policies { bubble_up_days: 7, ..Default::default() };
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
}
