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

/// Run auto-close on the board. Returns the IDs of cards that were moved.
pub fn run_auto_close(board: &mut Board, now: DateTime<Utc>) -> Vec<String> {
    let policies = board.policies.clone();
    let target_slug = policies.auto_close_target.clone();

    // Find the target column index
    let target_col = match board.columns.iter().position(|c| c.slug == target_slug) {
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
            if should_auto_close(card, &policies, now) {
                to_move.push((col_idx, card_idx));
            }
        }
    }

    // Move in reverse order to preserve indices
    let mut closed_ids = Vec::new();
    for &(col_idx, card_idx) in to_move.iter().rev() {
        let card = board.columns[col_idx].cards.remove(card_idx);
        closed_ids.push(card.id.clone());
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
}
