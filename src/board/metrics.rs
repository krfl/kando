use std::collections::BTreeMap;

use chrono::{DateTime, Datelike, IsoWeek, NaiveDate, Utc};

use super::{Board, Priority};

/// Aggregate board metrics for a given time window.
#[derive(Debug)]
pub struct BoardMetrics {
    /// Per-column WIP info including limits and active status.
    pub wip_per_column: Vec<WipEntry>,
    /// Total cards in active columns (between backlog and done).
    pub active_wip_total: usize,
    /// Blocked card count in active columns.
    pub blocked_count: u32,
    /// Blocked as percentage of active WIP.
    pub blocked_pct: f64,
    /// (week label "YYYY-Www", completed count) ordered chronologically.
    pub throughput_per_week: Vec<(String, u32)>,
    /// Standard deviation of throughput counts (None if < 2 weeks).
    pub throughput_stddev: Option<f64>,
    /// (week label, created count) for arrival rate.
    pub arrival_per_week: Vec<(String, u32)>,
    /// Lead time and cycle time statistics for completed cards.
    pub time_stats: Option<TimeStats>,
    /// Number of cards per priority level (only for completed cards in window).
    pub priority_breakdown: Vec<(Priority, u32)>,
    /// Total number of completed cards in the window.
    pub total_completed: u32,
    /// The start of the lookback window.
    pub since: DateTime<Utc>,
    /// Work item age for in-progress cards.
    pub work_item_age: Option<WorkItemAgeStats>,
}

/// Per-column WIP entry with limit and active status.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WipEntry {
    pub name: String,
    pub count: usize,
    pub wip_limit: Option<u32>,
    /// True for columns between backlog (idx 0) and done.
    pub is_active: bool,
}

/// Descriptive statistics for lead time and cycle time in days.
#[derive(Debug)]
pub struct TimeStats {
    // Lead time: completed - created (total time in system)
    pub lead_avg_days: f64,
    pub lead_median_days: f64,
    pub lead_min_days: f64,
    pub lead_max_days: f64,
    pub lead_p85_days: f64,
    // Cycle time: completed - started (active work time)
    pub cycle_avg_days: f64,
    pub cycle_median_days: f64,
    pub cycle_min_days: f64,
    pub cycle_max_days: f64,
    pub cycle_p85_days: f64,
}

/// Work item age statistics for in-progress cards.
#[derive(Debug)]
pub struct WorkItemAgeStats {
    pub count: usize,
    pub avg_age_days: f64,
    pub aging_cards: Vec<AgingCard>,
}

/// A card that exceeds the P85 cycle time threshold.
#[derive(Debug)]
pub struct AgingCard {
    pub id: String,
    pub title: String,
    pub column: String,
    pub age_days: f64,
}

/// Compute board metrics, optionally filtered to cards completed after `since`.
pub fn compute_metrics(board: &Board, since: Option<DateTime<Utc>>) -> BoardMetrics {
    let now = Utc::now();

    // 1. WIP per column with limits and active status
    let done_col_idx = board.columns.iter().position(|c| c.slug == "done");

    let wip_per_column: Vec<WipEntry> = board
        .columns
        .iter()
        .enumerate()
        .map(|(idx, col)| {
            let is_active = idx > 0 && done_col_idx.is_none_or(|d| idx < d);
            WipEntry {
                name: col.name.clone(),
                count: col.cards.len(),
                wip_limit: col.wip_limit,
                is_active,
            }
        })
        .collect();

    let active_wip_total: usize = wip_per_column
        .iter()
        .filter(|w| w.is_active)
        .map(|w| w.count)
        .sum();

    // 2. Blocked cards in active columns
    let blocked_count: u32 = board
        .columns
        .iter()
        .enumerate()
        .filter(|(idx, col)| *idx > 0 && col.slug != "done")
        .flat_map(|(_, col)| col.cards.iter())
        .filter(|card| card.blocked)
        .count() as u32;

    let blocked_pct = if active_wip_total > 0 {
        blocked_count as f64 / active_wip_total as f64 * 100.0
    } else {
        0.0
    };

    // 3. Determine effective `since`
    let effective_since = since
        .or(board.created_at)
        .or_else(|| oldest_card_created(board))
        .unwrap_or_else(Utc::now);

    // 4. Collect completed cards in window from the "done" column
    let done_cards: Vec<_> = board
        .columns
        .iter()
        .filter(|col| col.slug == "done")
        .flat_map(|col| col.cards.iter())
        .filter(|card| {
            card.completed
                .is_some_and(|completed| completed >= effective_since)
        })
        .collect();

    let total_completed = done_cards.len() as u32;

    // 5. Throughput per week (bucketed by ISO week)
    let mut week_counts: BTreeMap<IsoWeek, u32> = BTreeMap::new();
    for card in &done_cards {
        if let Some(completed) = card.completed {
            let week = completed.date_naive().iso_week();
            *week_counts.entry(week).or_insert(0) += 1;
        }
    }

    let throughput_per_week = if week_counts.is_empty() {
        Vec::new()
    } else {
        fill_week_gaps(&week_counts)
    };

    // 5b. Throughput standard deviation
    let throughput_stddev = if throughput_per_week.len() >= 2 {
        let counts: Vec<f64> = throughput_per_week.iter().map(|(_, c)| *c as f64).collect();
        let mean = counts.iter().sum::<f64>() / counts.len() as f64;
        let variance = counts.iter().map(|c| (c - mean).powi(2)).sum::<f64>() / counts.len() as f64;
        Some(variance.sqrt())
    } else {
        None
    };

    // 6. Arrival rate per week (cards created across ALL columns)
    let mut arrival_counts: BTreeMap<IsoWeek, u32> = BTreeMap::new();
    for col in &board.columns {
        for card in &col.cards {
            if card.created >= effective_since {
                let week = card.created.date_naive().iso_week();
                *arrival_counts.entry(week).or_insert(0) += 1;
            }
        }
    }
    let arrival_per_week = if arrival_counts.is_empty() {
        Vec::new()
    } else {
        fill_week_gaps(&arrival_counts)
    };

    // 7. Lead time (completed - created) and Cycle time (completed - started)
    let mut lead_days: Vec<f64> = done_cards
        .iter()
        .filter_map(|card| {
            let completed = card.completed?;
            let hours = (completed - card.created).num_hours();
            Some(hours as f64 / 24.0)
        })
        .collect();

    let mut cycle_days: Vec<f64> = done_cards
        .iter()
        .filter_map(|card| {
            let completed = card.completed?;
            let start = card.started.unwrap_or(card.created);
            let hours = (completed - start).num_hours();
            Some(hours as f64 / 24.0)
        })
        .collect();

    let time_stats = if lead_days.is_empty() {
        None
    } else {
        lead_days.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        cycle_days.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        Some(TimeStats {
            lead_avg_days: avg(&lead_days),
            lead_median_days: median(&lead_days),
            lead_min_days: lead_days[0],
            lead_max_days: lead_days[lead_days.len() - 1],
            lead_p85_days: percentile(&lead_days, 85),
            cycle_avg_days: avg(&cycle_days),
            cycle_median_days: median(&cycle_days),
            cycle_min_days: cycle_days[0],
            cycle_max_days: cycle_days[cycle_days.len() - 1],
            cycle_p85_days: percentile(&cycle_days, 85),
        })
    };

    // 8. Priority breakdown (completed cards only)
    let mut priority_counts = [0u32; 4]; // indexed by sort_key: Urgent=0, High=1, Normal=2, Low=3
    for card in &done_cards {
        priority_counts[card.priority.sort_key() as usize] += 1;
    }
    let priority_breakdown = Priority::ALL
        .iter()
        .map(|p| (*p, priority_counts[p.sort_key() as usize]))
        .filter(|(_, count)| *count > 0)
        .collect();

    // 9. Work item age for in-progress cards
    let active_cards: Vec<_> = board
        .columns
        .iter()
        .enumerate()
        .filter(|(idx, col)| *idx > 0 && col.slug != "done")
        .flat_map(|(_, col)| col.cards.iter().map(move |card| (col.name.clone(), card)))
        .collect();

    let work_item_age = if active_cards.is_empty() {
        None
    } else {
        let ages: Vec<f64> = active_cards
            .iter()
            .map(|(_, card)| {
                let start = card.started.unwrap_or(card.created);
                (now - start).num_hours() as f64 / 24.0
            })
            .collect();
        let avg_age = avg(&ages);

        let p85_threshold = time_stats.as_ref().map(|ts| ts.cycle_p85_days);

        let aging_cards: Vec<AgingCard> = active_cards
            .iter()
            .zip(ages.iter())
            .filter(|(_, age)| p85_threshold.is_some_and(|p| **age > p))
            .map(|((col_name, card), age)| AgingCard {
                id: card.id.clone(),
                title: card.title.clone(),
                column: col_name.clone(),
                age_days: *age,
            })
            .collect();

        Some(WorkItemAgeStats {
            count: active_cards.len(),
            avg_age_days: avg_age,
            aging_cards,
        })
    };

    BoardMetrics {
        wip_per_column,
        active_wip_total,
        blocked_count,
        blocked_pct,
        throughput_per_week,
        throughput_stddev,
        arrival_per_week,
        time_stats,
        priority_breakdown,
        total_completed,
        since: effective_since,
        work_item_age,
    }
}

// ── Math helpers ──

fn avg(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    values.iter().sum::<f64>() / values.len() as f64
}

fn median(sorted: &[f64]) -> f64 {
    let n = sorted.len();
    if n == 0 {
        return 0.0;
    }
    if n % 2 == 0 {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    } else {
        sorted[n / 2]
    }
}

fn percentile(sorted: &[f64], pct: usize) -> f64 {
    let n = sorted.len();
    if n == 0 {
        return 0.0;
    }
    sorted[(n * pct / 100).min(n - 1)]
}

// ── Week helpers ──

/// Find the oldest `card.created` timestamp across the entire board.
fn oldest_card_created(board: &Board) -> Option<DateTime<Utc>> {
    board
        .columns
        .iter()
        .flat_map(|col| col.cards.iter())
        .map(|card| card.created)
        .min()
}

/// Format an ISO week as "YYYY-Www".
fn format_week(week: IsoWeek) -> String {
    format!("{}-W{:02}", week.year(), week.week())
}

/// Fill gaps in a BTreeMap of week counts so every week between min and max is present.
fn fill_week_gaps(counts: &BTreeMap<IsoWeek, u32>) -> Vec<(String, u32)> {
    let first_week = *counts.keys().next().unwrap();
    let last_week = *counts.keys().next_back().unwrap();

    let mut result = Vec::new();
    let mut current = monday_of_week(first_week);
    let end = monday_of_week(last_week);

    while current <= end {
        let week = current.iso_week();
        let count = counts.get(&week).copied().unwrap_or(0);
        result.push((format_week(week), count));
        current += chrono::TimeDelta::weeks(1);
    }

    result
}

/// Get the Monday (NaiveDate) of a given ISO week.
fn monday_of_week(week: IsoWeek) -> NaiveDate {
    NaiveDate::from_isoywd_opt(week.year(), week.week(), chrono::Weekday::Mon)
        .expect("valid ISO week")
}

// ── Text/CSV formatting ──

/// Human-readable multi-line text output for the CLI.
pub fn format_text(metrics: &BoardMetrics) -> String {
    let mut out = String::new();

    // Summary
    out.push_str(&format!("Board Metrics (since {})\n", metrics.since.format("%Y-%m-%d")));
    out.push_str(&format!("Total completed: {}\n", metrics.total_completed));
    out.push('\n');

    // Lead time
    if let Some(ref ts) = metrics.time_stats {
        out.push_str("Lead Time (created → done)\n");
        out.push_str(&format!("  Average: {:.1} days\n", ts.lead_avg_days));
        out.push_str(&format!("  Median:  {:.1} days\n", ts.lead_median_days));
        out.push_str(&format!("  P85:     {:.1} days\n", ts.lead_p85_days));
        out.push_str(&format!("  Min:     {:.1} days\n", ts.lead_min_days));
        out.push_str(&format!("  Max:     {:.1} days\n", ts.lead_max_days));
        out.push('\n');

        out.push_str("Cycle Time (started → done)\n");
        out.push_str(&format!("  Average: {:.1} days\n", ts.cycle_avg_days));
        out.push_str(&format!("  Median:  {:.1} days\n", ts.cycle_median_days));
        out.push_str(&format!("  P85:     {:.1} days\n", ts.cycle_p85_days));
        out.push_str(&format!("  Min:     {:.1} days\n", ts.cycle_min_days));
        out.push_str(&format!("  Max:     {:.1} days\n", ts.cycle_max_days));
        out.push('\n');
    }

    // WIP
    out.push_str(&format!("WIP per Column (Active: {})\n", metrics.active_wip_total));
    let max_name_len = metrics
        .wip_per_column
        .iter()
        .map(|w| w.name.len())
        .max()
        .unwrap_or(0)
        .max(10);
    for entry in &metrics.wip_per_column {
        if let Some(limit) = entry.wip_limit {
            out.push_str(&format!("  {:<width$} {}/{}\n", entry.name, entry.count, limit, width = max_name_len));
        } else {
            out.push_str(&format!("  {:<width$} {}\n", entry.name, entry.count, width = max_name_len));
        }
    }
    if metrics.blocked_count > 0 {
        out.push_str(&format!("  Blocked: {} ({:.1}% of active WIP)\n", metrics.blocked_count, metrics.blocked_pct));
    }
    out.push('\n');

    // Throughput
    if !metrics.throughput_per_week.is_empty() {
        out.push_str("Throughput (cards/week)\n");
        let max_count = metrics.throughput_per_week.iter().map(|(_, c)| *c).max().unwrap_or(1);
        let bar_max = 30;
        for (i, (label, count)) in metrics.throughput_per_week.iter().enumerate() {
            let bar_len = if max_count > 0 {
                (*count as usize * bar_max) / max_count as usize
            } else {
                0
            };
            let bar: String = "\u{2588}".repeat(bar_len);
            let arrival = metrics.arrival_per_week.get(i).map_or(0, |(_, c)| *c);
            out.push_str(&format!("  {:<10} {bar} {count}  (arrived: {arrival})\n", label));
        }
        if let Some(stddev) = metrics.throughput_stddev {
            out.push_str(&format!("  Variability: stddev {:.1} cards/week\n", stddev));
        }
        out.push('\n');
    }

    // Work item age
    if let Some(ref wia) = metrics.work_item_age {
        out.push_str("Work Item Age\n");
        out.push_str(&format!("  In-progress items: {}\n", wia.count));
        out.push_str(&format!("  Average age:       {:.1} days\n", wia.avg_age_days));
        if !wia.aging_cards.is_empty() {
            out.push_str(&format!("  Aging (>P85):      {} card(s)\n", wia.aging_cards.len()));
            for card in &wia.aging_cards {
                out.push_str(&format!("    #{} \"{}\" in {} ({:.1} days)\n", card.id, card.title, card.column, card.age_days));
            }
        }
        out.push('\n');
    }

    // Priority breakdown
    if !metrics.priority_breakdown.is_empty() {
        out.push_str("Priority Breakdown\n");
        for (priority, count) in &metrics.priority_breakdown {
            out.push_str(&format!("  {}: {count}\n", priority.as_str()));
        }
        out.push('\n');
    }

    out
}

/// CSV output for piping to other tools.
pub fn format_csv(metrics: &BoardMetrics) -> String {
    let mut out = String::new();

    // Throughput + arrival section
    out.push_str("week,completed,arrived\n");
    for (i, (label, count)) in metrics.throughput_per_week.iter().enumerate() {
        let arrival = metrics.arrival_per_week.get(i).map_or(0, |(_, c)| *c);
        out.push_str(&format!("{label},{count},{arrival}\n"));
    }
    out.push('\n');

    // Summary
    out.push_str("metric,value\n");
    out.push_str(&format!("total_completed,{}\n", metrics.total_completed));
    out.push_str(&format!("active_wip,{}\n", metrics.active_wip_total));
    out.push_str(&format!("blocked_count,{}\n", metrics.blocked_count));
    out.push_str(&format!("blocked_pct,{:.1}\n", metrics.blocked_pct));
    if let Some(ref ts) = metrics.time_stats {
        out.push_str(&format!("lead_avg_days,{:.1}\n", ts.lead_avg_days));
        out.push_str(&format!("lead_median_days,{:.1}\n", ts.lead_median_days));
        out.push_str(&format!("lead_p85_days,{:.1}\n", ts.lead_p85_days));
        out.push_str(&format!("lead_min_days,{:.1}\n", ts.lead_min_days));
        out.push_str(&format!("lead_max_days,{:.1}\n", ts.lead_max_days));
        out.push_str(&format!("cycle_avg_days,{:.1}\n", ts.cycle_avg_days));
        out.push_str(&format!("cycle_median_days,{:.1}\n", ts.cycle_median_days));
        out.push_str(&format!("cycle_p85_days,{:.1}\n", ts.cycle_p85_days));
        out.push_str(&format!("cycle_min_days,{:.1}\n", ts.cycle_min_days));
        out.push_str(&format!("cycle_max_days,{:.1}\n", ts.cycle_max_days));
    }
    if let Some(stddev) = metrics.throughput_stddev {
        out.push_str(&format!("throughput_stddev,{:.1}\n", stddev));
    }

    // WIP
    out.push('\n');
    out.push_str("column,wip,limit,active\n");
    for entry in &metrics.wip_per_column {
        let limit = entry.wip_limit.map_or("-".to_string(), |l| l.to_string());
        out.push_str(&format!("{},{},{},{}\n", entry.name, entry.count, limit, entry.is_active));
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::{Card, Column, Policies};

    fn make_board(columns: Vec<Column>) -> Board {
        Board {
            name: "Test".into(),
            next_card_id: 100,
            policies: Policies::default(),
            sync_branch: None,
            tutorial_shown: true,
            nerd_font: false,
            created_at: None,
            columns,
        }
    }

    fn make_column(slug: &str, name: &str, cards: Vec<Card>) -> Column {
        Column {
            slug: slug.into(),
            name: name.into(),
            order: 0,
            wip_limit: None,
            hidden: false,
            cards,
        }
    }

    fn make_column_with_limit(slug: &str, name: &str, cards: Vec<Card>, limit: u32) -> Column {
        Column {
            slug: slug.into(),
            name: name.into(),
            order: 0,
            wip_limit: Some(limit),
            hidden: false,
            cards,
        }
    }

    fn card_completed_at(id: &str, created: DateTime<Utc>, completed: DateTime<Utc>) -> Card {
        let mut c = Card::new(id.into(), format!("Card {id}"));
        c.created = created;
        c.updated = completed;
        c.completed = Some(completed);
        c
    }

    fn card_started_completed(id: &str, created: DateTime<Utc>, started: DateTime<Utc>, completed: DateTime<Utc>) -> Card {
        let mut c = Card::new(id.into(), format!("Card {id}"));
        c.created = created;
        c.started = Some(started);
        c.updated = completed;
        c.completed = Some(completed);
        c
    }

    // ── Existing tests (adapted for new struct) ──

    #[test]
    fn empty_board_yields_zero_metrics() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 0);
        assert!(metrics.time_stats.is_none());
        assert!(metrics.throughput_per_week.is_empty());
        assert!(metrics.priority_breakdown.is_empty());
    }

    #[test]
    fn wip_counts_match_column_cards() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![
                Card::new("1".into(), "A".into()),
                Card::new("2".into(), "B".into()),
            ]),
            make_column("in-progress", "In Progress", vec![
                Card::new("3".into(), "C".into()),
            ]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.wip_per_column[0].count, 2);
        assert_eq!(metrics.wip_per_column[1].count, 1);
        assert_eq!(metrics.wip_per_column[2].count, 0);
    }

    #[test]
    fn completed_cards_counted_correctly() {
        let now = Utc::now();
        let five_days_ago = now - chrono::TimeDelta::days(5);
        let three_days_ago = now - chrono::TimeDelta::days(3);

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("done", "Done", vec![
                card_completed_at("1", five_days_ago - chrono::TimeDelta::days(2), five_days_ago),
                card_completed_at("2", three_days_ago - chrono::TimeDelta::days(1), three_days_ago),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 2);
    }

    #[test]
    fn lead_time_math_correct() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(5);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", created, now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.lead_avg_days - 5.0).abs() < 0.1, "expected ~5.0, got {}", ts.lead_avg_days);
        assert!((ts.lead_median_days - 5.0).abs() < 0.1);
        assert!((ts.lead_min_days - 5.0).abs() < 0.1);
        assert!((ts.lead_max_days - 5.0).abs() < 0.1);
    }

    #[test]
    fn cards_without_completed_excluded() {
        let mut card = Card::new("1".into(), "Incomplete".into());
        card.completed = None;

        let board = make_board(vec![
            make_column("done", "Done", vec![card]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 0);
        assert!(metrics.time_stats.is_none());
    }

    #[test]
    fn lookback_filters_old_completions() {
        let now = Utc::now();
        let two_weeks_ago = now - chrono::TimeDelta::weeks(2);
        let five_weeks_ago = now - chrono::TimeDelta::weeks(5);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", five_weeks_ago - chrono::TimeDelta::days(3), five_weeks_ago),
                card_completed_at("2", two_weeks_ago - chrono::TimeDelta::days(1), two_weeks_ago),
            ]),
        ]);

        let since = now - chrono::TimeDelta::weeks(3);
        let metrics = compute_metrics(&board, Some(since));
        assert_eq!(metrics.total_completed, 1);
    }

    #[test]
    fn priority_breakdown_counts() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(1);

        let mut high_card = card_completed_at("1", created, now);
        high_card.priority = Priority::High;
        let mut urgent_card = card_completed_at("2", created, now);
        urgent_card.priority = Priority::Urgent;
        let normal_card = card_completed_at("3", created, now);

        let board = make_board(vec![
            make_column("done", "Done", vec![high_card, urgent_card, normal_card]),
        ]);
        let metrics = compute_metrics(&board, None);

        assert_eq!(metrics.total_completed, 3);
        let breakdown: std::collections::HashMap<Priority, u32> =
            metrics.priority_breakdown.into_iter().collect();
        assert_eq!(breakdown.get(&Priority::High), Some(&1));
        assert_eq!(breakdown.get(&Priority::Urgent), Some(&1));
        assert_eq!(breakdown.get(&Priority::Normal), Some(&1));
    }

    #[test]
    fn throughput_fills_week_gaps() {
        let now = Utc::now();
        let three_weeks_ago = now - chrono::TimeDelta::weeks(3);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", three_weeks_ago - chrono::TimeDelta::days(1), three_weeks_ago),
                card_completed_at("2", now - chrono::TimeDelta::days(1), now),
            ]),
        ]);

        let metrics = compute_metrics(&board, None);
        assert!(
            metrics.throughput_per_week.len() >= 3,
            "expected >= 3 weeks, got {}",
            metrics.throughput_per_week.len()
        );
        assert!(metrics.throughput_per_week.first().unwrap().1 > 0);
        assert!(metrics.throughput_per_week.last().unwrap().1 > 0);
    }

    #[test]
    fn format_text_contains_key_sections() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(3);

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![Card::new("1".into(), "A".into())]),
            make_column("done", "Done", vec![card_completed_at("2", created, now)]),
        ]);
        let metrics = compute_metrics(&board, None);
        let text = format_text(&metrics);

        assert!(text.contains("Board Metrics"));
        assert!(text.contains("Total completed: 1"));
        assert!(text.contains("Lead Time"));
        assert!(text.contains("Cycle Time"));
        assert!(text.contains("WIP per Column"));
    }

    #[test]
    fn format_csv_has_headers() {
        let board = make_board(vec![
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let csv = format_csv(&metrics);

        assert!(csv.contains("week,completed,arrived"));
        assert!(csv.contains("metric,value"));
        assert!(csv.contains("column,wip,limit,active"));
    }

    #[test]
    fn median_even_number_of_cards() {
        let now = Utc::now();
        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", now - chrono::TimeDelta::days(2), now),
                card_completed_at("2", now - chrono::TimeDelta::days(4), now),
                card_completed_at("3", now - chrono::TimeDelta::days(6), now),
                card_completed_at("4", now - chrono::TimeDelta::days(8), now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.lead_avg_days - 5.0).abs() < 0.1);
        assert!((ts.lead_median_days - 5.0).abs() < 0.1);
        assert!((ts.lead_min_days - 2.0).abs() < 0.1);
        assert!((ts.lead_max_days - 8.0).abs() < 0.1);
    }

    #[test]
    fn board_created_at_used_as_effective_since() {
        let now = Utc::now();
        let created_at = now - chrono::TimeDelta::weeks(4);
        let old_completion = now - chrono::TimeDelta::weeks(6);
        let recent_completion = now - chrono::TimeDelta::weeks(2);

        let mut board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", old_completion - chrono::TimeDelta::days(1), old_completion),
                card_completed_at("2", recent_completion - chrono::TimeDelta::days(1), recent_completion),
            ]),
        ]);
        board.created_at = Some(created_at);

        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 1);
    }

    #[test]
    fn multiple_completions_same_week() {
        let now = Utc::now();
        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", now - chrono::TimeDelta::days(2), now),
                card_completed_at("2", now - chrono::TimeDelta::days(3), now),
                card_completed_at("3", now - chrono::TimeDelta::days(1), now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 3);
        assert_eq!(metrics.throughput_per_week.len(), 1);
        assert_eq!(metrics.throughput_per_week[0].1, 3);
    }

    #[test]
    fn cards_in_non_done_columns_not_counted() {
        let now = Utc::now();
        let mut rogue_card = Card::new("1".into(), "Rogue".into());
        rogue_card.completed = Some(now);

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![rogue_card]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.total_completed, 0);
    }

    #[test]
    fn format_csv_includes_time_metrics() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(3);

        let board = make_board(vec![
            make_column("done", "Done", vec![card_completed_at("1", created, now)]),
        ]);
        let metrics = compute_metrics(&board, None);
        let csv = format_csv(&metrics);

        assert!(csv.contains("lead_avg_days,"));
        assert!(csv.contains("lead_p85_days,"));
        assert!(csv.contains("cycle_avg_days,"));
        assert!(csv.contains("cycle_p85_days,"));
    }

    #[test]
    fn format_text_no_throughput_when_no_completions() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![Card::new("1".into(), "A".into())]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let text = format_text(&metrics);

        assert!(text.contains("Total completed: 0"));
        assert!(!text.contains("Throughput"));
    }

    // ── Lead time vs cycle time tests ──

    #[test]
    fn lead_time_uses_created() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(10);
        let started = now - chrono::TimeDelta::days(5);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_started_completed("1", created, started, now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.lead_avg_days - 10.0).abs() < 0.1, "lead time should use created, got {}", ts.lead_avg_days);
    }

    #[test]
    fn cycle_time_uses_started() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(10);
        let started = now - chrono::TimeDelta::days(5);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_started_completed("1", created, started, now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.cycle_avg_days - 5.0).abs() < 0.1, "cycle time should use started, got {}", ts.cycle_avg_days);
    }

    #[test]
    fn cycle_time_falls_back_to_created() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(7);

        // Card without started (legacy card)
        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", created, now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        // cycle time should fall back to lead time
        assert!((ts.cycle_avg_days - ts.lead_avg_days).abs() < 0.01);
    }

    #[test]
    fn lead_and_cycle_differ_when_started_set() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(10);
        let started = now - chrono::TimeDelta::days(3);

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_started_completed("1", created, started, now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.lead_avg_days - 10.0).abs() < 0.1);
        assert!((ts.cycle_avg_days - 3.0).abs() < 0.1);
        assert!(ts.lead_avg_days > ts.cycle_avg_days);
    }

    // ── P85 tests ──

    #[test]
    fn p85_calculation_correct() {
        let now = Utc::now();
        // 20 cards with lead times 1..=20 days
        let cards: Vec<Card> = (1..=20).map(|d| {
            card_completed_at(&d.to_string(), now - chrono::TimeDelta::days(d), now)
        }).collect();

        let board = make_board(vec![make_column("done", "Done", cards)]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        // sorted: [1, 2, ..., 20]. p85 index = (20 * 85 / 100).min(19) = 17 → value = 18
        assert!((ts.lead_p85_days - 18.0).abs() < 0.2, "p85 expected ~18, got {}", ts.lead_p85_days);
    }

    #[test]
    fn p85_single_card() {
        let now = Utc::now();
        let created = now - chrono::TimeDelta::days(5);

        let board = make_board(vec![
            make_column("done", "Done", vec![card_completed_at("1", created, now)]),
        ]);
        let metrics = compute_metrics(&board, None);
        let ts = metrics.time_stats.unwrap();
        assert!((ts.lead_p85_days - 5.0).abs() < 0.1);
    }

    // ── Active WIP tests ──

    #[test]
    fn active_wip_excludes_backlog_and_done() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![
                Card::new("1".into(), "A".into()),
                Card::new("2".into(), "B".into()),
            ]),
            make_column("in-progress", "In Progress", vec![
                Card::new("3".into(), "C".into()),
            ]),
            make_column("review", "Review", vec![
                Card::new("4".into(), "D".into()),
                Card::new("5".into(), "E".into()),
            ]),
            make_column("done", "Done", vec![
                Card::new("6".into(), "F".into()),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        // Active = in-progress (1) + review (2) = 3
        assert_eq!(metrics.active_wip_total, 3);
        assert!(!metrics.wip_per_column[0].is_active); // backlog
        assert!(metrics.wip_per_column[1].is_active);  // in-progress
        assert!(metrics.wip_per_column[2].is_active);  // review
        assert!(!metrics.wip_per_column[3].is_active); // done
    }

    #[test]
    fn wip_entry_includes_limit() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column_with_limit("in-progress", "In Progress", vec![
                Card::new("1".into(), "A".into()),
            ], 3),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.wip_per_column[1].wip_limit, Some(3));
        assert_eq!(metrics.wip_per_column[1].count, 1);
    }

    #[test]
    fn wip_over_limit_detection() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column_with_limit("in-progress", "In Progress", vec![
                Card::new("1".into(), "A".into()),
                Card::new("2".into(), "B".into()),
                Card::new("3".into(), "C".into()),
                Card::new("4".into(), "D".into()),
            ], 3),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let entry = &metrics.wip_per_column[1];
        assert_eq!(entry.count, 4);
        assert_eq!(entry.wip_limit, Some(3));
        assert!(entry.wip_limit.is_some_and(|l| entry.count as u32 >= l));
    }

    // ── Blocked tests ──

    #[test]
    fn blocked_count_only_active_columns() {
        let mut blocked_in_backlog = Card::new("1".into(), "A".into());
        blocked_in_backlog.blocked = true;
        let mut blocked_in_progress = Card::new("2".into(), "B".into());
        blocked_in_progress.blocked = true;
        let not_blocked = Card::new("3".into(), "C".into());

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![blocked_in_backlog]),
            make_column("in-progress", "In Progress", vec![blocked_in_progress, not_blocked]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        // Only the one in in-progress counts (backlog excluded)
        assert_eq!(metrics.blocked_count, 1);
    }

    #[test]
    fn blocked_pct_calculation() {
        let mut blocked = Card::new("1".into(), "A".into());
        blocked.blocked = true;

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("in-progress", "In Progress", vec![
                blocked,
                Card::new("2".into(), "B".into()),
                Card::new("3".into(), "C".into()),
                Card::new("4".into(), "D".into()),
                Card::new("5".into(), "E".into()),
            ]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.blocked_count, 1);
        assert!((metrics.blocked_pct - 20.0).abs() < 0.1, "expected 20%, got {}", metrics.blocked_pct);
    }

    #[test]
    fn blocked_zero_when_no_active_wip() {
        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![Card::new("1".into(), "A".into())]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert_eq!(metrics.blocked_count, 0);
        assert!((metrics.blocked_pct - 0.0).abs() < 0.01);
    }

    // ── Work item age tests ──

    #[test]
    fn work_item_age_calculates_from_started() {
        let now = Utc::now();
        let mut card = Card::new("1".into(), "Active".into());
        card.created = now - chrono::TimeDelta::days(10);
        card.started = Some(now - chrono::TimeDelta::days(5));

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("in-progress", "In Progress", vec![card]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let wia = metrics.work_item_age.unwrap();
        assert_eq!(wia.count, 1);
        assert!((wia.avg_age_days - 5.0).abs() < 0.2, "age should use started, got {}", wia.avg_age_days);
    }

    #[test]
    fn work_item_age_falls_back_to_created() {
        let now = Utc::now();
        let mut card = Card::new("1".into(), "Active".into());
        card.created = now - chrono::TimeDelta::days(7);
        // No started set

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("in-progress", "In Progress", vec![card]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let wia = metrics.work_item_age.unwrap();
        assert!((wia.avg_age_days - 7.0).abs() < 0.2, "age should fall back to created, got {}", wia.avg_age_days);
    }

    #[test]
    fn aging_cards_flagged_when_exceeding_p85() {
        let now = Utc::now();

        // Create done cards with known cycle times (1..=10 days) to establish p85
        let done_cards: Vec<Card> = (1..=10).map(|d| {
            let created = now - chrono::TimeDelta::days(d);
            let started = created + chrono::TimeDelta::days(0); // started = created, so cycle = lead
            card_started_completed(&d.to_string(), created, started, now)
        }).collect();

        // p85 of [1,2,3,4,5,6,7,8,9,10] = sorted[8] = 9 days
        // Active card with age 15 days should be flagged
        let mut old_card = Card::new("99".into(), "Old task".into());
        old_card.created = now - chrono::TimeDelta::days(15);
        old_card.started = Some(now - chrono::TimeDelta::days(15));

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("in-progress", "In Progress", vec![old_card]),
            make_column("done", "Done", done_cards),
        ]);
        let metrics = compute_metrics(&board, None);
        let wia = metrics.work_item_age.unwrap();
        assert_eq!(wia.aging_cards.len(), 1);
        assert_eq!(wia.aging_cards[0].id, "99");
    }

    #[test]
    fn no_aging_when_no_time_stats() {
        // No done cards → no p85 → no aging threshold
        let mut card = Card::new("1".into(), "Active".into());
        card.started = Some(Utc::now() - chrono::TimeDelta::days(100));

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![]),
            make_column("in-progress", "In Progress", vec![card]),
            make_column("done", "Done", vec![]),
        ]);
        let metrics = compute_metrics(&board, None);
        let wia = metrics.work_item_age.unwrap();
        assert!(wia.aging_cards.is_empty(), "no aging without p85 threshold");
    }

    // ── Arrival rate tests ──

    #[test]
    fn arrival_rate_counts_all_columns() {
        let now = Utc::now();
        let mut card_backlog = Card::new("1".into(), "A".into());
        card_backlog.created = now - chrono::TimeDelta::days(1);
        let mut card_ip = Card::new("2".into(), "B".into());
        card_ip.created = now - chrono::TimeDelta::days(1);
        let mut card_done = Card::new("3".into(), "C".into());
        card_done.created = now - chrono::TimeDelta::days(1);
        card_done.completed = Some(now);

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![card_backlog]),
            make_column("in-progress", "In Progress", vec![card_ip]),
            make_column("done", "Done", vec![card_done]),
        ]);
        let metrics = compute_metrics(&board, None);
        // All 3 cards created in the same week
        let total_arrived: u32 = metrics.arrival_per_week.iter().map(|(_, c)| *c).sum();
        assert_eq!(total_arrived, 3);
    }

    #[test]
    fn arrival_rate_respects_since_window() {
        let now = Utc::now();
        let mut old_card = Card::new("1".into(), "Old".into());
        old_card.created = now - chrono::TimeDelta::weeks(10);
        let mut recent_card = Card::new("2".into(), "New".into());
        recent_card.created = now - chrono::TimeDelta::days(1);

        let board = make_board(vec![
            make_column("backlog", "Backlog", vec![old_card, recent_card]),
            make_column("done", "Done", vec![]),
        ]);
        let since = now - chrono::TimeDelta::weeks(2);
        let metrics = compute_metrics(&board, Some(since));
        let total_arrived: u32 = metrics.arrival_per_week.iter().map(|(_, c)| *c).sum();
        assert_eq!(total_arrived, 1, "only cards after since should be counted");
    }

    // ── Throughput variability tests ──

    #[test]
    fn throughput_stddev_calculation() {
        let now = Utc::now();
        // 2 cards in week 1, 4 cards in week 2 → counts [2, 4], mean=3, stddev=1
        let week1 = now - chrono::TimeDelta::weeks(1);
        let week2 = now;

        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", week1 - chrono::TimeDelta::days(5), week1),
                card_completed_at("2", week1 - chrono::TimeDelta::days(3), week1),
                card_completed_at("3", week2 - chrono::TimeDelta::days(2), week2),
                card_completed_at("4", week2 - chrono::TimeDelta::days(1), week2),
                card_completed_at("5", week2 - chrono::TimeDelta::days(1), week2),
                card_completed_at("6", week2 - chrono::TimeDelta::days(1), week2),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert!(metrics.throughput_stddev.is_some());
    }

    #[test]
    fn throughput_stddev_none_for_single_week() {
        let now = Utc::now();
        let board = make_board(vec![
            make_column("done", "Done", vec![
                card_completed_at("1", now - chrono::TimeDelta::days(1), now),
            ]),
        ]);
        let metrics = compute_metrics(&board, None);
        assert!(metrics.throughput_stddev.is_none());
    }

    // ── Math helper tests ──

    #[test]
    fn avg_empty_returns_zero() {
        assert_eq!(avg(&[]), 0.0);
    }

    #[test]
    fn avg_single_value() {
        assert_eq!(avg(&[5.0]), 5.0);
    }

    #[test]
    fn avg_multiple_values() {
        assert!((avg(&[2.0, 4.0, 6.0]) - 4.0).abs() < f64::EPSILON);
    }

    #[test]
    fn median_empty_returns_zero() {
        assert_eq!(median(&[]), 0.0);
    }

    #[test]
    fn median_single_value() {
        assert_eq!(median(&[5.0]), 5.0);
    }

    #[test]
    fn median_odd_count() {
        assert_eq!(median(&[1.0, 2.0, 3.0]), 2.0);
    }

    #[test]
    fn percentile_empty_returns_zero() {
        assert_eq!(percentile(&[], 85), 0.0);
    }

    #[test]
    fn percentile_single_value() {
        assert_eq!(percentile(&[10.0], 85), 10.0);
    }

    #[test]
    fn percentile_0_returns_first() {
        assert_eq!(percentile(&[1.0, 2.0, 3.0], 0), 1.0);
    }

    #[test]
    fn percentile_100_returns_last() {
        assert_eq!(percentile(&[1.0, 2.0, 3.0], 100), 3.0);
    }

    // ── Week helper tests ──

    #[test]
    fn format_week_output() {
        let d = NaiveDate::from_ymd_opt(2025, 1, 6).unwrap(); // Monday of W02
        assert_eq!(format_week(d.iso_week()), "2025-W02");
    }

    #[test]
    fn monday_of_week_is_monday() {
        let d = NaiveDate::from_ymd_opt(2025, 6, 12).unwrap(); // Thursday
        let mon = monday_of_week(d.iso_week());
        assert_eq!(mon.weekday(), chrono::Weekday::Mon);
        assert_eq!(mon, NaiveDate::from_ymd_opt(2025, 6, 9).unwrap());
    }

    #[test]
    fn fill_week_gaps_inserts_missing() {
        use std::collections::BTreeMap;
        let d1 = NaiveDate::from_ymd_opt(2025, 6, 2).unwrap(); // W23
        let d3 = NaiveDate::from_ymd_opt(2025, 6, 16).unwrap(); // W25
        let mut counts = BTreeMap::new();
        counts.insert(d1.iso_week(), 3);
        counts.insert(d3.iso_week(), 1);
        let result = fill_week_gaps(&counts);
        assert_eq!(result.len(), 3); // W23, W24, W25
        assert_eq!(result[0].1, 3);
        assert_eq!(result[1].1, 0); // gap filled
        assert_eq!(result[2].1, 1);
    }

    #[test]
    fn oldest_card_created_empty_board() {
        let board = make_board(vec![make_column("backlog", "Backlog", vec![])]);
        assert!(oldest_card_created(&board).is_none());
    }
}
