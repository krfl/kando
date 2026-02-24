use chrono::{DateTime, Utc};
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::board::metrics::compute_metrics;
use crate::board::Board;

pub fn render_metrics(f: &mut Frame, area: Rect, board: &Board, scroll: u16, _now: DateTime<Utc>) {
    let panel_area = super::centered_rect(area, 80, 90, 60, 20);

    f.render_widget(Clear, panel_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            " Board Metrics ",
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ))
        .padding(Padding::new(2, 2, 1, 1));

    let inner = block.inner(panel_area);
    f.render_widget(block, panel_area);

    if inner.height == 0 || inner.width == 0 {
        return;
    }

    let metrics = compute_metrics(board, None);

    let heading = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED);
    let label = Style::default().fg(Theme::DIM);
    let value = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();

    // ── Summary ──
    let mut lines: Vec<Line> = vec![
        Line::from(Span::styled("Summary", heading)),
        Line::from(""),
        Line::from(vec![
            Span::styled("  Since:           ", label),
            Span::styled(metrics.since.format("%Y-%m-%d").to_string(), value),
        ]),
        Line::from(vec![
            Span::styled("  Total completed: ", label),
            Span::styled(metrics.total_completed.to_string(), value),
        ]),
        Line::from(vec![
            Span::styled("  Active WIP:      ", label),
            Span::styled(metrics.active_wip_total.to_string(), value),
        ]),
    ];

    if metrics.blocked_count > 0 {
        lines.push(Line::from(vec![
            Span::styled("  Blocked:         ", label),
            Span::styled(
                format!("{} ({:.1}%)", metrics.blocked_count, metrics.blocked_pct),
                Style::default().fg(Theme::BLOCKER).add_modifier(Modifier::BOLD),
            ),
        ]));
    }

    if let Some(ref ts) = metrics.time_stats {
        lines.push(Line::from(vec![
            Span::styled("  Cycle time P85:  ", label),
            Span::styled(format!("{:.1} days", ts.cycle_p85_days), value),
        ]));
    }
    lines.push(Line::from(""));

    // ── WIP per Column ──
    lines.push(Line::from(Span::styled(
        format!("WIP per Column (Active: {})", metrics.active_wip_total),
        heading,
    )));
    lines.push(Line::from(""));

    let max_wip = metrics.wip_per_column.iter().map(|w| w.count).max().unwrap_or(1).max(1);
    let max_name_len = metrics
        .wip_per_column
        .iter()
        .map(|w| w.name.len())
        .max()
        .unwrap_or(0)
        .max(10);
    let bar_max = (inner.width as usize).saturating_sub(max_name_len + 19).min(40);
    for entry in &metrics.wip_per_column {
        let bar_len = if entry.count > 0 {
            (entry.count * bar_max) / max_wip
        } else {
            0
        }
        .max(if entry.count > 0 { 1 } else { 0 });
        let bar: String = "\u{2588}".repeat(bar_len);

        let over_limit = entry
            .wip_limit
            .is_some_and(|l| entry.count as u32 >= l);
        let color = if over_limit {
            Theme::WIP_OVER
        } else if entry.count == 0 {
            Theme::DIM
        } else {
            Theme::WIP_OK
        };

        let count_str = if let Some(limit) = entry.wip_limit {
            format!(" {}/{}", entry.count, limit)
        } else {
            format!(" {}", entry.count)
        };

        let name_label = format!("  {:<width$} ", entry.name, width = max_name_len);

        lines.push(Line::from(vec![
            Span::styled(name_label, label),
            Span::styled(bar, Style::default().fg(color)),
            Span::styled(count_str, if over_limit {
                Style::default().fg(Theme::WIP_OVER).add_modifier(Modifier::BOLD)
            } else {
                value
            }),
        ]));
    }
    lines.push(Line::from(""));

    // ── Throughput ──
    if !metrics.throughput_per_week.is_empty() {
        lines.push(Line::from(Span::styled("Throughput (cards/week)", heading)));
        lines.push(Line::from(""));

        let max_tp = metrics
            .throughput_per_week
            .iter()
            .map(|(_, c)| *c)
            .max()
            .unwrap_or(1)
            .max(1);
        let tp_bar_max = (inner.width as usize).saturating_sub(30).min(40);
        for (i, (week_label, count)) in metrics.throughput_per_week.iter().enumerate() {
            let bar_len = if *count > 0 {
                (*count as usize * tp_bar_max) / max_tp as usize
            } else {
                0
            }
            .max(if *count > 0 { 1 } else { 0 });
            let bar: String = "\u{2588}".repeat(bar_len);
            let color = if *count == 0 {
                Theme::DIM
            } else {
                Theme::WIP_OK
            };
            let arrival = metrics.arrival_per_week.get(i).map_or(0, |(_, c)| *c);
            lines.push(Line::from(vec![
                Span::styled(format!("  {:<10}", week_label), label),
                Span::styled(bar, Style::default().fg(color)),
                Span::styled(format!(" {count}"), value),
                Span::styled(format!("  ({arrival} arrived)"), dim),
            ]));
        }
        if let Some(stddev) = metrics.throughput_stddev {
            lines.push(Line::from(vec![
                Span::styled("  Variability: ", label),
                Span::styled(format!("stddev {:.1} cards/week", stddev), value),
            ]));
        }
        lines.push(Line::from(""));
    }

    // ── Lead Time ──
    if let Some(ref ts) = metrics.time_stats {
        lines.push(Line::from(Span::styled("Lead Time (created \u{2192} done)", heading)));
        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("  Average: ", label),
            Span::styled(format!("{:.1} days", ts.lead_avg_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Median:  ", label),
            Span::styled(format!("{:.1} days", ts.lead_median_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  P85:     ", label),
            Span::styled(format!("{:.1} days", ts.lead_p85_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Min:     ", label),
            Span::styled(format!("{:.1} days", ts.lead_min_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Max:     ", label),
            Span::styled(format!("{:.1} days", ts.lead_max_days), value),
        ]));
        lines.push(Line::from(""));

        // ── Cycle Time ──
        lines.push(Line::from(Span::styled("Cycle Time (started \u{2192} done)", heading)));
        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("  Average: ", label),
            Span::styled(format!("{:.1} days", ts.cycle_avg_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Median:  ", label),
            Span::styled(format!("{:.1} days", ts.cycle_median_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  P85:     ", label),
            Span::styled(format!("{:.1} days", ts.cycle_p85_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Min:     ", label),
            Span::styled(format!("{:.1} days", ts.cycle_min_days), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Max:     ", label),
            Span::styled(format!("{:.1} days", ts.cycle_max_days), value),
        ]));
        lines.push(Line::from(""));
    }

    // ── Work Item Age ──
    if let Some(ref wia) = metrics.work_item_age {
        lines.push(Line::from(Span::styled("Work Item Age", heading)));
        lines.push(Line::from(""));
        lines.push(Line::from(vec![
            Span::styled("  In-progress:  ", label),
            Span::styled(format!("{} items", wia.count), value),
        ]));
        lines.push(Line::from(vec![
            Span::styled("  Average age:  ", label),
            Span::styled(format!("{:.1} days", wia.avg_age_days), value),
        ]));
        if !wia.aging_cards.is_empty() {
            lines.push(Line::from(vec![
                Span::styled("  Aging (>P85): ", label),
                Span::styled(
                    format!("{} card(s)", wia.aging_cards.len()),
                    Style::default().fg(Theme::BUBBLE_UP_WARN).add_modifier(Modifier::BOLD),
                ),
            ]));
            for card in &wia.aging_cards {
                lines.push(Line::from(vec![
                    Span::styled("    ", dim),
                    Span::styled(
                        format!("#{} \"{}\" in {} ({:.1}d)", card.id, card.title, card.column, card.age_days),
                        Style::default().fg(Theme::BUBBLE_UP_WARN),
                    ),
                ]));
            }
        }
        lines.push(Line::from(""));
    }

    // ── Priority Breakdown ──
    if !metrics.priority_breakdown.is_empty() {
        lines.push(Line::from(Span::styled("Priority Breakdown", heading)));
        lines.push(Line::from(""));
        for (priority, count) in &metrics.priority_breakdown {
            let pcolor = Theme::priority_color(*priority);
            lines.push(Line::from(vec![
                Span::styled("  ", dim),
                Span::styled(format!("{:<10}", priority.as_str()), Style::default().fg(pcolor)),
                Span::styled(count.to_string(), value),
            ]));
        }
        lines.push(Line::from(""));
    }

    // ── Footer ──
    lines.push(Line::from(Span::styled(
        "J/K scroll  |  Esc close",
        dim,
    )));

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));
    f.render_widget(paragraph, inner);
}
