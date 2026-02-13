use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::board::age::{format_age, staleness, Staleness};
use crate::board::{Card, Policies};
use chrono::{DateTime, Utc};

pub fn render_card_detail(f: &mut Frame, area: Rect, card: &Card, policies: &Policies, scroll: u16, now: DateTime<Utc>) {

    let panel_area = super::centered_rect(area, 60, 80, 40, 10);

    // Clear background
    f.render_widget(Clear, panel_area);

    let stale = staleness(card, policies, now);
    let border_color = if card.blocked {
        Theme::BLOCKER
    } else {
        match stale {
            Staleness::VeryStale => Theme::BUBBLE_UP_CRITICAL,
            Staleness::Stale => Theme::BUBBLE_UP_WARN,
            Staleness::Fresh => Theme::FG,
        }
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(border_color))
        .title(Span::styled(
            format!(" {} ", card.id),
            Style::default()
                .fg(Theme::FG)
                .add_modifier(Modifier::BOLD),
        ))
        .padding(Padding::new(2, 2, 1, 1));

    let inner = block.inner(panel_area);
    f.render_widget(block, panel_area);

    if inner.height == 0 {
        return;
    }

    let mut lines = Vec::new();

    // Title
    lines.push(Line::from(Span::styled(
        &card.title,
        Style::default()
            .fg(Theme::FG)
            .add_modifier(Modifier::BOLD),
    )));
    lines.push(Line::from(""));

    // Metadata
    let age = format_age(card.created, now);
    lines.push(Line::from(vec![
        Span::styled("Age:      ", Theme::dim_style()),
        Span::raw(age),
    ]));

    // Priority with glyph
    let priority_spans = {
        let mut spans = vec![Span::styled("Priority: ", Theme::dim_style())];
        if let Some(sym) = card.priority.symbol() {
            let color = Theme::priority_color(card.priority);
            spans.push(Span::styled(format!("{sym} "), Style::default().fg(color)));
        }
        spans.push(Span::raw(card.priority.as_str()));
        spans
    };
    lines.push(Line::from(priority_spans));

    if card.blocked {
        lines.push(Line::from(vec![
            Span::styled("Status:   ", Theme::dim_style()),
            Span::styled(
                "\u{f05e} BLOCKED", // nf-fa-ban
                Style::default().fg(Theme::BLOCKER),
            ),
        ]));
    }

    // Assignees
    if !card.assignees.is_empty() {
        let mut spans = vec![Span::styled("Assigned: ", Theme::dim_style())];
        for (i, assignee) in card.assignees.iter().enumerate() {
            if i > 0 {
                spans.push(Span::raw(", "));
            }
            spans.push(Span::styled(
                assignee.as_str(),
                Style::default().fg(Theme::ASSIGNEE),
            ));
        }
        lines.push(Line::from(spans));
    }

    // Tags
    if !card.tags.is_empty() {
        let mut tag_spans = vec![Span::styled("Tags:     ", Theme::dim_style())];
        for (i, tag) in card.tags.iter().enumerate() {
            if i > 0 {
                tag_spans.push(Span::raw(" · "));
            }
            tag_spans.push(Span::styled(
                tag.as_str(),
                Style::default().fg(Theme::tag_color(tag)),
            ));
        }
        lines.push(Line::from(tag_spans));
    }

    // Timestamps
    lines.push(Line::from(vec![
        Span::styled("Created:  ", Theme::dim_style()),
        Span::styled(
            card.created.format("%Y-%m-%d %H:%M").to_string(),
            Theme::dim_style(),
        ),
    ]));
    lines.push(Line::from(vec![
        Span::styled("Updated:  ", Theme::dim_style()),
        Span::styled(
            card.updated.format("%Y-%m-%d %H:%M").to_string(),
            Theme::dim_style(),
        ),
    ]));

    // Body
    if !card.body.is_empty() {
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(
            "─".repeat(inner.width as usize),
            Theme::dim_style(),
        )));
        lines.push(Line::from(""));
        for body_line in card.body.lines() {
            lines.push(Line::from(body_line.to_string()));
        }
    }

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));
    f.render_widget(paragraph, inner);
}
