use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;

pub fn render_tutorial(f: &mut Frame, area: Rect, scroll: &mut u16) {
    let panel_area = super::overlay_rect(area);

    f.render_widget(Clear, panel_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            " Welcome to Kando! ",
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

    let key = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();
    let heading = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);

    let lines = vec![
        Line::from(Span::styled("j/k scroll  |  Esc close", key)),
        Line::from(""),
        // ── Plain-text by design ──
        Line::from(Span::styled("Plain-text by design", heading)),
        Line::from(Span::styled(
            "Your board lives in .kando/ — cards are markdown files",
            dim,
        )),
        Line::from(Span::styled(
            "with TOML frontmatter, columns are directories. Everything",
            dim,
        )),
        Line::from(Span::styled(
            "is git-friendly: commit your board, diff changes, and",
            dim,
        )),
        Line::from(Span::styled(
            "edit cards with any text editor.",
            dim,
        )),
        Line::from(""),
        // ── Workflow & lifecycle ──
        Line::from(Span::styled("Workflow & lifecycle", heading)),
        Line::from(Span::styled(
            "Cards flow Backlog \u{2192} In Progress \u{2192} Done \u{2192} Archive.",
            dim,
        )),
        Line::from(Span::styled(
            "Moving a card out of Backlog marks it as started (the",
            dim,
        )),
        Line::from(Span::styled(
            "commitment point). Reaching Done records a completion",
            dim,
        )),
        Line::from(Span::styled(
            "timestamp. Both feed into board metrics (m).",
            dim,
        )),
        Line::from(""),
        // ── Board policies ──
        Line::from(Span::styled("Board policies", heading)),
        Line::from(Span::styled(
            "Policies in config.toml keep the board healthy:",
            dim,
        )),
        Line::from(vec![
            Span::styled("  Stale warnings  ", key),
            Span::styled("Cards untouched for 7d get flagged", dim),
        ]),
        Line::from(vec![
            Span::styled("  Auto-close      ", key),
            Span::styled("Untouched 30d \u{2192} moved to archive", dim),
        ]),
        Line::from(vec![
            Span::styled("  WIP limits      ", key),
            Span::styled("Warn when a column is at capacity", dim),
        ]),
        Line::from(vec![
            Span::styled("  Trash purge     ", key),
            Span::styled("Deleted cards purged after 30d", dim),
        ]),
        Line::from(Span::styled(
            "All thresholds are configurable; set to 0 to disable.",
            dim,
        )),
        Line::from(""),
        // ── Cards ──
        Line::from(Span::styled("Cards", heading)),
        Line::from(Span::styled(
            "Each card has a title, markdown body, priority (urgent/",
            dim,
        )),
        Line::from(Span::styled(
            "high/normal/low), tags, assignees, and a blocked flag.",
            dim,
        )),
        Line::from(Span::styled(
            "Cards sort by priority first, then most recently updated.",
            dim,
        )),
        Line::from(""),
        // ── Git sync ──
        Line::from(Span::styled("Git sync", heading)),
        Line::from(Span::styled(
            "Set sync_branch in config.toml to enable automatic",
            dim,
        )),
        Line::from(Span::styled(
            "syncing. Kando uses a shadow clone to pull every 30s",
            dim,
        )),
        Line::from(Span::styled(
            "and push after every change \u{2014} no manual git needed.",
            dim,
        )),
        Line::from(Span::styled(
            "Works offline: changes queue and sync when back online.",
            dim,
        )),
        Line::from(""),
        // ── Getting around ──
        Line::from(Span::styled("Getting around", heading)),
        Line::from(vec![
            Span::styled("  h/l  j/k      ", key),
            Span::styled("Navigate columns and cards", dim),
        ]),
        Line::from(vec![
            Span::styled("  H/L           ", key),
            Span::styled("Move card between columns", dim),
        ]),
        Line::from(vec![
            Span::styled("  Space         ", key),
            Span::styled("Command menu (create, delete, edit, \u{2026})", dim),
        ]),
        Line::from(vec![
            Span::styled("  Enter         ", key),
            Span::styled("Open card detail view", dim),
        ]),
        Line::from(vec![
            Span::styled("  /             ", key),
            Span::styled("Fuzzy search (supports !neg and @user)", dim),
        ]),
        Line::from(vec![
            Span::styled("  f             ", key),
            Span::styled("Filter by tag, assignee, or staleness", dim),
        ]),
        Line::from(vec![
            Span::styled("  ?             ", key),
            Span::styled("Full keybinding reference", dim),
        ]),
        Line::from(""),
        // ── Pipe to external commands ──
        Line::from(Span::styled("Pipe to external commands", heading)),
        Line::from(Span::styled(
            "Press | to pipe a card's file contents to any shell",
            dim,
        )),
        Line::from(Span::styled(
            "command. The command receives the full card (frontmatter",
            dim,
        )),
        Line::from(Span::styled(
            "+ body) on stdin, plus KANDO_CARD_* env vars.",
            dim,
        )),
        Line::from(vec![
            Span::styled("  | pbcopy      ", key),
            Span::styled("Copy card to clipboard", dim),
        ]),
        Line::from(vec![
            Span::styled("  | wc -l       ", key),
            Span::styled("Count lines in card", dim),
        ]),
        Line::from(vec![
            Span::styled("  | my-script   ", key),
            Span::styled("Send to your own tool", dim),
        ]),
        Line::from(""),
        // ── Footer ──
        Line::from(Span::styled("Press Esc to start using your board", key)),
    ];

    let max_scroll = (lines.len() as u16).saturating_sub(inner.height);
    *scroll = (*scroll).min(max_scroll);

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((*scroll, 0));
    f.render_widget(paragraph, inner);
}
