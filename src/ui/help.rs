use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;
use crate::app::HelpPage;
use crate::input::keymap;

pub fn render_help(f: &mut Frame, area: Rect, scroll: &mut u16, page: &HelpPage) {
    let panel_area = super::overlay_rect(area);

    f.render_widget(Clear, panel_area);

    let active = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD);
    let inactive = Theme::dim_style();

    let (keys_style, concepts_style) = match page {
        HelpPage::Keybindings => (active, inactive),
        HelpPage::Concepts => (inactive, active),
    };

    let title = Line::from(vec![
        Span::raw(" "),
        Span::styled("Keybindings", keys_style),
        Span::styled(" | ", inactive),
        Span::styled("Concepts", concepts_style),
        Span::raw(" "),
    ]);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(title)
        .padding(Padding::new(2, 2, 1, 1));

    let inner = block.inner(panel_area);
    f.render_widget(block, panel_area);

    if inner.height == 0 {
        return;
    }

    let key = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();

    let mut lines = vec![
        Line::from(Span::styled(
            "j/k scroll  |  ?/Tab switch  |  Esc close",
            Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
    ];

    match page {
        HelpPage::Keybindings => build_keybindings(&mut lines, key, dim),
        HelpPage::Concepts => build_concepts(&mut lines, key, dim),
    }

    let max_scroll = (lines.len() as u16).saturating_sub(inner.height);
    *scroll = (*scroll).min(max_scroll);

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .scroll((*scroll, 0));
    f.render_widget(paragraph, inner);
}

fn build_keybindings(lines: &mut Vec<Line<'static>>, key: Style, dim: Style) {
    let heading = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED);

    for group in keymap::HELP_GROUPS {
        lines.push(Line::from(Span::styled(group.name, heading)));
        for b in group.bindings {
            lines.push(Line::from(vec![
                Span::styled(format!("  {:<14}", b.key), key),
                Span::styled(b.description, dim),
            ]));
        }
        lines.push(Line::from(""));
    }
}

fn build_concepts(lines: &mut Vec<Line<'static>>, key: Style, dim: Style) {
    let heading = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD);

    lines.push(Line::from(Span::styled("Plain-text by design", heading)));
    lines.push(Line::from(Span::styled(
        "Board lives in .kando/ — cards are markdown files with TOML",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "frontmatter, columns are directories. Git-friendly and",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "editable with any text editor.",
        dim,
    )));
    lines.push(Line::from(""));

    lines.push(Line::from(Span::styled("Workflow lifecycle", heading)));
    lines.push(Line::from(Span::styled(
        "Cards flow Backlog → In Progress → Done → Archive.",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "Moving out of Backlog marks start; reaching Done records",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "completion. Both feed into board metrics (m).",
        dim,
    )));
    lines.push(Line::from(""));

    lines.push(Line::from(Span::styled("Board policies", heading)));
    lines.push(Line::from(Span::styled(
        "Policies in config.toml keep the board healthy:",
        dim,
    )));
    lines.push(Line::from(vec![
        Span::styled("  Stale warnings  ", key),
        Span::styled("Cards untouched for 7d get flagged", dim),
    ]));
    lines.push(Line::from(vec![
        Span::styled("  Auto-close      ", key),
        Span::styled("Untouched 30d → moved to archive", dim),
    ]));
    lines.push(Line::from(vec![
        Span::styled("  WIP limits      ", key),
        Span::styled("Warn when a column is at capacity", dim),
    ]));
    lines.push(Line::from(vec![
        Span::styled("  Trash purge     ", key),
        Span::styled("Deleted cards purged after 30d", dim),
    ]));
    lines.push(Line::from(Span::styled(
        "All thresholds are configurable; set to 0 to disable.",
        dim,
    )));
    lines.push(Line::from(""));

    lines.push(Line::from(Span::styled("Cards", heading)));
    lines.push(Line::from(Span::styled(
        "Each card has a title, markdown body, priority, tags,",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "assignees, blocked flag, and due date. Sort by priority",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "then most recently updated.",
        dim,
    )));
    lines.push(Line::from(""));

    lines.push(Line::from(Span::styled("Git sync", heading)));
    lines.push(Line::from(Span::styled(
        "Set sync_branch in config.toml to enable automatic syncing.",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "Kando uses a shadow clone to pull every 30s and push after",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "every change. Works offline: changes queue and sync later.",
        dim,
    )));
    lines.push(Line::from(""));

    lines.push(Line::from(Span::styled("Pipe commands", heading)));
    lines.push(Line::from(Span::styled(
        "Press | to pipe a card to any shell command. The command",
        dim,
    )));
    lines.push(Line::from(Span::styled(
        "receives frontmatter + body on stdin, plus KANDO_CARD_*",
        dim,
    )));
    lines.push(Line::from(Span::styled("environment variables.", dim)));
}
