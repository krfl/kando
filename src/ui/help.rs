use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;

pub fn render_help(f: &mut Frame, area: Rect) {
    let panel_area = super::centered_rect(area, 70, 85, 60, 24);

    f.render_widget(Clear, panel_area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_type(ratatui::widgets::BorderType::Rounded)
        .border_style(Style::default().fg(Theme::FG))
        .title(Span::styled(
            " Kando Help ",
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
    let heading = Style::default()
        .fg(Theme::FG)
        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED);

    let lines = vec![
        // Normal mode
        Line::from(Span::styled("Normal Mode", heading)),
        Line::from(vec![
            Span::styled("  h / l       ", key),
            Span::styled("Switch columns", dim),
        ]),
        Line::from(vec![
            Span::styled("  j / k       ", key),
            Span::styled("Move between cards", dim),
        ]),
        Line::from(vec![
            Span::styled("  H / L       ", key),
            Span::styled("Move card left/right", dim),
        ]),
        Line::from(vec![
            Span::styled("  Enter       ", key),
            Span::styled("Open card detail", dim),
        ]),
        Line::from(vec![
            Span::styled("  /           ", key),
            Span::styled("Search cards", dim),
        ]),
        Line::from(vec![
            Span::styled("  Esc         ", key),
            Span::styled("Clear filters", dim),
        ]),
        Line::from(vec![
            Span::styled("  q           ", key),
            Span::styled("Quit", dim),
        ]),
        Line::from(""),
        // Commands
        Line::from(Span::styled("Commands (Space)", heading)),
        Line::from(vec![
            Span::styled("  n           ", key),
            Span::styled("New card", dim),
        ]),
        Line::from(vec![
            Span::styled("  d           ", key),
            Span::styled("Delete card", dim),
        ]),
        Line::from(vec![
            Span::styled("  e           ", key),
            Span::styled("Edit in $EDITOR", dim),
        ]),
        Line::from(vec![
            Span::styled("  t           ", key),
            Span::styled("Edit tags", dim),
        ]),
        Line::from(vec![
            Span::styled("  p           ", key),
            Span::styled("Set priority", dim),
        ]),
        Line::from(vec![
            Span::styled("  m           ", key),
            Span::styled("Move to column", dim),
        ]),
        Line::from(vec![
            Span::styled("  f           ", key),
            Span::styled("Filter by tag", dim),
        ]),
        Line::from(vec![
            Span::styled("  b           ", key),
            Span::styled("Toggle blocker", dim),
        ]),
        Line::from(vec![
            Span::styled("  r           ", key),
            Span::styled("Reload board", dim),
        ]),
        Line::from(vec![
            Span::styled("  /           ", key),
            Span::styled("Search all", dim),
        ]),
        Line::from(vec![
            Span::styled("  ?           ", key),
            Span::styled("This help", dim),
        ]),
        Line::from(""),
        // Goto
        Line::from(Span::styled("Goto (g)", heading)),
        Line::from(vec![
            Span::styled("  1-9         ", key),
            Span::styled("Jump to column", dim),
        ]),
        Line::from(vec![
            Span::styled("  g           ", key),
            Span::styled("First card", dim),
        ]),
        Line::from(vec![
            Span::styled("  e           ", key),
            Span::styled("Last card", dim),
        ]),
        Line::from(vec![
            Span::styled("  b           ", key),
            Span::styled("Backlog", dim),
        ]),
        Line::from(vec![
            Span::styled("  d           ", key),
            Span::styled("Done", dim),
        ]),
        Line::from(""),
        // View
        Line::from(Span::styled("View (z)", heading)),
        Line::from(vec![
            Span::styled("  c           ", key),
            Span::styled("Collapse empty column", dim),
        ]),
        Line::from(vec![
            Span::styled("  a           ", key),
            Span::styled("Show all columns", dim),
        ]),
        Line::from(vec![
            Span::styled("  w           ", key),
            Span::styled("Toggle WIP display", dim),
        ]),
        Line::from(vec![
            Span::styled("  z           ", key),
            Span::styled("Center on selected card", dim),
        ]),
        Line::from(vec![
            Span::styled("  h           ", key),
            Span::styled("Toggle hidden columns", dim),
        ]),
        Line::from(""),
        // Card detail
        Line::from(Span::styled("Card Detail", heading)),
        Line::from(vec![
            Span::styled("  j / k       ", key),
            Span::styled("Next/prev card", dim),
        ]),
        Line::from(vec![
            Span::styled("  J / K       ", key),
            Span::styled("Scroll content", dim),
        ]),
        Line::from(vec![
            Span::styled("  e           ", key),
            Span::styled("Edit in $EDITOR", dim),
        ]),
        Line::from(vec![
            Span::styled("  t           ", key),
            Span::styled("Edit tags", dim),
        ]),
        Line::from(vec![
            Span::styled("  p           ", key),
            Span::styled("Cycle priority", dim),
        ]),
        Line::from(vec![
            Span::styled("  b           ", key),
            Span::styled("Toggle blocker", dim),
        ]),
        Line::from(vec![
            Span::styled("  Esc         ", key),
            Span::styled("Close", dim),
        ]),
        Line::from(""),
        Line::from(Span::styled(
            "Press Esc to close",
            Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
        )),
    ];

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });
    f.render_widget(paragraph, inner);
}
