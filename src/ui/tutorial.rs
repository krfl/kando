use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Padding, Paragraph, Wrap};
use ratatui::Frame;

use super::theme::Theme;

pub fn render_tutorial(f: &mut Frame, area: Rect) {
    let panel_area = super::centered_rect(area, 50, 80, 44, 20);

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

    let key_style = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);
    let dim = Theme::dim_style();
    let heading = Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD);

    let lines = vec![
        Line::from(Span::styled("Navigation", heading)),
        Line::from(vec![
            Span::styled("  h / l       ", key_style),
            Span::styled("Switch columns", dim),
        ]),
        Line::from(vec![
            Span::styled("  j / k       ", key_style),
            Span::styled("Move between cards", dim),
        ]),
        Line::from(vec![
            Span::styled("  Enter       ", key_style),
            Span::styled("Open card detail", dim),
        ]),
        Line::from(vec![
            Span::styled("  H / L       ", key_style),
            Span::styled("Move card left/right", dim),
        ]),
        Line::from(""),
        Line::from(Span::styled("Commands (Space)", heading)),
        Line::from(vec![
            Span::styled("  n           ", key_style),
            Span::styled("Create new card", dim),
        ]),
        Line::from(vec![
            Span::styled("  d           ", key_style),
            Span::styled("Delete card", dim),
        ]),
        Line::from(vec![
            Span::styled("  e           ", key_style),
            Span::styled("Edit in $EDITOR", dim),
        ]),
        Line::from(vec![
            Span::styled("  p           ", key_style),
            Span::styled("Set priority", dim),
        ]),
        Line::from(vec![
            Span::styled("  m           ", key_style),
            Span::styled("Move to column", dim),
        ]),
        Line::from(vec![
            Span::styled("  f           ", key_style),
            Span::styled("Filter by tag", dim),
        ]),
        Line::from(""),
        Line::from(Span::styled("More", heading)),
        Line::from(vec![
            Span::styled("  g           ", key_style),
            Span::styled("Goto mode (jump to columns)", dim),
        ]),
        Line::from(vec![
            Span::styled("  z           ", key_style),
            Span::styled("View mode (collapse, hidden cols)", dim),
        ]),
        Line::from(vec![
            Span::styled("  /           ", key_style),
            Span::styled("Search cards", dim),
        ]),
        Line::from(vec![
            Span::styled("  Space ?     ", key_style),
            Span::styled("Full help", dim),
        ]),
        Line::from(vec![
            Span::styled("  q           ", key_style),
            Span::styled("Quit", dim),
        ]),
        Line::from(""),
        Line::from(Span::styled(
            "Press any key to start",
            Style::default().fg(Theme::FG).add_modifier(Modifier::BOLD),
        )),
    ];

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });
    f.render_widget(paragraph, inner);
}
