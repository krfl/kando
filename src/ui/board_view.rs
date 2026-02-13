use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use ratatui::widgets::{
    Block, Borders, Padding, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
};
use ratatui::Frame;

use super::theme::Theme;
use crate::app::AppState;
use crate::board::age::{format_age, staleness, Staleness};
use crate::board::{Board, Card};
use chrono::{DateTime, Utc};

pub fn render_board(f: &mut Frame, area: Rect, board: &Board, state: &AppState, now: DateTime<Utc>) {

    // Filter visible columns
    let visible_columns: Vec<(usize, &crate::board::Column)> = board
        .columns
        .iter()
        .enumerate()
        .filter(|(_, col)| !col.hidden || state.show_hidden_columns)
        .collect();

    if visible_columns.is_empty() {
        let msg = Paragraph::new("No visible columns. Press z then h to show hidden columns.");
        f.render_widget(msg, area);
        return;
    }

    // Split area evenly among visible columns
    let constraints: Vec<Constraint> = visible_columns
        .iter()
        .map(|_| Constraint::Ratio(1, visible_columns.len() as u32))
        .collect();
    let col_areas = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(constraints)
        .split(area);

    for (vis_idx, &(col_idx, col)) in visible_columns.iter().enumerate() {
        let is_focused = state.focused_column == col_idx;
        render_column(
            f,
            col_areas[vis_idx],
            col,
            is_focused,
            state,
            &board.policies,
            now,
        );
    }
}

fn render_column(
    f: &mut Frame,
    area: Rect,
    col: &crate::board::Column,
    is_focused: bool,
    state: &AppState,
    policies: &crate::board::Policies,
    now: chrono::DateTime<Utc>,
) {
    // Filter cards if filter is active
    let cards: Vec<(usize, &Card)> = col
        .cards
        .iter()
        .enumerate()
        .filter(|(_, card)| {
            if let Some(ref filter) = state.active_filter {
                let filter_lower = filter.to_lowercase();
                card.title.to_lowercase().contains(&filter_lower)
                    || card.tags.iter().any(|t| t.contains(&filter_lower))
            } else if !state.active_tag_filters.is_empty() {
                card.tags
                    .iter()
                    .any(|t| state.active_tag_filters.contains(t))
            } else {
                true
            }
        })
        .collect();

    // Build header
    let card_count = if state.active_filter.is_some() || !state.active_tag_filters.is_empty() {
        format!("{}/{}", cards.len(), col.cards.len())
    } else {
        format!("{}", col.cards.len())
    };

    let wip_text = if let Some(limit) = col.wip_limit {
        let wip_color = if col.cards.len() as u32 > limit {
            Theme::WIP_OVER
        } else if col.cards.len() as u32 == limit {
            Theme::WIP_NEAR
        } else {
            Theme::WIP_OK
        };
        Span::styled(
            format!(" [{}/{}]", col.cards.len(), limit),
            Style::default().fg(wip_color),
        )
    } else {
        Span::raw("")
    };

    let focused_mod = if is_focused { Modifier::BOLD } else { Modifier::empty() };

    let header_line = Line::from(vec![
        Span::styled(
            format!(" {} ", col.name),
            Style::default()
                .fg(Theme::COLUMN_HEADER)
                .add_modifier(Modifier::BOLD),
        ),
        Span::styled(format!("({card_count})"), Theme::dim_style()),
        wip_text,
    ]);

    let border_color = if is_focused {
        Theme::COLUMN_FOCUSED_BORDER
    } else {
        Theme::COLUMN_BORDER
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(border_color).add_modifier(focused_mod))
        .border_type(ratatui::widgets::BorderType::Rounded)
        .title(header_line)
        .padding(Padding::new(1, 1, 0, 0));

    let inner = block.inner(area);
    f.render_widget(block, area);

    if inner.height == 0 || inner.width == 0 {
        return;
    }

    // Render cards
    let card_height: u16 = 5; // 3 inner lines + 2 border lines
    let max_visible = (inner.height / card_height) as usize;

    // Calculate scroll offset
    let selected_in_col = if is_focused {
        state.selected_card
    } else {
        0
    };
    let scroll_offset = if cards.len() > max_visible {
        if selected_in_col >= max_visible {
            selected_in_col - max_visible + 1
        } else {
            0
        }
    } else {
        0
    };

    for (vis_idx, &(real_idx, card)) in cards.iter().enumerate().skip(scroll_offset) {
        if vis_idx - scroll_offset >= max_visible {
            break;
        }

        let y = inner.y + ((vis_idx - scroll_offset) as u16 * card_height);
        let card_area = Rect::new(inner.x, y, inner.width, card_height);

        let is_selected = is_focused && state.selected_card == real_idx;
        render_card(f, card_area, card, is_selected, policies, now);
    }

    // Scroll indicator
    if cards.len() > max_visible {
        let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight);
        let mut scrollbar_state = ScrollbarState::new(cards.len()).position(scroll_offset);
        f.render_stateful_widget(scrollbar, area, &mut scrollbar_state);
    }
}

fn render_card(
    f: &mut Frame,
    area: Rect,
    card: &Card,
    is_selected: bool,
    policies: &crate::board::Policies,
    now: chrono::DateTime<Utc>,
) {
    if area.width < 4 || area.height < 3 {
        return;
    }

    let stale = staleness(card, policies, now);
    let age_str = format_age(card.created, now);

    let border_color = if card.blocked {
        Theme::BLOCKER
    } else if is_selected {
        Theme::CARD_SELECTED_BORDER
    } else {
        match stale {
            Staleness::VeryStale => Theme::BUBBLE_UP_CRITICAL,
            Staleness::Stale => Theme::BUBBLE_UP_WARN,
            Staleness::Fresh => Theme::CARD_BORDER,
        }
    };

    let selected_mod = if is_selected { Modifier::BOLD } else { Modifier::empty() };

    let border_style = Style::default()
        .fg(border_color)
        .add_modifier(selected_mod);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .border_type(if is_selected {
            ratatui::widgets::BorderType::Thick
        } else {
            ratatui::widgets::BorderType::Plain
        });

    let inner = block.inner(area);
    f.render_widget(block, area);

    if inner.height == 0 || inner.width < 2 {
        return;
    }

    // Build the right-side glyphs for line 1
    let mut right_glyphs: Vec<Span> = Vec::new();

    // Staleness glyph
    match stale {
        Staleness::Stale => {
            right_glyphs.push(Span::styled(
                "\u{f017}", // nf-fa-clock
                Style::default().fg(Theme::BUBBLE_UP_WARN),
            ));
        }
        Staleness::VeryStale => {
            right_glyphs.push(Span::styled(
                "\u{f017}\u{f017}", // double clock
                Style::default().fg(Theme::BUBBLE_UP_CRITICAL),
            ));
        }
        Staleness::Fresh => {}
    }

    // Priority glyph
    if let Some(sym) = card.priority.symbol() {
        let color = Theme::priority_color(card.priority);
        if !right_glyphs.is_empty() {
            right_glyphs.push(Span::raw(" "));
        }
        right_glyphs.push(Span::styled(sym, Style::default().fg(color)));
    }

    // Blocked glyph
    if card.blocked {
        if !right_glyphs.is_empty() {
            right_glyphs.push(Span::raw(" "));
        }
        right_glyphs.push(Span::styled(
            "\u{f05e}", // nf-fa-ban
            Style::default().fg(Theme::BLOCKER),
        ));
    }

    // Calculate glyph width for right-alignment
    let glyphs_width: usize = right_glyphs.iter().map(|s| s.width()).sum();

    // Line 1: [marker] ID  age ... [glyphs on right]
    let marker = "\u{f054} "; // nf-fa-chevron_right — always present for stable layout
    let marker_style = if is_selected {
        Style::default().fg(Theme::FG).add_modifier(selected_mod)
    } else {
        // Invisible: render as spaces matching the marker's display width
        Style::default()
    };
    let marker_display = if is_selected { marker } else { "  " };
    let left_width = marker.width() + card.id.width() + 1 + age_str.width();
    // Reserve 1 char gap before glyphs so they don't touch the border
    let padding_needed =
        (inner.width as usize).saturating_sub(left_width + glyphs_width + 1);

    let mut line1_spans = vec![
        Span::styled(marker_display, marker_style),
        Span::styled(&card.id, Style::default().fg(Theme::DIM).add_modifier(selected_mod)),
        Span::raw(" "),
        Span::styled(age_str, Style::default().fg(Theme::DIM).add_modifier(selected_mod)),
        Span::raw(" ".repeat(padding_needed)),
    ];
    line1_spans.extend(right_glyphs);

    // Line 2: title (unicode-safe truncation using grapheme clusters)
    let max_title_width = inner.width as usize;
    let title_display_width = card.title.width() + 2; // 2 for leading spaces
    let title = if title_display_width > max_title_width {
        let avail = max_title_width.saturating_sub(3); // 2 spaces + 1 for '…'
        let truncated: String = card
            .title
            .graphemes(true)
            .scan(0, |w, g| {
                let gw = g.width();
                (*w + gw <= avail).then(|| {
                    *w += gw;
                    g
                })
            })
            .collect();
        format!("  {truncated}…")
    } else {
        format!("  {}", card.title)
    };
    let title_line = Line::from(Span::styled(
        title,
        Style::default().fg(Theme::CARD_TITLE).add_modifier(selected_mod),
    ));

    // Render lines
    if inner.height >= 1 {
        f.render_widget(
            Paragraph::new(Line::from(line1_spans)),
            Rect::new(inner.x, inner.y, inner.width, 1),
        );
    }
    if inner.height >= 2 {
        f.render_widget(
            Paragraph::new(title_line),
            Rect::new(inner.x, inner.y + 1, inner.width, 1),
        );
    }
    // Line 3: tags (if any)
    if inner.height >= 3 && !card.tags.is_empty() {
        let mut spans = vec![Span::raw("  ")];
        for (i, tag) in card.tags.iter().enumerate() {
            if i > 0 {
                spans.push(Span::styled(" · ", Theme::dim_style()));
            }
            spans.push(Span::styled(
                tag.as_str(),
                Style::default().fg(Theme::tag_color(tag)).add_modifier(selected_mod),
            ));
        }
        f.render_widget(
            Paragraph::new(Line::from(spans)),
            Rect::new(inner.x, inner.y + 2, inner.width, 1),
        );
    }
}
