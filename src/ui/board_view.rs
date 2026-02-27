use fuzzy_matcher::skim::SkimMatcherV2;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use ratatui::symbols::border;
use ratatui::widgets::{
    Block, BorderType, Borders, Padding, Paragraph, Scrollbar, ScrollbarOrientation,
    ScrollbarState,
};
use ratatui::Frame;

use super::theme::{self, Icons, Theme};
use crate::app::AppState;
use crate::board::age::{format_age, staleness, Staleness};
use crate::board::{Board, Card};
use chrono::{DateTime, Utc};

/// Total display width of an icon list, including one-space separators between items.
pub(crate) fn total_icon_width(icons: &[(&str, Style)]) -> usize {
    icons.iter().map(|(t, _)| t.width()).sum::<usize>()
        + icons.len().saturating_sub(1)
}

/// Return the subset of `candidates` that fits within `avail_width`.
///
/// Icons are dropped from the left (least important first) until the remaining
/// set fits. If even the single last icon exceeds `avail_width`, returns empty.
/// Input order is preserved in the output.
pub(crate) fn fit_icons<'a>(candidates: &[(&'a str, Style)], avail_width: usize) -> Vec<(&'a str, Style)> {
    // Advance a start index from the left (dropping least-important icons first)
    // until the remaining slice fits. This avoids Vec::remove(0) element-shifting.
    let mut start = 0;
    while start + 1 < candidates.len() && total_icon_width(&candidates[start..]) > avail_width {
        start += 1;
    }
    let remaining = &candidates[start..];
    if total_icon_width(remaining) > avail_width {
        return Vec::new();
    }
    remaining.to_vec()
}

/// Border color for a card based on blocked state, column focus, and staleness.
///
/// Semantic border colors (blocker, stale, very-stale) are always visible
/// regardless of column focus — these are information-carrying signals.
/// Only fresh cards in unfocused columns are dimmed.
///
/// Selection is expressed via `BorderType::Thick + Modifier::BOLD`, not via
/// color — so `is_selected` is intentionally absent from this function.
pub(crate) fn card_border_color(blocked: bool, is_col_focused: bool, stale: Staleness) -> Color {
    if blocked {
        Theme::BLOCKER
    } else {
        match stale {
            Staleness::VeryStale => Theme::VERY_STALE,
            Staleness::Stale => Theme::STALE,
            Staleness::Fresh if is_col_focused => Theme::CARD_BORDER,
            Staleness::Fresh => Theme::DIM,
        }
    }
}

/// Title color: green for cards created on the same UTC day as `now`.
pub(crate) fn title_color(created: DateTime<Utc>, now: DateTime<Utc>) -> Color {
    if created.date_naive() == now.date_naive() {
        Theme::NEW_CARD_TITLE
    } else {
        Theme::CARD_TITLE
    }
}

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

    let matcher = SkimMatcherV2::default();
    let icons = theme::icons(state.nerd_font);
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
            &matcher,
            icons,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn render_column(
    f: &mut Frame,
    area: Rect,
    col: &crate::board::Column,
    is_focused: bool,
    state: &AppState,
    policies: &crate::board::Policies,
    now: chrono::DateTime<Utc>,
    matcher: &SkimMatcherV2,
    icons: &Icons,
) {
    // Text search (/) takes precedence; tag/assignee picker filters apply otherwise
    let cards: Vec<(usize, &Card)> = col
        .cards
        .iter()
        .enumerate()
        .filter(|(_, card)| {
            crate::board::card_is_visible(
                card,
                state.active_filter.as_deref(),
                &state.active_tag_filters,
                &state.active_assignee_filters,
                matcher,
            )
        })
        .collect();

    // Build header
    let card_count = if state.active_filter.is_some() || !state.active_tag_filters.is_empty() || !state.active_assignee_filters.is_empty() {
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

    let mut block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(border_color).add_modifier(focused_mod))
        .border_type(BorderType::Rounded)
        .title(header_line)
        .padding(Padding::new(1, 1, 0, 0));

    if col.hidden {
        block = block.border_set(border::Set {
            top_left: "╭",
            top_right: "╮",
            bottom_left: "╰",
            bottom_right: "╯",
            vertical_left: "╎",
            vertical_right: "╎",
            horizontal_top: "╌",
            horizontal_bottom: "╌",
        });
    }

    let inner = block.inner(area);
    f.render_widget(block, area);

    if inner.height == 0 || inner.width == 0 {
        return;
    }

    // Render cards
    let card_height: u16 = 5; // 3 inner lines + 2 border lines
    let max_visible = (inner.height / card_height) as usize;

    // Calculate scroll offset (position within the filtered list, not unfiltered index)
    let selected_in_col = if is_focused {
        cards
            .iter()
            .position(|&(real_idx, _)| real_idx == state.selected_card)
            .unwrap_or(0)
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
        render_card(f, card_area, card, is_selected, is_focused, policies, now, icons);
    }

    // Scroll indicator
    if cards.len() > max_visible {
        let scrollbar = Scrollbar::new(ScrollbarOrientation::VerticalRight);
        let mut scrollbar_state = ScrollbarState::new(cards.len()).position(scroll_offset);
        f.render_stateful_widget(scrollbar, area, &mut scrollbar_state);
    }
}

#[allow(clippy::too_many_arguments)]
fn render_card(
    f: &mut Frame,
    area: Rect,
    card: &Card,
    is_selected: bool,
    is_col_focused: bool,
    policies: &crate::board::Policies,
    now: chrono::DateTime<Utc>,
    icons: &Icons,
) {
    debug_assert!(!is_selected || is_col_focused, "a card cannot be selected in an unfocused column");
    if area.width < 4 || area.height < 3 {
        return;
    }

    let stale = staleness(card, policies, now);
    let age_str = format_age(card.created, now);

    let border_color = card_border_color(card.blocked, is_col_focused, stale);

    let selected_mod = if is_selected { Modifier::BOLD } else { Modifier::empty() };

    let border_style = Style::default()
        .fg(border_color)
        .add_modifier(selected_mod);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .border_type(if is_selected { BorderType::Thick } else { BorderType::Rounded });

    let inner = block.inner(area);
    f.render_widget(block, area);

    if inner.height == 0 || inner.width < 2 {
        return;
    }

    let title_color = title_color(card.created, now);

    // Compute left-side width up front so glyph truncation can use it.
    let marker = format!("{} ", icons.chevron);
    let marker_width = marker.width();
    let left_width = marker_width + card.id.width() + 1 + age_str.width();

    // Build icon candidates in display order: staleness (left, least important)
    // → priority (middle) → blocker (right, most important).
    // When space is tight, icons are dropped from the left first so that the
    // most important signal (blocker) survives as long as possible.
    let mut candidates: Vec<(&str, Style)> = Vec::new();

    match stale {
        Staleness::Stale => candidates.push((icons.stale, Style::default().fg(Theme::STALE))),
        Staleness::VeryStale => candidates.push((icons.very_stale, Style::default().fg(Theme::VERY_STALE))),
        Staleness::Fresh => {}
    }
    if let Some(sym) = icons.priority(card.priority) {
        candidates.push((sym, Style::default().fg(Theme::priority_color(card.priority))));
    }
    if card.blocked {
        candidates.push((icons.blocker, Style::default().fg(Theme::BLOCKER)));
    }

    // Available width for icons. We reserve 1 char between the left content
    // (marker + id + age) and the icon cluster; padding_needed below fills it.
    let avail_glyph_width = (inner.width as usize).saturating_sub(left_width + 1);
    let candidates = fit_icons(&candidates, avail_glyph_width);

    // Build the final Span list with separators.
    let mut right_glyphs: Vec<Span> = Vec::new();
    for (i, (text, style)) in candidates.into_iter().enumerate() {
        if i > 0 {
            right_glyphs.push(Span::raw(" "));
        }
        right_glyphs.push(Span::styled(text, style));
    }

    // Calculate glyph width for right-alignment
    let glyphs_width: usize = right_glyphs.iter().map(|s| s.width()).sum();

    // Line 1: [marker] ID  age ... [glyphs on right]
    // marker/marker_width/left_width already computed above for glyph truncation.
    let marker_style = if is_selected {
        Style::default().fg(Theme::FG).add_modifier(selected_mod)
    } else {
        // Invisible: render as spaces matching the marker's display width
        Style::default()
    };
    let placeholder = " ".repeat(marker_width);
    let marker_display: &str = if is_selected { &marker } else { &placeholder };
    // Padding fills the gap between left content and right icons (minimum 0).
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
        Style::default()
            .fg(title_color)
            .add_modifier(selected_mod),
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
    // Line 3: assignees + tags
    let has_metadata = !card.assignees.is_empty() || !card.tags.is_empty();
    if inner.height >= 3 && has_metadata {
        let mut spans = vec![Span::raw("  ")];
        let mut need_sep = false;
        for assignee in &card.assignees {
            if need_sep {
                spans.push(Span::raw(" "));
            }
            spans.push(Span::styled(
                format!("@{assignee}"),
                Style::default()
                    .fg(Theme::ASSIGNEE)
                    .add_modifier(selected_mod),
            ));
            need_sep = true;
        }
        for tag in &card.tags {
            if need_sep {
                spans.push(Span::styled(" · ", Theme::dim_style()));
            }
            spans.push(Span::styled(
                tag.as_str(),
                Style::default()
                    .fg(Theme::tag_color(tag))
                    .add_modifier(selected_mod),
            ));
            need_sep = true;
        }
        f.render_widget(
            Paragraph::new(Line::from(spans)),
            Rect::new(inner.x, inner.y + 2, inner.width, 1),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s() -> Style { Style::default() }

    // ── total_icon_width ──────────────────────────────────────────────────────

    #[test]
    fn total_icon_width_empty_is_zero() {
        assert_eq!(total_icon_width(&[]), 0);
    }

    #[test]
    fn total_icon_width_one_icon_no_separator() {
        assert_eq!(total_icon_width(&[("X", s())]), 1);
    }

    #[test]
    fn total_icon_width_two_icons_one_separator() {
        // "~" + " " + "X" = 3
        assert_eq!(total_icon_width(&[("~", s()), ("X", s())]), 3);
    }

    #[test]
    fn total_icon_width_three_icons_two_separators() {
        // "~" + " " + "!" + " " + "X" = 5
        assert_eq!(total_icon_width(&[("~", s()), ("!", s()), ("X", s())]), 5);
    }

    #[test]
    fn total_icon_width_two_char_icon_counts_correctly() {
        // "!!" (urgent, w=2) + " " + "X" = 4
        assert_eq!(total_icon_width(&[("!!", s()), ("X", s())]), 4);
    }

    // ── fit_icons ─────────────────────────────────────────────────────────────

    #[test]
    fn fit_icons_empty_input_returns_empty() {
        assert!(fit_icons(&[], 100).is_empty());
    }

    #[test]
    fn fit_icons_all_fit_returns_all() {
        // "~" + " " + "X" = 3; avail = 10
        let r = fit_icons(&[("~", s()), ("X", s())], 10);
        assert_eq!(r.len(), 2);
        assert_eq!(r[0].0, "~");
        assert_eq!(r[1].0, "X");
    }

    #[test]
    fn fit_icons_exact_boundary_no_drop() {
        // "~" + " " + "X" = 3; avail = 3 → fits exactly
        let r = fit_icons(&[("~", s()), ("X", s())], 3);
        assert_eq!(r.len(), 2);
    }

    #[test]
    fn fit_icons_one_over_boundary_drops_leftmost() {
        // "~" + " " + "X" = 3; avail = 2 → drop "~", "X" = 1 fits
        let r = fit_icons(&[("~", s()), ("X", s())], 2);
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].0, "X");
    }

    #[test]
    fn fit_icons_drops_stale_when_only_blocker_fits() {
        // avail = 1: only "X" (width 1) fits
        let r = fit_icons(&[("~", s()), ("X", s())], 1);
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].0, "X");
    }

    #[test]
    fn fit_icons_drops_stale_and_priority_blocker_survives() {
        // 3 icons, avail = 1: drop stale + priority, keep blocker
        let r = fit_icons(&[("~", s()), ("!", s()), ("X", s())], 1);
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].0, "X");
    }

    #[test]
    fn fit_icons_clears_all_when_nothing_fits() {
        // "X" = 1; avail = 0 → clear
        assert!(fit_icons(&[("X", s())], 0).is_empty());
    }

    #[test]
    fn fit_icons_urgent_exact_fit() {
        // "!!" = 2; avail = 2 → fits
        let r = fit_icons(&[("!!", s())], 2);
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].0, "!!");
    }

    #[test]
    fn fit_icons_urgent_dropped_blocker_survives() {
        // "!!" + " " + "X" = 4; avail = 1 → drop "!!", "X" = 1 fits
        let r = fit_icons(&[("!!", s()), ("X", s())], 1);
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].0, "X");
    }

    #[test]
    fn fit_icons_preserves_input_order() {
        let r = fit_icons(&[("~", s()), ("!", s()), ("X", s())], 100);
        assert_eq!(r[0].0, "~");
        assert_eq!(r[1].0, "!");
        assert_eq!(r[2].0, "X");
    }

    #[test]
    fn fit_icons_after_left_drop_remaining_order_preserved() {
        // "~" + " " + "!" + " " + "X" = 5; avail = 3 → drop "~", "!" + " " + "X" = 3
        let r = fit_icons(&[("~", s()), ("!", s()), ("X", s())], 3);
        assert_eq!(r.len(), 2);
        assert_eq!(r[0].0, "!");
        assert_eq!(r[1].0, "X");
    }

    // ── card_border_color ─────────────────────────────────────────────────────

    #[test]
    fn border_color_blocked_wins_over_all() {
        assert_eq!(card_border_color(true, true,  Staleness::Fresh),     Theme::BLOCKER);
        assert_eq!(card_border_color(true, false, Staleness::Fresh),     Theme::BLOCKER);
        assert_eq!(card_border_color(true, false, Staleness::VeryStale), Theme::BLOCKER);
        assert_eq!(card_border_color(true, true,  Staleness::Stale),     Theme::BLOCKER);
    }

    #[test]
    fn border_color_unfocused_fresh_returns_dim() {
        assert_eq!(card_border_color(false, false, Staleness::Fresh), Theme::DIM);
    }

    #[test]
    fn border_color_unfocused_stale_returns_stale_color() {
        assert_eq!(card_border_color(false, false, Staleness::Stale), Theme::STALE);
    }

    #[test]
    fn border_color_unfocused_very_stale_returns_very_stale_color() {
        assert_eq!(card_border_color(false, false, Staleness::VeryStale), Theme::VERY_STALE);
    }

    #[test]
    fn border_color_focused_fresh_is_card_border() {
        assert_eq!(card_border_color(false, true, Staleness::Fresh), Theme::CARD_BORDER);
    }

    #[test]
    fn border_color_focused_stale_is_stale_color() {
        assert_eq!(card_border_color(false, true, Staleness::Stale), Theme::STALE);
    }

    #[test]
    fn border_color_focused_very_stale_is_very_stale_color() {
        assert_eq!(card_border_color(false, true, Staleness::VeryStale), Theme::VERY_STALE);
    }

    #[test]
    fn border_color_stale_always_shows_semantic_color() {
        // Stale border color must be visible regardless of column focus.
        assert_eq!(card_border_color(false, true, Staleness::Stale), Theme::STALE);
        assert_eq!(card_border_color(false, false, Staleness::Stale), Theme::STALE);
    }

    #[test]
    fn border_color_very_stale_always_shows_semantic_color() {
        assert_eq!(card_border_color(false, true, Staleness::VeryStale), Theme::VERY_STALE);
        assert_eq!(card_border_color(false, false, Staleness::VeryStale), Theme::VERY_STALE);
    }

    // ── title_color ─────────────────────────────────────────────────────────

    #[test]
    fn title_color_created_today_is_green() {
        use chrono::NaiveDate;
        let day = NaiveDate::from_ymd_opt(2025, 6, 15).unwrap();
        let now = day.and_hms_opt(12, 0, 0).unwrap().and_utc();
        assert_eq!(title_color(now, now), Theme::NEW_CARD_TITLE);
    }

    #[test]
    fn title_color_created_yesterday_is_default() {
        use chrono::NaiveDate;
        let today = NaiveDate::from_ymd_opt(2025, 6, 15).unwrap();
        let yesterday = today.pred_opt().unwrap();
        let created = yesterday.and_hms_opt(18, 0, 0).unwrap().and_utc();
        let now = today.and_hms_opt(9, 0, 0).unwrap().and_utc();
        assert_eq!(title_color(created, now), Theme::CARD_TITLE);
    }

    #[test]
    fn title_color_same_day_start_and_end() {
        use chrono::NaiveDate;
        let day = NaiveDate::from_ymd_opt(2025, 6, 15).unwrap();
        let start = day.and_hms_opt(0, 0, 0).unwrap().and_utc();
        let end = day.and_hms_opt(23, 59, 59).unwrap().and_utc();
        assert_eq!(title_color(start, end), Theme::NEW_CARD_TITLE);
    }

    #[test]
    fn title_color_one_second_across_midnight_is_not_green() {
        use chrono::NaiveDate;
        let day = NaiveDate::from_ymd_opt(2025, 6, 15).unwrap();
        let before_midnight = day.and_hms_opt(23, 59, 59).unwrap().and_utc();
        let after_midnight = day.succ_opt().unwrap().and_hms_opt(0, 0, 0).unwrap().and_utc();
        assert_eq!(title_color(before_midnight, after_midnight), Theme::CARD_TITLE);
    }
}
