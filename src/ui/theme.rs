use ratatui::style::{Color, Style};

use crate::board::Priority;

/// Icon set for Kando glyphs.
///
/// Two variants: ASCII (default) and Nerd Font for terminals with a patched
/// font. Select with `icons(nerd_font)`.
pub struct Icons {
    pub priority_low: &'static str,
    pub priority_high: &'static str,
    pub priority_urgent: &'static str,
    pub stale: &'static str,
    pub very_stale: &'static str,
    pub blocker: &'static str,
    pub chevron: &'static str,
    pub sync_online: &'static str,
    pub sync_offline: &'static str,
}

pub const NERD_ICONS: Icons = Icons {
    priority_low: "\u{f063}",       // nf-fa-arrow_down
    priority_high: "\u{f0e7}",      // nf-fa-bolt
    priority_urgent: "\u{f06d}",    // nf-fa-fire
    stale: "\u{f017}",              // nf-fa-clock
    very_stale: "\u{f017}\u{f017}", // double clock
    blocker: "\u{f05e}",            // nf-fa-ban
    chevron: "\u{f054}",            // nf-fa-chevron_right
    sync_online: "\u{f021}",        // nf-fa-refresh
    sync_offline: "\u{f071}",       // nf-fa-warning
};

pub const ASCII_ICONS: Icons = Icons {
    priority_low: "v",
    priority_high: "!",
    priority_urgent: "!!",
    stale: "~",
    very_stale: "~~",
    blocker: "X",
    chevron: ">",
    sync_online: "*",
    sync_offline: "!",
};

/// Return the icon set for the given mode.
pub fn icons(nerd_font: bool) -> &'static Icons {
    if nerd_font { &NERD_ICONS } else { &ASCII_ICONS }
}

impl Icons {
    /// Priority icon for a given level, or `None` for Normal.
    pub fn priority(&self, priority: Priority) -> Option<&'static str> {
        match priority {
            Priority::Low => Some(self.priority_low),
            Priority::Normal => None,
            Priority::High => Some(self.priority_high),
            Priority::Urgent => Some(self.priority_urgent),
        }
    }
}

/// Color theme for Kando.
///
/// All text and UI chrome uses the terminal's default foreground color (Color::Reset).
/// Only functional glyphs (priority, staleness, blocker) and tags get color.
pub struct Theme;

impl Theme {
    // Base — everything defaults to the terminal's own foreground
    pub const FG: Color = Color::Reset;
    pub const DIM: Color = Color::DarkGray;

    // Column
    pub const COLUMN_HEADER: Color = Color::Reset;
    pub const COLUMN_BORDER: Color = Color::Reset;
    pub const COLUMN_FOCUSED_BORDER: Color = Color::Reset;

    // Card
    pub const CARD_BORDER: Color = Color::Reset;
    pub const CARD_SELECTED_BORDER: Color = Color::Reset;
    pub const CARD_TITLE: Color = Color::Reset;

    // Functional glyph colors (these are the only colored elements besides tags)
    pub const PRIORITY_LOW: Color = Color::Green;
    pub const PRIORITY_HIGH: Color = Color::Yellow;
    pub const PRIORITY_URGENT: Color = Color::Red;
    pub const BUBBLE_UP_WARN: Color = Color::Yellow;
    pub const BUBBLE_UP_CRITICAL: Color = Color::Red;
    pub const BLOCKER: Color = Color::Red;

    // WIP limit glyphs
    pub const WIP_OK: Color = Color::Green;
    pub const WIP_NEAR: Color = Color::Yellow;
    pub const WIP_OVER: Color = Color::Red;

    // Assignee
    pub const ASSIGNEE: Color = Color::Cyan;

    // Status bar
    pub const STATUS_ERROR: Color = Color::Red;

    // Hint popup
    pub const HINT_KEY: Color = Color::Reset;
    pub const HINT_DESC: Color = Color::Reset;

    pub fn dim_style() -> Style {
        Style::default().fg(Self::DIM)
    }

    pub fn status_style() -> Style {
        Style::default().fg(Self::FG)
    }

    /// Color for a priority level.
    pub fn priority_color(priority: Priority) -> Color {
        match priority {
            Priority::Low => Self::PRIORITY_LOW,
            Priority::Normal => Self::FG,
            Priority::High => Self::PRIORITY_HIGH,
            Priority::Urgent => Self::PRIORITY_URGENT,
        }
    }

    /// Assign a consistent color to a tag based on its name.
    pub fn tag_color(tag: &str) -> Color {
        let hash = tag
            .bytes()
            .fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));
        const PALETTE: [Color; 12] = [
            Color::Cyan,
            Color::Green,
            Color::Magenta,
            Color::Blue,
            Color::Yellow,
            Color::Red,
            Color::LightCyan,
            Color::LightGreen,
            Color::LightMagenta,
            Color::LightBlue,
            Color::LightYellow,
            Color::LightRed,
        ];
        PALETTE[(hash % PALETTE.len() as u32) as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn icons_default_is_ascii() {
        let i = icons(false);
        assert_eq!(i.chevron, ">");
        assert_eq!(i.blocker, "X");
        assert_eq!(i.priority_urgent, "!!");
        assert_eq!(i.very_stale, "~~");
    }

    #[test]
    fn icons_nerd_font_when_enabled() {
        let i = icons(true);
        assert_eq!(i.chevron, "\u{f054}");
        assert_eq!(i.blocker, "\u{f05e}");
    }

    #[test]
    fn priority_returns_none_for_normal() {
        let ascii = icons(false);
        assert!(ascii.priority(Priority::Normal).is_none());
        let nerd = icons(true);
        assert!(nerd.priority(Priority::Normal).is_none());
    }

    #[test]
    fn priority_returns_some_for_non_normal() {
        let i = icons(false);
        assert_eq!(i.priority(Priority::Low), Some("v"));
        assert_eq!(i.priority(Priority::High), Some("!"));
        assert_eq!(i.priority(Priority::Urgent), Some("!!"));
    }

    #[test]
    fn priority_color_all_variants() {
        assert_eq!(Theme::priority_color(Priority::Low), Color::Green);
        assert_eq!(Theme::priority_color(Priority::Normal), Color::Reset);
        assert_eq!(Theme::priority_color(Priority::High), Color::Yellow);
        assert_eq!(Theme::priority_color(Priority::Urgent), Color::Red);
    }

    #[test]
    fn tag_color_deterministic() {
        let c1 = Theme::tag_color("bug");
        let c2 = Theme::tag_color("bug");
        assert_eq!(c1, c2);
    }

    #[test]
    fn tag_color_different_tags_may_differ() {
        // Not guaranteed but very likely for distinct strings
        let c1 = Theme::tag_color("bug");
        let c2 = Theme::tag_color("feature");
        // At minimum both should be valid colors (not panic)
        let _ = (c1, c2);
    }

    #[test]
    fn tag_color_empty_string_no_panic() {
        let _ = Theme::tag_color("");
    }

    #[test]
    fn dim_style_uses_dim_color() {
        let s = Theme::dim_style();
        assert_eq!(s.fg, Some(Theme::DIM));
    }

    #[test]
    fn status_style_uses_fg_color() {
        let s = Theme::status_style();
        assert_eq!(s.fg, Some(Theme::FG));
    }

    #[test]
    fn all_icon_fields_are_non_empty() {
        for nerd_font in [false, true] {
            let i = icons(nerd_font);
            assert!(!i.priority_low.is_empty());
            assert!(!i.priority_high.is_empty());
            assert!(!i.priority_urgent.is_empty());
            assert!(!i.stale.is_empty());
            assert!(!i.very_stale.is_empty());
            assert!(!i.blocker.is_empty());
            assert!(!i.chevron.is_empty());
            assert!(!i.sync_online.is_empty());
            assert!(!i.sync_offline.is_empty());
        }
    }
}
