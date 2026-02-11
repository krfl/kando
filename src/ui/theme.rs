use ratatui::style::{Color, Style};

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

    // Status bar
    pub const STATUS_HINT: Color = Color::DarkGray;

    // Hint popup
    pub const HINT_KEY: Color = Color::Reset;
    pub const HINT_DESC: Color = Color::Reset;

    pub fn dim_style() -> Style {
        Style::default().fg(Self::DIM)
    }

    pub fn status_style() -> Style {
        Style::default().fg(Self::FG)
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
