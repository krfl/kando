use serde::{Deserialize, Serialize};

use crate::board::storage::{BoardSection, ColumnConfig};

#[derive(Debug, Serialize, Deserialize)]
pub struct BoardConfig {
    pub board: BoardSection,
    #[serde(rename = "columns")]
    pub columns: Vec<ColumnConfig>,
}

/// Per-user local preferences stored in `.kando/local.toml` (gitignored).
/// Defaults to all-off so missing fields are handled gracefully.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct LocalConfig {
    /// Whether the first-launch tutorial has been shown.
    #[serde(default)]
    pub tutorial_shown: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_config_default_creates_empty_struct() {
        let _cfg = LocalConfig::default();
    }

    #[test]
    fn local_config_toml_roundtrip() {
        let original = LocalConfig::default();
        let serialized = toml::to_string_pretty(&original).unwrap();
        let _loaded: LocalConfig = toml::from_str(&serialized).unwrap();
    }

    #[test]
    fn local_config_empty_string_deserializes() {
        let _loaded: LocalConfig = toml::from_str("").unwrap();
    }

    #[test]
    fn local_config_ignores_unknown_fields() {
        // Old local.toml files may still contain `focus_mode` from before it was removed.
        let _loaded: LocalConfig = toml::from_str("focus_mode = true\n").unwrap();
    }

    #[test]
    fn local_config_default_tutorial_shown_is_false() {
        assert!(!LocalConfig::default().tutorial_shown);
    }

    #[test]
    fn local_config_tutorial_shown_true_roundtrip() {
        let original = LocalConfig { tutorial_shown: true };
        let serialized = toml::to_string_pretty(&original).unwrap();
        let loaded: LocalConfig = toml::from_str(&serialized).unwrap();
        assert!(loaded.tutorial_shown);
    }

    #[test]
    fn local_config_missing_tutorial_shown_defaults_to_false() {
        let loaded: LocalConfig = toml::from_str("").unwrap();
        assert!(!loaded.tutorial_shown);
    }
}
