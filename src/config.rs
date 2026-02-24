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
    #[serde(default)]
    pub focus_mode: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_config_default_is_false() {
        assert!(!LocalConfig::default().focus_mode);
    }

    #[test]
    fn local_config_toml_roundtrip_focus_true() {
        let original = LocalConfig { focus_mode: true };
        let serialized = toml::to_string_pretty(&original).unwrap();
        let loaded: LocalConfig = toml::from_str(&serialized).unwrap();
        assert_eq!(loaded.focus_mode, original.focus_mode);
    }

    #[test]
    fn local_config_toml_roundtrip_focus_false() {
        let original = LocalConfig { focus_mode: false };
        let serialized = toml::to_string_pretty(&original).unwrap();
        // Confirm false is written explicitly, not omitted
        assert!(serialized.contains("focus_mode"));
        let loaded: LocalConfig = toml::from_str(&serialized).unwrap();
        assert_eq!(loaded.focus_mode, original.focus_mode);
    }

    #[test]
    fn local_config_missing_field_deserializes_as_false() {
        let loaded: LocalConfig = toml::from_str("").unwrap();
        assert!(!loaded.focus_mode);
    }
}
