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
    // Currently empty — kept as the extension point for future local prefs.
    // serde(deny_unknown_fields) is intentionally absent so that old
    // local.toml files with `focus_mode` (removed) don't cause errors.
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
        let original = LocalConfig {};
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
}
