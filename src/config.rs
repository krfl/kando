use serde::{Deserialize, Serialize};

use crate::board::storage::{BoardSection, ColumnConfig};

#[derive(Debug, Serialize, Deserialize)]
pub struct BoardConfig {
    pub board: BoardSection,
    #[serde(rename = "columns")]
    pub columns: Vec<ColumnConfig>,
}

/// Marker file `.kando.toml` for git-synced boards.
/// Committed to the repo; the only footprint in the working tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KandoToml {
    #[serde(default = "default_kando_branch")]
    pub branch: String,
}

fn default_kando_branch() -> String {
    "kando".to_string()
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

    // ---- KandoToml tests ----

    #[test]
    fn kando_toml_roundtrip() {
        let original = KandoToml { branch: "kando".to_string() };
        let serialized = toml::to_string_pretty(&original).unwrap();
        let loaded: KandoToml = toml::from_str(&serialized).unwrap();
        assert_eq!(loaded.branch, "kando");
    }

    #[test]
    fn kando_toml_default_branch_when_missing() {
        let loaded: KandoToml = toml::from_str("").unwrap();
        assert_eq!(loaded.branch, "kando");
    }

    #[test]
    fn kando_toml_custom_branch() {
        let loaded: KandoToml = toml::from_str("branch = \"my-board\"\n").unwrap();
        assert_eq!(loaded.branch, "my-board");
    }

    #[test]
    fn kando_toml_malformed_returns_err() {
        let result = toml::from_str::<KandoToml>("branch = !!!");
        assert!(result.is_err());
    }

    #[test]
    fn kando_toml_ignores_unknown_fields() {
        let loaded: KandoToml = toml::from_str("branch = \"kando\"\nextra = 42\n").unwrap();
        assert_eq!(loaded.branch, "kando");
    }

    #[test]
    fn kando_toml_serialization_contains_branch() {
        let toml = KandoToml { branch: "kando".to_string() };
        let s = toml::to_string_pretty(&toml).unwrap();
        assert!(s.contains("branch = \"kando\""));
    }

    #[test]
    fn kando_toml_branch_with_slashes() {
        let loaded: KandoToml = toml::from_str("branch = \"team/board\"\n").unwrap();
        assert_eq!(loaded.branch, "team/board");
    }
}
