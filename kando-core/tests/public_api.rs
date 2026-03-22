/// Verify that re-exported board types are accessible at the crate root.
#[test]
fn re_export_board_types_accessible() {
    let _priority = kando_core::Priority::default();
    let _template = kando_core::Template {
        priority: kando_core::Priority::Normal,
        tags: vec![],
        assignees: vec![],
        blocked: None,
        due_offset_days: None,
        body: String::new(),
    };
}

/// Verify that re-exported storage types are accessible at the crate root.
#[test]
fn re_export_storage_types_accessible() {
    let _mode = kando_core::BoardMode::Local;
}

/// Verify that re-exported config types are accessible and roundtrip.
#[test]
fn re_export_config_types_accessible() {
    let cfg = kando_core::LocalConfig::default();
    let serialized = toml::to_string_pretty(&cfg).unwrap();
    let _loaded: kando_core::LocalConfig = toml::from_str(&serialized).unwrap();

    let toml = kando_core::KandoToml { branch: "main".into() };
    let serialized = toml::to_string_pretty(&toml).unwrap();
    let _loaded: kando_core::KandoToml = toml::from_str(&serialized).unwrap();

    // BoardConfig is accessible (no Default, just verify the type resolves)
    fn _assert_board_config_type(_: kando_core::BoardConfig) {}
}

/// Verify that submodule paths are accessible through the library crate.
#[test]
fn submodule_paths_accessible() {
    // board::age
    let _staleness = kando_core::board::age::staleness;

    // board::storage
    let _init = kando_core::board::storage::init_board;

    // board::hooks
    let _scaffold = kando_core::board::hooks::scaffold_hook;

    // board::metrics
    let _compute = kando_core::board::metrics::compute_metrics;

    // board::sync
    let _find = kando_core::board::sync::find_git_root;
}
