use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::mpsc;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

use chrono::Utc;

/// Notification sent from a hook thread back to the TUI.
pub struct HookNotification {
    /// Name of the hook that ran (e.g. "post-create"). Useful for filtering
    /// or deduplicating notifications by event type.
    #[allow(dead_code)]
    pub hook_name: String,
    pub message: String,
    pub is_error: bool,
}

/// Info about a single hook slot (whether it exists, is executable, etc.).
pub struct HookInfo {
    pub name: String,
    pub path: PathBuf,
    pub exists: bool,
    pub executable: bool,
}

/// All hook events that Kando fires.
pub const ALL_HOOK_EVENTS: &[&str] = &[
    "create",
    "move",
    "delete",
    "archive",
    "restore",
    "auto-close",
    "edit",
    "priority",
    "tags",
    "assignees",
    "blocker",
    "due",
    "col-add",
    "col-remove",
    "col-rename",
    "col-move",
];

// ---------------------------------------------------------------------------
// Global sender (thread-safe, optional)
// ---------------------------------------------------------------------------

static HOOK_TX: OnceLock<Mutex<Option<mpsc::Sender<HookNotification>>>> = OnceLock::new();

fn hook_tx() -> &'static Mutex<Option<mpsc::Sender<HookNotification>>> {
    HOOK_TX.get_or_init(|| Mutex::new(None))
}

/// Register a channel sender so hook results are forwarded to the TUI.
/// When no sender is registered (CLI context), hooks still run but output
/// is only logged to `hooks.log`.
pub fn register_hook_sender(tx: mpsc::Sender<HookNotification>) {
    if let Ok(mut guard) = hook_tx().lock() {
        *guard = Some(tx);
    }
}

/// Remove the sender (call on TUI shutdown).
pub fn deregister_hook_sender() {
    if let Ok(mut guard) = hook_tx().lock() {
        *guard = None;
    }
}

// ---------------------------------------------------------------------------
// Hook naming
// ---------------------------------------------------------------------------

/// Map an activity-log action name to its hook filename.
pub fn hook_name_for_action(action: &str) -> String {
    format!("post-{action}")
}

// ---------------------------------------------------------------------------
// Hook execution
// ---------------------------------------------------------------------------

/// Best-effort read of a card's `due` and `blocked` fields from disk.
/// Returns `(due, blocked)` as strings (empty if not found or not set).
fn read_card_due_blocked(kando_dir: &Path, card_id: &str) -> (String, String) {
    let columns_dir = kando_dir.join("columns");
    let entries = match std::fs::read_dir(&columns_dir) {
        Ok(e) => e,
        Err(_) => return (String::new(), String::new()),
    };
    for entry in entries.flatten() {
        if !entry.path().is_dir() {
            continue;
        }
        let card_path = entry.path().join(format!("{card_id}.md"));
        if let Ok(contents) = std::fs::read_to_string(&card_path) {
            let due = extract_frontmatter_value(&contents, "due");
            let blocked = extract_frontmatter_value(&contents, "blocked");
            return (due, blocked);
        }
    }
    (String::new(), String::new())
}

/// Extract a single value from TOML frontmatter delimited by `---`.
fn extract_frontmatter_value(content: &str, key: &str) -> String {
    let Some(start) = content.find("---") else {
        return String::new();
    };
    let rest = &content[start + 3..];
    let Some(end) = rest.find("---") else {
        return String::new();
    };
    let frontmatter = &rest[..end];
    for line in frontmatter.lines() {
        let trimmed = line.trim();
        // Match exact key followed by optional whitespace and `=`
        if let Some(after_key) = trimmed.strip_prefix(key) {
            // Ensure the key is an exact match (next char must be whitespace or '=')
            if let Some(first) = after_key.chars().next() {
                if first != '=' && !first.is_whitespace() {
                    continue;
                }
            }
            let after_key = after_key.trim_start();
            if let Some(value) = after_key.strip_prefix('=') {
                let value = value.trim().trim_matches('"');
                if !value.is_empty() {
                    return value.to_string();
                }
            }
        }
    }
    String::new()
}

/// Fire a post-hook for the given action, if one exists.
///
/// This is called from `append_activity` so every board mutation automatically
/// triggers the corresponding hook. The hook runs in a background thread so it
/// never blocks the caller.
pub fn fire_hook(
    kando_dir: &Path,
    action: &str,
    card_id: &str,
    card_title: &str,
    extras: &[(&str, &str)],
) {
    let hooks_dir = kando_dir.join("hooks");
    let hook_name = hook_name_for_action(action);
    let hook_path = hooks_dir.join(&hook_name);

    // Early return if hooks dir or hook file doesn't exist
    if !hook_path.exists() {
        return;
    }

    // Early return if not executable (Unix only)
    if !is_executable(&hook_path) {
        return;
    }

    // Build owned copies for the thread
    let kando_dir = kando_dir.to_path_buf();
    let action = action.to_string();
    let card_id = card_id.to_string();
    let card_title = card_title.to_string();
    let extras: Vec<(String, String)> = extras
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();

    std::thread::spawn(move || {
        run_hook(
            &kando_dir,
            &hook_path,
            &hook_name,
            &action,
            &card_id,
            &card_title,
            &extras,
        );
    });
}

/// Actually execute the hook and handle logging + notification.
fn run_hook(
    kando_dir: &Path,
    hook_path: &Path,
    hook_name: &str,
    action: &str,
    card_id: &str,
    card_title: &str,
    extras: &[(String, String)],
) {
    let project_dir = kando_dir
        .parent()
        .unwrap_or(kando_dir)
        .canonicalize()
        .unwrap_or_else(|_| kando_dir.parent().unwrap_or(kando_dir).to_path_buf());

    // Read card due/blocked from disk (best-effort, empty string if not found)
    let (card_due, card_blocked) = read_card_due_blocked(kando_dir, card_id);

    let start = Instant::now();

    let result = Command::new(hook_path)
        .env("KANDO_EVENT", action)
        .env("KANDO_CARD_ID", card_id)
        .env("KANDO_CARD_TITLE", card_title)
        .env("KANDO_CARD_DUE", &card_due)
        .env("KANDO_CARD_BLOCKED", &card_blocked)
        .env("KANDO_BOARD_DIR", &project_dir)
        .envs(
            extras
                .iter()
                .map(|(k, v)| (format!("KANDO_{}", k.to_uppercase()), v.as_str())),
        )
        .output();

    let duration = start.elapsed();

    match result {
        Ok(output) => {
            let code = output.status.code().unwrap_or(-1);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            let first_line = stdout
                .lines()
                .next()
                .or_else(|| stderr.lines().next())
                .unwrap_or("")
                .to_string();

            // Log to hooks.log
            log_hook_result(
                kando_dir,
                hook_name,
                code,
                duration.as_millis(),
                &first_line,
            );

            // Send notification if TUI is listening
            let is_error = code != 0;
            let message = if is_error {
                if first_line.is_empty() {
                    format!("{hook_name} failed (exit {code})")
                } else {
                    format!("{hook_name}: {first_line}")
                }
            } else if first_line.is_empty() {
                format!("{hook_name} done")
            } else {
                format!("{hook_name}: {first_line}")
            };

            send_notification(HookNotification {
                hook_name: hook_name.to_string(),
                message,
                is_error,
            });
        }
        Err(e) => {
            let message = format!("{hook_name}: {e}");
            log_hook_result(kando_dir, hook_name, -1, duration.as_millis(), &message);
            send_notification(HookNotification {
                hook_name: hook_name.to_string(),
                message,
                is_error: true,
            });
        }
    }
}

fn send_notification(notif: HookNotification) {
    if let Ok(guard) = hook_tx().lock() {
        if let Some(ref tx) = *guard {
            let _ = tx.send(notif);
        }
    }
}

fn log_hook_result(
    kando_dir: &Path,
    hook_name: &str,
    exit_code: i32,
    duration_ms: u128,
    first_line: &str,
) {
    use std::fs::OpenOptions;
    use std::io::Write;

    let log_path = kando_dir.join("hooks.log");
    let timestamp = Utc::now().format("%Y-%m-%dT%H:%M:%SZ");

    let line = format!("{timestamp}  {hook_name}  exit={exit_code}  {duration_ms}ms  {first_line}\n");

    if let Ok(mut file) = OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_path)
    {
        let _ = file.write_all(line.as_bytes());
    }
}

// ---------------------------------------------------------------------------
// Discovery
// ---------------------------------------------------------------------------

/// List all hook slots with their status.
pub fn list_hooks(kando_dir: &Path) -> Vec<HookInfo> {
    let hooks_dir = kando_dir.join("hooks");
    ALL_HOOK_EVENTS
        .iter()
        .map(|event| {
            let name = hook_name_for_action(event);
            let path = hooks_dir.join(&name);
            let exists = path.exists();
            let executable = exists && is_executable(&path);
            HookInfo {
                name,
                path,
                exists,
                executable,
            }
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Platform helpers
// ---------------------------------------------------------------------------

#[cfg(unix)]
fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    path.metadata()
        .map(|m| m.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

#[cfg(not(unix))]
fn is_executable(_path: &Path) -> bool {
    true // On non-Unix, attempt to run
}

// ---------------------------------------------------------------------------
// CRUD helpers
// ---------------------------------------------------------------------------

fn valid_hooks_list() -> String {
    ALL_HOOK_EVENTS
        .iter()
        .map(|e| format!("post-{e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Check that `name` is a valid hook name (e.g. `post-create`).
/// Returns the event suffix on success, or an error listing valid names.
pub fn validate_hook_name(name: &str) -> Result<&str, String> {
    let suffix = name.strip_prefix("post-").ok_or_else(|| {
        format!(
            "Invalid hook name '{name}'. Hook names must start with 'post-'. Valid hooks:\n  {}",
            valid_hooks_list()
        )
    })?;

    if ALL_HOOK_EVENTS.contains(&suffix) {
        Ok(suffix)
    } else {
        Err(format!(
            "Unknown hook event '{suffix}'. Valid hooks:\n  {}",
            valid_hooks_list()
        ))
    }
}

/// Create a new hook file with a platform-appropriate starter script.
pub fn scaffold_hook(kando_dir: &Path, hook_name: &str) -> std::io::Result<PathBuf> {
    let hooks_dir = kando_dir.join("hooks");
    std::fs::create_dir_all(&hooks_dir)?;

    let path = hooks_dir.join(hook_name);

    #[cfg(unix)]
    std::fs::write(&path, "#!/usr/bin/env sh\n")?;

    #[cfg(not(unix))]
    std::fs::write(&path, "@echo off\r\n")?;

    make_executable(&path)?;
    Ok(path)
}

/// Set the executable bit on Unix; no-op on other platforms.
#[cfg(unix)]
pub fn make_executable(path: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755))
}

#[cfg(not(unix))]
pub fn make_executable(_path: &Path) -> std::io::Result<()> {
    Ok(())
}

/// Open `path` in the user's `$EDITOR`.
pub fn open_in_editor(path: &Path) -> color_eyre::Result<()> {
    use color_eyre::eyre::bail;

    #[cfg(unix)]
    let default_editor = "vi";
    #[cfg(not(unix))]
    let default_editor = "notepad";

    let editor = std::env::var("EDITOR").unwrap_or_else(|_| default_editor.to_string());
    let editor_trimmed = editor.trim();
    if editor_trimmed.is_empty() {
        bail!("$EDITOR is empty or not set");
    }

    #[cfg(unix)]
    let status = Command::new("sh")
        .arg("-c")
        .arg(format!("{editor_trimmed} \"$1\""))
        .arg("--")
        .arg(path)
        .status()?;

    #[cfg(not(unix))]
    let status = Command::new("cmd")
        .arg("/c")
        .arg(editor_trimmed)
        .arg(path)
        .status()?;

    if !status.success() {
        eprintln!("Warning: editor exited with {status}");
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::Mutex as StdMutex;

    /// Tests that use the global HOOK_TX must not run concurrently.
    static TEST_LOCK: StdMutex<()> = StdMutex::new(());

    #[test]
    fn hook_name_for_action_prefixes_post() {
        assert_eq!(hook_name_for_action("create"), "post-create");
        assert_eq!(hook_name_for_action("move"), "post-move");
        assert_eq!(hook_name_for_action("col-add"), "post-col-add");
        assert_eq!(hook_name_for_action("auto-close"), "post-auto-close");
    }

    #[test]
    fn fire_hook_no_hooks_dir_is_noop() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let tmp = tempfile::tempdir().unwrap();
        // No hooks/ dir — should not panic
        fire_hook(tmp.path(), "create", "001", "Test Card", &[]);
    }

    #[test]
    fn list_hooks_returns_all_events() {
        let tmp = tempfile::tempdir().unwrap();
        let hooks = list_hooks(tmp.path());
        assert_eq!(hooks.len(), ALL_HOOK_EVENTS.len());
        for (hook, event) in hooks.iter().zip(ALL_HOOK_EVENTS.iter()) {
            assert_eq!(hook.name, format!("post-{event}"));
            assert!(!hook.exists);
            assert!(!hook.executable);
        }
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_non_executable_is_skipped() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho hello").unwrap();
        // Remove execute permission
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o644)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);
        fire_hook(tmp.path(), "create", "001", "Test", &[]);
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Should not have received any notification since hook is not executable
        assert!(rx.try_recv().is_err());
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_executable_sends_notification() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho 'card created'").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "create", "001", "Test Card", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert_eq!(notif.hook_name, "post-create");
        assert!(notif.message.contains("card created"));
        assert!(!notif.is_error);
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_failing_script_sends_error() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-delete");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho 'oops' >&2\nexit 1").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "delete", "002", "Bad Card", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.is_error);
        assert!(notif.message.contains("oops"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn list_hooks_detects_executable() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho hi").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let hooks = list_hooks(tmp.path());
        let create_hook = hooks.iter().find(|h| h.name == "post-create").unwrap();
        assert!(create_hook.exists);
        assert!(create_hook.executable);
    }

    #[cfg(unix)]
    #[test]
    fn list_hooks_detects_non_executable() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-move");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho hi").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o644)).unwrap();

        let hooks = list_hooks(tmp.path());
        let move_hook = hooks.iter().find(|h| h.name == "post-move").unwrap();
        assert!(move_hook.exists);
        assert!(!move_hook.executable);
    }

    #[cfg(unix)]
    #[test]
    fn env_vars_include_extras_uppercased() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        // Script that prints env vars we care about
        let hook_path = hooks_dir.join("post-move");
        fs::write(
            &hook_path,
            "#!/usr/bin/env sh\necho \"from=$KANDO_FROM to=$KANDO_TO event=$KANDO_EVENT\"",
        )
        .unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(
            tmp.path(),
            "move",
            "003",
            "My Card",
            &[("from", "backlog"), ("to", "in-progress")],
        );

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.message.contains("from=backlog"));
        assert!(notif.message.contains("to=in-progress"));
        assert!(notif.message.contains("event=move"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn hook_log_written_on_execution() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho logged").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "create", "001", "Log Test", &[]);

        // Wait for notification to ensure hook finished
        let _ = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();

        let log_path = tmp.path().join("hooks.log");
        assert!(log_path.exists());
        let contents = fs::read_to_string(log_path).unwrap();
        assert!(contents.contains("post-create"));
        assert!(contents.contains("exit=0"));
        assert!(contents.contains("logged"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_success_no_output_says_done() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\n# silent hook").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "create", "001", "Silent", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(!notif.is_error);
        assert_eq!(notif.message, "post-create done");
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_failure_no_output_says_failed() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-delete");
        fs::write(&hook_path, "#!/usr/bin/env sh\nexit 42").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "delete", "002", "Fail", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.is_error);
        assert_eq!(notif.message, "post-delete failed (exit 42)");
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_stderr_used_when_stdout_empty() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        // Hook writes only to stderr but exits 0
        let hook_path = hooks_dir.join("post-edit");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho 'stderr only' >&2").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "edit", "003", "Card", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(!notif.is_error);
        assert!(notif.message.contains("stderr only"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn hook_log_records_failure_exit_code() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-delete");
        fs::write(&hook_path, "#!/usr/bin/env sh\nexit 7").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "delete", "001", "Fail", &[]);
        let _ = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();

        let contents = fs::read_to_string(tmp.path().join("hooks.log")).unwrap();
        assert!(contents.contains("exit=7"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_without_sender_still_logs() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        // Ensure no sender is registered
        deregister_hook_sender();

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho 'cli mode'").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        // Call run_hook directly (synchronous) instead of fire_hook to avoid race
        run_hook(
            tmp.path(),
            &hook_path,
            "post-create",
            "create",
            "001",
            "CLI Test",
            &[],
        );

        let log_path = tmp.path().join("hooks.log");
        assert!(log_path.exists());
        let contents = fs::read_to_string(log_path).unwrap();
        assert!(contents.contains("post-create"));
        assert!(contents.contains("exit=0"));
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_multiline_stdout_uses_first_line_only() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho 'line one'\necho 'line two'").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "create", "001", "Multi", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.message.contains("line one"));
        assert!(!notif.message.contains("line two"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_stdout_takes_priority_over_stderr() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(
            &hook_path,
            "#!/usr/bin/env sh\necho 'from stdout'\necho 'from stderr' >&2",
        )
        .unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(tmp.path(), "create", "001", "Both", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.message.contains("from stdout"));
        assert!(!notif.message.contains("from stderr"));
        deregister_hook_sender();
    }

    #[cfg(unix)]
    #[test]
    fn env_vars_include_kando_board_dir() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let kando_dir = tmp.path().join(".kando");
        fs::create_dir(&kando_dir).unwrap();
        let hooks_dir = kando_dir.join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        let hook_path = hooks_dir.join("post-create");
        fs::write(&hook_path, "#!/usr/bin/env sh\necho \"board_dir=$KANDO_BOARD_DIR\"").unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(&kando_dir, "create", "001", "Dir Test", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        // KANDO_BOARD_DIR should be the project root (parent of .kando), not .kando itself
        let canonical_root = tmp.path().canonicalize().unwrap();
        assert!(
            notif.message.contains(&canonical_root.display().to_string()),
            "Expected project root in KANDO_BOARD_DIR, got: {}",
            notif.message,
        );
        deregister_hook_sender();
    }

    #[test]
    fn fire_hook_hooks_dir_exists_but_hook_missing() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();
        // hooks/ exists but no post-create file
        fire_hook(tmp.path(), "create", "001", "Test", &[]);
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Should not panic and should not create a log entry
        assert!(!tmp.path().join("hooks.log").exists());
    }

    #[cfg(unix)]
    #[test]
    fn list_hooks_mixed_states() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();

        // Executable hook
        let exec_path = hooks_dir.join("post-create");
        fs::write(&exec_path, "#!/usr/bin/env sh").unwrap();
        fs::set_permissions(&exec_path, fs::Permissions::from_mode(0o755)).unwrap();

        // Non-executable hook
        let noexec_path = hooks_dir.join("post-move");
        fs::write(&noexec_path, "#!/usr/bin/env sh").unwrap();
        fs::set_permissions(&noexec_path, fs::Permissions::from_mode(0o644)).unwrap();

        // "post-delete" is NOT created (missing)

        let hooks = list_hooks(tmp.path());

        let create = hooks.iter().find(|h| h.name == "post-create").unwrap();
        assert!(create.exists && create.executable);

        let mv = hooks.iter().find(|h| h.name == "post-move").unwrap();
        assert!(mv.exists && !mv.executable);

        let del = hooks.iter().find(|h| h.name == "post-delete").unwrap();
        assert!(!del.exists && !del.executable);
    }

    // -----------------------------------------------------------------------
    // validate_hook_name
    // -----------------------------------------------------------------------

    #[test]
    fn validate_hook_name_accepts_post_create() {
        assert_eq!(validate_hook_name("post-create"), Ok("create"));
    }

    #[test]
    fn validate_hook_name_accepts_post_move() {
        assert_eq!(validate_hook_name("post-move"), Ok("move"));
    }

    #[test]
    fn validate_hook_name_accepts_post_col_add() {
        assert_eq!(validate_hook_name("post-col-add"), Ok("col-add"));
    }

    #[test]
    fn validate_hook_name_accepts_post_auto_close() {
        assert_eq!(validate_hook_name("post-auto-close"), Ok("auto-close"));
    }

    #[test]
    fn validate_hook_name_accepts_all_events() {
        for event in ALL_HOOK_EVENTS {
            let name = format!("post-{event}");
            assert_eq!(
                validate_hook_name(&name),
                Ok(*event),
                "Expected Ok for {name}"
            );
        }
    }

    #[test]
    fn validate_hook_name_rejects_missing_prefix() {
        let err = validate_hook_name("create").unwrap_err();
        assert!(err.contains("must start with 'post-'"), "got: {err}");
    }

    #[test]
    fn validate_hook_name_rejects_unknown_event() {
        let err = validate_hook_name("post-foobar").unwrap_err();
        assert!(err.contains("Unknown hook event 'foobar'"), "got: {err}");
    }

    #[test]
    fn validate_hook_name_rejects_empty_string() {
        assert!(validate_hook_name("").is_err());
    }

    #[test]
    fn validate_hook_name_rejects_just_prefix() {
        let err = validate_hook_name("post-").unwrap_err();
        assert!(err.contains("Unknown hook event"), "got: {err}");
    }

    #[test]
    fn validate_hook_name_rejects_wrong_prefix() {
        let err = validate_hook_name("pre-create").unwrap_err();
        assert!(err.contains("must start with 'post-'"), "got: {err}");
    }

    #[test]
    fn validate_hook_name_rejects_case_mismatch() {
        assert!(validate_hook_name("post-Create").is_err());
        assert!(validate_hook_name("POST-CREATE").is_err());
    }

    // -----------------------------------------------------------------------
    // scaffold_hook
    // -----------------------------------------------------------------------

    #[test]
    fn scaffold_hook_creates_hooks_dir_and_file() {
        let tmp = tempfile::tempdir().unwrap();
        let path = scaffold_hook(tmp.path(), "post-create").unwrap();
        assert_eq!(path, tmp.path().join("hooks").join("post-create"));
        assert!(path.exists());
    }

    #[cfg(unix)]
    #[test]
    fn scaffold_hook_file_contains_shebang() {
        let tmp = tempfile::tempdir().unwrap();
        let path = scaffold_hook(tmp.path(), "post-create").unwrap();
        let content = fs::read_to_string(path).unwrap();
        assert_eq!(content, "#!/usr/bin/env sh\n");
    }

    #[cfg(unix)]
    #[test]
    fn scaffold_hook_file_is_executable() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = tempfile::tempdir().unwrap();
        let path = scaffold_hook(tmp.path(), "post-move").unwrap();
        let mode = fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o755);
    }

    #[test]
    fn scaffold_hook_hooks_dir_already_exists() {
        let tmp = tempfile::tempdir().unwrap();
        fs::create_dir(tmp.path().join("hooks")).unwrap();
        // Should not error when hooks/ already exists
        let path = scaffold_hook(tmp.path(), "post-edit").unwrap();
        assert!(path.exists());
    }

    #[test]
    fn scaffold_hook_returns_correct_path() {
        let tmp = tempfile::tempdir().unwrap();
        let path = scaffold_hook(tmp.path(), "post-delete").unwrap();
        assert_eq!(path, tmp.path().join("hooks").join("post-delete"));
    }

    #[test]
    fn scaffold_hook_overwrites_existing_file() {
        let tmp = tempfile::tempdir().unwrap();
        let hooks_dir = tmp.path().join("hooks");
        fs::create_dir(&hooks_dir).unwrap();
        let path = hooks_dir.join("post-create");
        fs::write(&path, "custom content").unwrap();

        // scaffold_hook unconditionally writes — caller is responsible for existence check
        scaffold_hook(tmp.path(), "post-create").unwrap();
        let content = fs::read_to_string(&path).unwrap();
        assert_ne!(content, "custom content");
    }

    #[test]
    fn validate_hook_name_rejects_whitespace() {
        assert!(validate_hook_name(" post-create").is_err());
        assert!(validate_hook_name("post-create ").is_err());
        assert!(validate_hook_name(" ").is_err());
    }

    // -----------------------------------------------------------------------
    // make_executable
    // -----------------------------------------------------------------------

    #[cfg(unix)]
    #[test]
    fn make_executable_sets_755() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("script");
        fs::write(&path, "#!/usr/bin/env sh\n").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o644)).unwrap();

        make_executable(&path).unwrap();
        let mode = fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o755);
    }

    #[cfg(unix)]
    #[test]
    fn make_executable_idempotent() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("script");
        fs::write(&path, "#!/usr/bin/env sh\n").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();

        make_executable(&path).unwrap();
        let mode = fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o755);
    }

    #[test]
    fn make_executable_nonexistent_file_returns_error() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("nonexistent");
        assert!(make_executable(&path).is_err());
    }

    // ── extract_frontmatter_value tests ──

    #[test]
    fn extract_frontmatter_value_basic_due() {
        let content = "---\nid = \"1\"\ndue = \"2025-12-31\"\n---\nBody";
        assert_eq!(extract_frontmatter_value(content, "due"), "2025-12-31");
    }

    #[test]
    fn extract_frontmatter_value_basic_blocked() {
        let content = "---\nblocked = \"waiting on API\"\n---\n";
        assert_eq!(extract_frontmatter_value(content, "blocked"), "waiting on API");
    }

    #[test]
    fn extract_frontmatter_value_missing_key() {
        let content = "---\nid = \"1\"\ntitle = \"Task\"\n---\nBody";
        assert_eq!(extract_frontmatter_value(content, "due"), "");
    }

    #[test]
    fn extract_frontmatter_value_no_frontmatter() {
        assert_eq!(extract_frontmatter_value("Just some text", "due"), "");
    }

    #[test]
    fn extract_frontmatter_value_single_delimiter() {
        assert_eq!(extract_frontmatter_value("---\ndue = \"2025-01-01\"\n", "due"), "");
    }

    #[test]
    fn extract_frontmatter_value_empty_value() {
        let content = "---\ndue = \"\"\n---\n";
        assert_eq!(extract_frontmatter_value(content, "due"), "");
    }

    #[test]
    fn extract_frontmatter_value_unquoted() {
        let content = "---\ndue = 2025-12-31\n---\n";
        assert_eq!(extract_frontmatter_value(content, "due"), "2025-12-31");
    }

    #[test]
    fn extract_frontmatter_value_key_prefix_no_false_positive() {
        let content = "---\ndue_offset_days = 7\n---\n";
        assert_eq!(extract_frontmatter_value(content, "due"), "");
    }

    #[test]
    fn extract_frontmatter_value_spaces_around_equals() {
        let content = "---\ndue  =  \"2025-12-31\"\n---\n";
        assert_eq!(extract_frontmatter_value(content, "due"), "2025-12-31");
    }

    #[test]
    fn extract_frontmatter_value_body_not_matched() {
        let content = "---\ntitle = \"Task\"\n---\ndue = \"fake\"";
        assert_eq!(extract_frontmatter_value(content, "due"), "");
    }

    // ── read_card_due_blocked tests ──

    #[test]
    fn read_card_due_blocked_finds_card() {
        let tmp = tempfile::tempdir().unwrap();
        let col_dir = tmp.path().join("columns").join("backlog");
        fs::create_dir_all(&col_dir).unwrap();
        fs::write(
            col_dir.join("001.md"),
            "---\nid = \"001\"\ntitle = \"T\"\ndue = \"2025-06-01\"\nblocked = \"waiting\"\ncreated = \"2025-01-01T00:00:00Z\"\nupdated = \"2025-01-01T00:00:00Z\"\n---\n",
        ).unwrap();
        let (due, blocked) = read_card_due_blocked(tmp.path(), "001");
        assert_eq!(due, "2025-06-01");
        assert_eq!(blocked, "waiting");
    }

    #[test]
    fn read_card_due_blocked_card_not_found() {
        let tmp = tempfile::tempdir().unwrap();
        let col_dir = tmp.path().join("columns").join("backlog");
        fs::create_dir_all(&col_dir).unwrap();
        let (due, blocked) = read_card_due_blocked(tmp.path(), "999");
        assert_eq!(due, "");
        assert_eq!(blocked, "");
    }

    #[test]
    fn read_card_due_blocked_no_columns_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let (due, blocked) = read_card_due_blocked(tmp.path(), "001");
        assert_eq!(due, "");
        assert_eq!(blocked, "");
    }

    #[test]
    fn read_card_due_blocked_card_without_due_or_blocked() {
        let tmp = tempfile::tempdir().unwrap();
        let col_dir = tmp.path().join("columns").join("backlog");
        fs::create_dir_all(&col_dir).unwrap();
        fs::write(
            col_dir.join("001.md"),
            "---\nid = \"001\"\ntitle = \"T\"\ncreated = \"2025-01-01T00:00:00Z\"\nupdated = \"2025-01-01T00:00:00Z\"\n---\n",
        ).unwrap();
        let (due, blocked) = read_card_due_blocked(tmp.path(), "001");
        assert_eq!(due, "");
        assert_eq!(blocked, "");
    }

    #[test]
    fn read_card_due_blocked_finds_card_in_non_first_column() {
        let tmp = tempfile::tempdir().unwrap();
        fs::create_dir_all(tmp.path().join("columns").join("backlog")).unwrap();
        let col_dir = tmp.path().join("columns").join("in-progress");
        fs::create_dir_all(&col_dir).unwrap();
        fs::write(
            col_dir.join("001.md"),
            "---\nid = \"001\"\ntitle = \"T\"\ndue = \"2025-06-01\"\ncreated = \"2025-01-01T00:00:00Z\"\nupdated = \"2025-01-01T00:00:00Z\"\n---\n",
        ).unwrap();
        let (due, blocked) = read_card_due_blocked(tmp.path(), "001");
        assert_eq!(due, "2025-06-01");
        assert_eq!(blocked, "");
    }

    #[cfg(unix)]
    #[test]
    fn fire_hook_env_includes_card_due_and_blocked() {
        use std::os::unix::fs::PermissionsExt;
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let tmp = tempfile::tempdir().unwrap();
        let kando_dir = tmp.path().join(".kando");

        // Create card on disk
        let col_dir = kando_dir.join("columns").join("backlog");
        fs::create_dir_all(&col_dir).unwrap();
        fs::write(
            col_dir.join("001.md"),
            "---\nid = \"001\"\ntitle = \"T\"\ndue = \"2025-06-15\"\nblocked = \"needs review\"\ncreated = \"2025-01-01T00:00:00Z\"\nupdated = \"2025-01-01T00:00:00Z\"\n---\n",
        ).unwrap();

        // Create hook
        let hooks_dir = kando_dir.join("hooks");
        fs::create_dir(&hooks_dir).unwrap();
        let hook_path = hooks_dir.join("post-create");
        fs::write(
            &hook_path,
            "#!/usr/bin/env sh\necho \"due=$KANDO_CARD_DUE blocked=$KANDO_CARD_BLOCKED\"",
        ).unwrap();
        fs::set_permissions(&hook_path, fs::Permissions::from_mode(0o755)).unwrap();

        let (tx, rx) = mpsc::channel();
        register_hook_sender(tx);

        fire_hook(&kando_dir, "create", "001", "T", &[]);

        let notif = rx.recv_timeout(std::time::Duration::from_secs(5)).unwrap();
        assert!(notif.message.contains("due=2025-06-15"), "msg: {}", notif.message);
        assert!(notif.message.contains("blocked=needs review"), "msg: {}", notif.message);
        deregister_hook_sender();
    }
}
