use std::collections::hash_map::DefaultHasher;
use std::env;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug)]
pub struct SyncState {
    /// Path to the shadow clone in XDG cache.
    pub shadow_path: PathBuf,
    /// Branch to sync on.
    pub branch: String,
    /// Whether we're currently online (last git op succeeded).
    pub online: bool,
    /// Path to the original repo root (parent of .kando/).
    #[allow(dead_code)]
    pub repo_root: PathBuf,
    /// Last error message from a git operation, shown in the status bar.
    pub last_error: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SyncStatus {
    Updated,
    AlreadyUpToDate,
    Offline,
}

#[derive(Debug, thiserror::Error)]
pub enum SyncError {
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("not a git repository")]
    NotGitRepo,
    #[error("no remote configured")]
    NoRemote,
    #[error("git command failed: {0}")]
    GitFailed(String),
}

/// Check if a path is inside a git repository.
pub fn find_git_root(start: &Path) -> Option<PathBuf> {
    let mut dir = start.to_path_buf();
    loop {
        if dir.join(".git").exists() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Check if the remote uses SSH and warn if no ssh-agent is running.
/// Returns true if the user should be warned.
pub fn check_ssh_agent(remote_url: &str) -> bool {
    let is_ssh = remote_url.starts_with("git@")
        || remote_url.starts_with("ssh://")
        || remote_url.contains("@") && !remote_url.starts_with("http");

    if !is_ssh {
        return false;
    }

    // Check if ssh-agent is available
    match env::var("SSH_AUTH_SOCK") {
        Ok(sock) if !sock.is_empty() => {
            // Agent socket exists, check if it has keys loaded
            let output = Command::new("ssh-add").arg("-l").output();
            match output {
                Ok(out) if out.status.success() => false, // keys loaded, all good
                _ => true,                                // no keys loaded
            }
        }
        _ => true, // no agent running
    }
}

/// Build a git Command with SSH connection multiplexing enabled.
/// This reuses a single SSH connection for multiple git operations,
/// avoiding repeated passphrase prompts within the same session.
fn git_cmd(shadow_path: &Path) -> Command {
    let mut cmd = Command::new("git");
    cmd.current_dir(shadow_path);

    // Set up SSH ControlMaster for connection reuse.
    // Socket path must be short (< 104 bytes on macOS), so use /tmp.
    let mut hasher = DefaultHasher::new();
    shadow_path.hash(&mut hasher);
    let hash = hasher.finish();
    let ssh_cmd = format!(
        "ssh -o ControlMaster=auto -o ControlPath=/tmp/kando-ssh-{hash:016x} -o ControlPersist=600",
    );
    cmd.env("GIT_SSH_COMMAND", ssh_cmd);

    cmd
}

/// Get the remote URL of the git repo.
pub fn get_remote_url(repo_root: &Path) -> Result<String, SyncError> {
    let output = Command::new("git")
        .args(["remote", "get-url", "origin"])
        .current_dir(repo_root)
        .output()?;

    if !output.status.success() {
        return Err(SyncError::NoRemote);
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Compute a stable shadow path for a given repo based on its remote URL.
pub fn shadow_dir_for(remote_url: &str) -> PathBuf {
    let cache_dir = dirs::cache_dir().unwrap_or_else(|| PathBuf::from("/tmp"));
    let mut hasher = DefaultHasher::new();
    remote_url.hash(&mut hasher);
    let hash = hasher.finish();
    cache_dir.join(".kando").join(format!("{hash:016x}"))
}

/// Initialize or validate the shadow clone.
pub fn init_shadow(kando_dir: &Path, branch: &str) -> Result<SyncState, SyncError> {
    // kando_dir is .kando/, repo_root is its parent
    let repo_root = kando_dir
        .parent()
        .ok_or_else(|| SyncError::Io(std::io::Error::new(std::io::ErrorKind::NotFound, "no parent")))?;

    let git_root = find_git_root(repo_root).ok_or(SyncError::NotGitRepo)?;
    let remote_url = get_remote_url(&git_root)?;
    let shadow_path = shadow_dir_for(&remote_url);

    let ssh_warning = if check_ssh_agent(&remote_url) {
        Some("No ssh-agent keys found. Run `ssh-add` to avoid passphrase prompts.".to_string())
    } else {
        None
    };

    if shadow_path.join(".git").exists() {
        // Validate remote matches
        let shadow_remote = get_remote_url(&shadow_path).unwrap_or_default();
        if shadow_remote != remote_url {
            // Remote changed, re-clone
            std::fs::remove_dir_all(&shadow_path)?;
            clone_shadow(&remote_url, &shadow_path, branch)?;
        } else {
            // Ensure we're on the right branch and tracking remote
            let _ = git_cmd(&shadow_path)
                .args(["checkout", branch])
                .output();
            let _ = git_cmd(&shadow_path)
                .args(["branch", "--set-upstream-to", &format!("origin/{branch}"), branch])
                .output();
        }
    } else {
        clone_shadow(&remote_url, &shadow_path, branch)?;
    }

    Ok(SyncState {
        shadow_path,
        branch: branch.to_string(),
        online: true,
        repo_root: git_root,
        last_error: ssh_warning,
    })
}

fn clone_shadow(remote_url: &str, shadow_path: &Path, branch: &str) -> Result<(), SyncError> {
    std::fs::create_dir_all(shadow_path)?;

    // Try to clone with the specific branch
    let output = Command::new("git")
        .args([
            "clone",
            "--branch",
            branch,
            "--single-branch",
            remote_url,
            &shadow_path.to_string_lossy(),
        ])
        .output()?;

    if !output.status.success() {
        // Branch might not exist yet; clone default and create branch
        let _ = std::fs::remove_dir_all(shadow_path);
        std::fs::create_dir_all(shadow_path)?;

        let output = Command::new("git")
            .args(["clone", remote_url, &shadow_path.to_string_lossy()])
            .output()?;

        if !output.status.success() {
            return Err(SyncError::GitFailed(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        // Create and checkout the branch
        let _ = Command::new("git")
            .args(["checkout", "-b", branch])
            .current_dir(shadow_path)
            .output();

        // Set push upstream so first push creates the remote branch
        let _ = Command::new("git")
            .args(["config", "push.default", "current"])
            .current_dir(shadow_path)
            .output();
    }

    Ok(())
}

/// Pull latest changes from remote into shadow, then copy .kando/ to working dir.
pub fn pull(state: &mut SyncState, kando_dir: &Path) -> SyncStatus {
    // Pull in shadow
    let output = git_cmd(&state.shadow_path)
        .args(["pull", "--rebase", "--autostash"])
        .output();

    match output {
        Ok(out) if out.status.success() => {
            state.online = true;
            state.last_error = None;

            let stderr = String::from_utf8_lossy(&out.stderr);
            let stdout = String::from_utf8_lossy(&out.stdout);

            // Copy .kando/ from shadow to working dir
            let shadow_kando = state.shadow_path.join(".kando");
            if shadow_kando.exists() {
                if let Err(e) = copy_dir_contents(&shadow_kando, kando_dir) {
                    state.last_error = Some(format!("sync copy failed: {e}"));
                }
            }

            if stdout.contains("Already up to date") || stderr.contains("Already up to date") {
                SyncStatus::AlreadyUpToDate
            } else {
                SyncStatus::Updated
            }
        }
        Ok(out) => {
            let stderr = String::from_utf8_lossy(&out.stderr);
            // No tracking info means branch is new and hasn't been pushed yet — not an error
            if stderr.contains("no tracking information") {
                state.online = true;
                state.last_error = None;
                SyncStatus::AlreadyUpToDate
            } else {
                state.online = false;
                state.last_error = Some(stderr.trim().to_string());
                SyncStatus::Offline
            }
        }
        Err(e) => {
            state.online = false;
            state.last_error = Some(format!("git pull failed: {e}"));
            SyncStatus::Offline
        }
    }
}

/// Copy .kando/ from working dir to shadow, commit, and push.
pub fn commit_and_push(state: &mut SyncState, kando_dir: &Path, message: &str) {
    // Replace shadow .kando/ with working dir version to capture deletions
    let shadow_kando = state.shadow_path.join(".kando");
    let _ = std::fs::remove_dir_all(&shadow_kando);
    if let Err(e) = copy_dir_contents(kando_dir, &shadow_kando) {
        state.last_error = Some(format!("sync copy failed: {e}"));
        return;
    }

    // Stage all changes including deletions
    let _ = git_cmd(&state.shadow_path)
        .args(["add", "-A", ".kando/"])
        .output();

    // Check if there are changes to commit
    let diff_output = git_cmd(&state.shadow_path)
        .args(["diff", "--cached", "--quiet"])
        .output();

    match diff_output {
        Ok(out) if out.status.success() => {
            // No changes staged, nothing to commit
            return;
        }
        _ => {}
    }

    // Commit
    let commit_result = git_cmd(&state.shadow_path)
        .args(["commit", "-m", message])
        .output();

    if let Ok(out) = commit_result {
        if !out.status.success() {
            state.last_error = Some(
                String::from_utf8_lossy(&out.stderr).trim().to_string()
            );
            return;
        }
    }

    // Push
    let push_result = git_cmd(&state.shadow_path)
        .args(["push", "origin", &state.branch])
        .output();

    match push_result {
        Ok(out) if out.status.success() => {
            state.online = true;
            state.last_error = None;
        }
        Ok(out) => {
            state.online = false;
            state.last_error = Some(
                String::from_utf8_lossy(&out.stderr).trim().to_string()
            );
        }
        Err(e) => {
            state.online = false;
            state.last_error = Some(format!("git push failed: {e}"));
        }
    }
}

/// Recursively copy directory contents from src to dst.
///
/// Skips symlinks and removes files/dirs in dst that don't exist in src,
/// so that deletions are propagated correctly.
fn copy_dir_contents(src: &Path, dst: &Path) -> std::io::Result<()> {
    if !dst.exists() {
        std::fs::create_dir_all(dst)?;
    }

    // Collect source entry names for stale-file cleanup
    let mut src_names = std::collections::HashSet::new();

    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;

        // Skip symlinks
        if file_type.is_symlink() {
            continue;
        }

        let name = entry.file_name();
        src_names.insert(name.clone());
        let src_path = entry.path();
        let dst_path = dst.join(&name);

        if file_type.is_dir() {
            copy_dir_contents(&src_path, &dst_path)?;
        } else {
            std::fs::copy(&src_path, &dst_path)?;
        }
    }

    // Remove stale entries in dst that no longer exist in src
    for entry in std::fs::read_dir(dst)? {
        let entry = entry?;
        if !src_names.contains(&entry.file_name()) {
            let path = entry.path();
            if path.is_dir() {
                std::fs::remove_dir_all(&path)?;
            } else {
                std::fs::remove_file(&path)?;
            }
        }
    }

    Ok(())
}
