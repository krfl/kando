# Kando

A keyboard-first Kanban TUI for your terminal.

## Features

- **Vim-style navigation** with Helix-inspired minor modes (`g` goto, `Space` commands, `z` view)
- **Cards as Markdown files** with TOML frontmatter in a `.kando/` directory
- **Priority sorting** &mdash; cards auto-sort by priority then recency, no manual ordering
- **Tags, blockers, and WIP limits**
- **Fuzzy search and tag filtering**
- **Edit cards in `$EDITOR`**
- **Git-based team sync** via a shadow clone &mdash; pull/push on a dedicated branch with offline fallback
- **Auto-close policies** for stale cards
- **CLI commands** for scripting: `kando add`, `kando list`, `kando move`, `kando tags`, `kando sync`

## Quick Start

```sh
cargo install --path .

# Initialize an offline board
kando init

# Initialize a synced board (uses git as backend)
kando init --branch kando

# Add cards from the CLI
kando add "My first card" -t bug,ui -p high

# Open the TUI
kando
```

## Keybindings

| Key | Action |
|-----|--------|
| `h/l` | Navigate columns |
| `j/k` | Navigate cards |
| `H/L` | Move card between columns |
| `Enter` | Card detail |
| `Space` | Command menu (new, delete, edit, tags, priority, blocker, search) |
| `g` | Goto menu (column 1-9, first/last card, backlog, done) |
| `z` | View menu (toggle hidden columns) |
| `/` | Filter cards |
| `q` | Quit |

## Coming Soon

- **Undo for card deletion** — soft delete with trash/restore
- **Card duplication** — clone existing cards
- **Column reordering** — reorder columns from the TUI
- **Column collapse** — collapse empty or inactive columns
- **Dynamic card height** — auto-size cards based on content
- **Extended CLI** — `kando delete`, `kando edit`, more `kando config` options
- **WIP limit display toggle** — show/hide WIP indicators

## SSH Setup for Team Sync

If your git remote uses SSH, you'll want `ssh-agent` running to avoid repeated passphrase prompts. Kando will warn you if no agent keys are found.

**Linux / WSL:**

```sh
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
```

Add the `eval` line to your shell config to start the agent automatically.

**macOS:**

```sh
ssh-add --apple-use-keychain ~/.ssh/id_ed25519
```

This stores the passphrase in the macOS Keychain. Add the following to `~/.ssh/config` to load it automatically:

```
Host *
  AddKeysToAgent yes
  UseKeychain yes
```
