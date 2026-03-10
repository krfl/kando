# Kando

A keyboard-first Kanban board that runs right in your terminal.

![A screenshot of Kando](screenshot.png)

## Why Kando?

Your board is plain text. Cards are Markdown files with TOML frontmatter, columns are directories, and the whole thing lives in a `.kando/` folder you can commit alongside your code. No server, no browser, no account — just files you can grep, diff, and pipe to whatever you want. Kando adds vim-style navigation, real kanban mechanics (WIP limits, cycle-time metrics, staleness policies), and a scriptable CLI on top.

## Requirements

- A terminal with UTF-8 support
- Optional: install a [Nerd Font](https://www.nerdfonts.com/) and run with `--nerd-font` for fancier glyphs
- Git (optional, for team sync feature)
- SSH agent running (optional, for git sync over SSH)

## Getting Started

```sh
# Install
cargo install --path .

# Create a new board in your project folder
kando init

# Open the board
kando
```

The help panel opens automatically on first launch. Press `?` to reopen it at any time — it has two pages: keybinding reference and concepts. You can also have Kando handle syncing your board to git. See the section on [Team Collaboration](#team-collaboration) for more details.

## The Board

Your board consists of **columns** (like Backlog, In Progress, Done) that contain **cards** (individual tasks). Each card is stored as a plain Markdown file on your computer, so you can always view or edit them outside of Kando if needed.

### What You Can Do With Cards

| Feature | Description |
|---------|-------------|
| **Title & Description** | Every card has a title and a body for notes (Markdown supported) |
| **Tags** | Categorize cards with tags like `bug`, `feature`, `urgent` |
| **Assignees** | Assign cards to team members by name |
| **Priority** | Set priority level (low, normal, high, urgent). Cards auto-sort by priority |
| **Due Dates** | Set a due date on a card. Overdue cards get a visual warning |
| **Blockers** | Mark a card as blocked with an optional reason explaining what's preventing progress |
| **Age Tracking** | See how long a card has been sitting around |

### Navigating the Board

The interface is designed for speed. You don't need a mouse.

- `h` / `l` - Move between columns
- `j` / `k` - Move between cards
- `H` / `L` - Move the selected card left or right
- `Enter` - Open card details
- `Tab` / `Shift+Tab` - Cycle through cards across all columns
- `/` - Fuzzy search (supports `!neg` and `@user`)
- `n` / `N` - Jump to next / previous search match
- `f` - Open filter menu
- `s` - Sort current column
- `m` - Show board metrics
- `r` - Reload board from disk
- `u` - Undo last delete
- `.` - Repeat last action
- `|` - Pipe selected card to an external command
- `?` - Show keybinding reference
- `q` - Quit
- `Esc` - Clear any active filters

### Minor Modes

Kando uses "minor modes" (inspired by the Helix text editor) to keep commands discoverable without memorizing dozens of keybindings:

**Press `Space` to see available card actions:**
- `n` - Create a new card (shows template picker when templates exist)
- `d` - Delete the selected card
- `e` - Edit card in your text editor ($EDITOR)
- `t` - Edit tags
- `a` - Edit assignees
- `p` - Change priority
- `m` - Move card to a specific column
- `b` - Block (prompts for optional reason) / unblock
- `x` - Archive card
- `u` - Undo last delete

**Press `g` to jump quickly:**
- `1`-`9` - Jump to column 1-9
- `g` - Jump to first card in column
- `e` - Jump to last card in column
- `b` - Jump to Backlog
- `d` - Jump to Done

**Press `c` to manage columns:**
- `h` - Toggle focused column hidden
- `s` - Show or hide hidden columns
- `r` - Rename focused column
- `a` - Add a new column
- `d` - Delete focused column
- `w` - Set WIP limit
- `m` - Enter column move sub-mode
  - `h`/`l` to shift
  - `g`/`e` for first/last
  - `1`-`9` for position

**Press `t` to manage templates:**
- `n` - Create a new template (opens in $EDITOR)
- `e` - Edit an existing template
- `d` - Delete a template
- `r` - Rename a template
- `a` - Create a new card from a template

### Repeat Last Action

Press `.` to repeat the last mutation. The status bar shows what `.` will do (e.g. `. priority: urgent`).

**Supported actions:**

| Action | Hint | Behavior |
|--------|------|----------|
| Move card left/right | `move ->` / `move <-` | Moves selected card one column |
| Move to column | `move -> Done` | Moves selected card to the stored column |
| Set priority | `priority: high` | Applies stored priority to selected card |
| Set tags | `tags: bug, ui` | Overwrites tags on selected card |
| Set assignees | `assignees: alice` | Overwrites assignees on selected card |
| Set due date | `due: 2026-03-15` | Sets or clears due date on selected card |
| Toggle blocked | `block` / `unblock` | Sets or clears blocked status (with reason) |
| Archive | `archive` | Moves selected card to archive column |
| Delete card | `delete` | Opens confirmation prompt before deleting |
| Pipe command | `pipe: echo hi` | Re-runs stored command on selected card |
| Sort column | `sort: priority` | Re-sorts focused column by stored key |
| Remove column | `remove col` | Opens confirmation prompt before removing |
| Move column | `col ->` / `col <-` | Shifts focused column one position |
| Hide/show column | `hide col` / `show col` | Toggles column visibility |
| Set WIP limit | `wip: 3` / `wip: none` | Sets or clears WIP limit on focused column |

The hint is color-coded by risk: destructive actions (delete, remove column) appear in bold yellow, all others use the default foreground color. Destructive actions always require confirmation — `.` opens the prompt, you must press `y` to proceed.

### Searching and Filtering

**Press `/` to search (fuzzy, supports !neg and @user)**
- `n` - Jump to next
- `N` - Jump to previous

**Press `f` to filter the board**
- `t` - Filter by tag
- `a` - Filter by assignee
- `s` - Filter by staleness
- `d` - Filter by overdue
- `Esc` - Clear filters

Filters can be combined. For example, you can filter to show only cards tagged `bug` assigned to `alice`.

### Card Details

Press `Enter` on a card to see its full content in a detail pane. From there:
- `j`/`k` - Scroll through long card content
- `Tab`/`Shift+Tab` - Browse other cards without leaving the detail view
- `e` - Edit in your external editor
- `|` - Pipe card to an external command
- `Esc` or `q` - Close detail view

## Piping
Kando lets you pipe any card's raw file contents to a shell command, making it composable with the Unix ecosystem: Clipboard utilities, APIs, AI agents, issue trackers or your own scripts. The card is never modified — piping is a read-only operation.

### Environment variables
Every pipe command also receives the following environment variables so
scripts can access structured card metadata without parsing frontmatter:

| Variable | Description | Example value |
|---|---|---|
| `KANDO_CARD_ID` | Card identifier | `42` |
| `KANDO_CARD_TITLE` | Card title | `Add login page` |
| `KANDO_CARD_TAGS` | Comma-separated tags | `frontend,auth` |
| `KANDO_CARD_ASSIGNEES` | Comma-separated assignees | `alice` |
| `KANDO_CARD_PRIORITY` | Priority level (`low`, `normal`, `high`, `urgent`) | `high` |
| `KANDO_CARD_COLUMN` | Display name of the card's column | `In Progress` |
| `KANDO_CARD_DUE` | Due date (`YYYY-MM-DD`) | `2026-04-01` |
| `KANDO_CARD_BLOCKED` | Blocker reason | `waiting on API review` |

Tags, assignees, column, due, and blocked values are empty strings when unset.

## Team Collaboration

Kando can sync your board with a git repository, making it easy to collaborate with a team:

```sh
# Initialize with git sync enabled
kando init --branch kando

# Manually sync with remote
kando sync

# Check sync status
kando sync-status
```

How it works:
- Kando creates a "shadow clone" of your git repo in a hidden directory
- **TUI:** Changes are committed and pushed automatically as you work — no manual sync needed
- **CLI:** Changes are written to disk only. Run `kando sync` after CLI mutations to push them to the remote
- When offline, changes are saved locally and synced when you're back online
- Each team member can work on the same board, and changes merge automatically

## Board Policies

Kando can enforce a few lightweight rules to keep your board healthy. All thresholds are configurable and can be disabled by setting them to `0`.

### WIP Limits

Set a maximum number of cards per column. When a column is at capacity, Kando warns you before letting more cards in.

```sh
kando config wip in-progress 3
```

### Staleness Warnings

Cards that go untouched for a configurable number of days get a visual indicator so they don't slip through the cracks.

```sh
kando config stale-days 14
```

### Auto-Close

Cards that stay untouched beyond a second, longer threshold are automatically moved to a target column (typically Archive). This keeps the board clean without manual housekeeping.

```sh
kando config auto-close-days 60
kando config auto-close-target archive
```

### Auto-Archive

Completed cards can be automatically archived after a set number of days so the Done column doesn't grow forever.

```sh
kando config archive-after-days 7
```

## Command Line Interface

Kando isn't just a TUI. You can also manage cards from the command line for scripting and automation.

**Important:** CLI commands write changes to disk but do not automatically sync with the remote. If your board uses git sync, run `kando sync` after any mutations to push your changes.

```sh
# Add a card
kando add "Fix login bug" -t bug,auth -p high -a alice

# Add a card to a specific column
kando add "Review PR" -c in-progress

# Add a card from a template
kando add "Login crash" --template bug

# List all cards (filter with --column, --tag)
kando list

# Show a card's raw file
kando show 3

# Move a card
kando move 3 done

# Edit a card's metadata
kando edit 3 --priority high --tag-add backend --assignee-add alice

# Delete a card (soft-delete, recoverable from trash)
kando delete 3

# Sync changes to remote (required after CLI mutations)
kando sync
```

### Column management
```sh
kando col list
kando col add "Code Review"
kando col rename "in-progress" "Doing"
kando col move "Doing" 2
kando col hide backlog
```

### Templates
Templates reduce friction for recurring card types like bugs, features, or chores. They are stored as `.md` files with TOML frontmatter in `.kando/templates/` and can set priority, tags, assignees, blocked status, due date offset, and body content.

```sh
kando template list              # list all templates
kando template add "Bug Report"  # create and open in $EDITOR
kando template edit bug-report   # edit an existing template
kando template remove bug-report # delete a template
```

When creating a card with `--template`, the template's fields are applied first. Explicit CLI flags (`--priority`, `--due`) override the template values, while `--tags` and `--assignee` are merged with the template's values.

### Archive and trash
```sh
kando archive list
kando archive search "login"
kando archive restore 7 in-progress
kando trash                  # list trashed cards
kando trash restore 5
kando trash purge            # permanently delete trash
```

### Metrics and activity
```sh
kando metrics                # cycle time, throughput, WIP, per-stage time
kando metrics --weeks 8
kando log                    # human-readable activity feed
kando log -f                 # live-tail new activity (like tail -f)
kando log --stream           # raw JSONL stream (for scripts)
kando tags                   # list all tags with counts
```

### Configuration
```sh
kando config show
kando config wip in-progress 3
kando config stale-days 14
kando config auto-close-days 60
kando config auto-close-target archive
kando config archive-after-days 7
kando config trash-purge-days 30
kando config nerd-font on
```

### Diagnostics

If something feels off, `kando doctor` takes a look at your board and tells you what's wrong. It checks that your `.kando/` directory exists and is readable, that `config.toml` parses correctly, that every column has a matching directory on disk, and that all your cards loaded without errors. If you have git sync enabled, it goes further: verifying you're in a git repo, that a remote is configured and reachable, that your SSH agent has keys loaded (for SSH remotes), and that the shadow clone is in place. Each check gets a pass/fail with a suggested fix when something is broken.

```sh
kando doctor
```

This makes it easy to integrate Kando with git hooks, CI pipelines, or shell scripts.

### Hooks

Kando can run your own scripts whenever something happens on the board. Place executable scripts in `.kando/hooks/` named `post-<event>`, and Kando will fire them automatically after each action.

**Supported events:** `create`, `move`, `delete`, `archive`, `restore`, `auto-close`, `edit`, `priority`, `tags`, `assignees`, `blocker`, `due`, `col-add`, `col-remove`, `col-rename`, `col-move`

**Managing hooks:**

```sh
kando hooks add post-create      # create a hook and open in $EDITOR
kando hooks edit post-create     # edit an existing hook in $EDITOR
kando hooks remove post-create   # delete a hook
kando hooks list                 # list all hooks and their status
```

**Example — announce new cards with `say` on macOS:**

```sh
kando hooks add post-create
```

Then in the editor that opens, write:

```sh
#!/bin/sh
say "New card: $KANDO_CARD_TITLE"
```

Now every time a card is created (TUI or CLI), your Mac reads the title aloud.

**Environment variables passed to hooks:**

| Variable | Description |
|----------|-------------|
| `KANDO_EVENT` | The event name (e.g. `create`, `move`) |
| `KANDO_CARD_ID` | Card identifier |
| `KANDO_CARD_TITLE` | Card title |
| `KANDO_CARD_DUE` | Due date (`YYYY-MM-DD`, empty if unset) |
| `KANDO_CARD_BLOCKED` | Blocker reason (empty if unset) |
| `KANDO_BOARD_DIR` | Absolute path to the project root |
| `KANDO_FROM`, `KANDO_TO`, etc. | Extra context depending on the event |

Hooks run in background threads so they never block the TUI. The first line of stdout (or stderr) is shown as a notification in the status bar. Results are logged to `.kando/hooks.log`.

### JSON Output

All commands produce human-readable output by default. Pass the global `--json` flag to get structured JSON instead — useful for scripting and automation:

```sh
kando --json list
kando --json tags
kando --json col list
kando --json show 001
kando --json metrics
kando --json config show
kando --json doctor
kando --json log
kando --json add "New card" -t bug
kando --json move 001 done
kando --json template list
```

Mutation commands return result objects (e.g. `{"id", "title", "column"}`). Read commands return arrays or objects. `--json` is ignored by commands that don't produce data output (`init`, `sync`).

### CSV Output

All tabular commands support `--csv` for spreadsheet-friendly output. Fields containing commas or quotes are automatically escaped per RFC 4180. `--csv` conflicts with `--json` — use one or the other.

```sh
kando list --csv
kando tags --csv
kando col list --csv
kando trash --csv
kando archive list --csv
kando archive search "login" --csv
kando metrics --csv
kando template list --csv
```

This makes it easy to pipe board data into other tools:

```sh
kando list --csv | cut -d, -f1,5    # just IDs and titles
kando metrics --csv > report.csv     # export to spreadsheet
kando tags --csv | sort -t, -k2 -rn # tags sorted by count
```

## Card format

All board data lives in a `.kando/` directory in your project/branch.

Each card is a regular Markdown file with TOML frontmatter:

```markdown
---
id = "1"
title = "Fix login bug"
tags = ["bug", "auth"]
assignees = ["alice"]
priority = "high"
created = "2024-01-15T10:30:00Z"
updated = "2024-01-16T14:22:00Z"
blocked = "waiting on API review"
---

## Notes

This is the card body. Use regular Markdown here.

- Step 1: Reproduce the issue
- Step 2: Fix it
- Step 3: Write tests
```
