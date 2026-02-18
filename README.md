# Kando

A keyboard-first Kanban board that runs right in your terminal.

## What is Kando?

Kando helps you manage tasks and projects using the Kanban method, where work moves through columns (like "To Do", "In Progress", "Done"). Unlike web-based tools, Kando lives in your terminal and is designed to be controlled entirely with your keyboard, making it fast and distraction-free.

## Getting Started

```sh
# Install
cargo install --path .

# Create a new board in your project folder
kando init

# Open the board
kando
```

When you first open Kando, you'll see a brief tutorial showing the essential keys. Press any key to dismiss it and start using the board. You can also have kando handle syncing your board to git. See the section on [Team Collaboration](#team-collaboration) for more details.

## The Board

Your board consists of **columns** (like Backlog, In Progress, Done) that contain **cards** (individual tasks). Each card is stored as a plain Markdown file on your computer, so you can always view or edit them outside of Kando if needed.

### What You Can Do With Cards

| Feature | Description |
|---------|-------------|
| **Title & Description** | Every card has a title and a body for notes (Markdown supported) |
| **Tags** | Categorize cards with tags like `bug`, `feature`, `urgent` |
| **Assignees** | Assign cards to team members by name |
| **Priority** | Set priority level (low, normal, high, urgent) - cards auto-sort by priority |
| **Blockers** | Mark a card as blocked when something is preventing progress |
| **Age Tracking** | See how long a card has been sitting around |

### Navigating the Board

The interface is designed for speed. You don't need a mouse.

| Key | Action |
|-----|--------|
| `h` / `l` | Move between columns |
| `j` / `k` | Move between cards |
| `H` / `L` | Move the selected card left or right |
| `Enter` | Open card details |
| `Tab` | Cycle through cards across all columns |
| `u` | Undo last delete |
| `Esc` | Clear any active filters |

### Minor Modes

Kando uses "minor modes" (inspired by the Helix text editor) to keep commands discoverable without memorizing dozens of keybindings:

**Space (Command Menu)**
Press `Space` to see available actions, then press the corresponding key:
- `n` - Create a new card
- `d` - Delete the selected card
- `e` - Edit card in your text editor ($EDITOR)
- `t` - Edit tags
- `a` - Edit assignees
- `p` - Change priority
- `m` - Move card to a specific column
- `b` - Toggle blocked status
- `u` - Undo last delete

**g (Goto Menu)**
Press `g` to jump quickly:
- `1`-`9` - Jump to column 1-9
- `g` - Jump to first card in column
- `e` - Jump to last card in column
- `b` - Jump to Backlog column
- `d` - Jump to Done column

**z (View Menu)**
Press `z` then `h` to show or hide columns you've marked as hidden.

### Searching and Filtering

| Key | Action |
|-----|--------|
| `/` | Start typing to fuzzy-search card titles |
| `f` then `t` | Filter by tag |
| `f` then `a` | Filter by assignee |
| `Esc` | Clear all filters |

Filters can be combined. For example, you can filter to show only cards tagged `bug` assigned to `alice`.

### Command Mode

Press `:` to open a command line for powerful operations:
- `:move done` - Move selected card to Done column
- `:tag urgent,backend` - Add tags to selected card
- `:assign alice,bob` - Assign multiple people
- `:sort priority` - Re-sort cards

Tab completion is available for commands and arguments.

### Card Details

Press `Enter` on a card to see its full content in a detail pane. From there:
- `j`/`k` - Browse other cards without leaving the detail view
- `J`/`K` - Scroll through long card content
- `e` - Edit in your external editor
- `Esc` - Close detail view

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
- Changes are committed and pushed to your specified branch automatically
- When offline, changes are saved locally and synced when you're back online
- Each team member can work on the same board, and changes merge automatically

### WIP Limits

You can set Work-In-Progress limits on columns to prevent too many cards from piling up:

```sh
kando config wip in-progress 3
```

When a column exceeds its WIP limit, Kando will warn you before allowing more cards to be moved in.

### Auto-Close Stale Cards

Cards that sit untouched for too long can be automatically moved to an Archive column. This keeps your board clean and highlights work that may need attention.

## Command Line Interface

Kando isn't just a TUI. You can also manage cards from the command line for scripting and automation:

```sh
# Add a card
kando add "Fix login bug" -t bug,auth -p high -a alice

# List all cards
kando list

# List cards in a specific column
kando list --column done

# List cards with a specific tag
kando list --tag urgent

# Move a card
kando move 3 done

# See all tags used on the board
kando tags

# Diagnose board issues
kando doctor
```

This makes it easy to integrate Kando with git hooks, CI pipelines, or shell scripts.

## Card format

All board data lives in a `.kando/` directory in your project/branch.

Each card is a regular Markdown file with optional TOML frontmatter:

```markdown
+++
id = "1"
title = "Fix login bug"
tags = ["bug", "auth"]
assignees = ["alice"]
priority = "high"
created = 2024-01-15T10:30:00Z
updated = 2024-01-16T14:22:00Z
blocked = false
+++

## Notes

This is the card body. Use regular Markdown here.

- Step 1: Reproduce the issue
- Step 2: Fix it
- Step 3: Write tests
```

## Tips for New Users

1. **Start simple** - Just use `h/j/k/l` to navigate and `Space n` to create cards
2. **Learn modes gradually** - You don't need to memorize everything; the on-screen hints show available keys
3. **Press `Esc` to cancel** - If you're ever stuck in a mode or dialog, `Esc` will get you back to normal navigation
4. **Use `Space ?` for help** - Opens a full keybinding reference
5. **Edit in your favorite editor** - Press `Space e` to open the card in `$EDITOR` for longer writing

## Requirements

- A terminal with UTF-8 support
- For best visual results, install a Nerd Font (icons will appear as boxes otherwise)
- Git (optional, for team sync feature)
- SSH agent running (optional, for git sync over SSH)
