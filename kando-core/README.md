# kando-core

The core library behind [Kando](https://github.com/krfl/kando) — a keyboard-first Kanban board for the terminal.

This crate provides the data model, storage layer, and business logic for Kando boards. It exists as a separate crate so that other tools can read, manipulate, and analyze Kando boards programmatically without depending on the TUI.

## Features

- **Board model** — `Board`, `Column`, `Card`, `Priority`, `Template`, and `Policies` types that represent a complete Kanban board
- **Storage** — Load and save boards from the `.kando/` directory format (Markdown cards with TOML frontmatter, one file per card)
- **Board discovery** — `resolve_board()` walks up from any path to find a board, supporting both local (`.kando/`) and git-synced (shadow clone) modes
- **Git sync** — Shadow-clone based sync engine: pull, commit-and-push, offline detection, SSH agent checks
- **Metrics** — Compute cycle time, lead time, throughput, WIP, work item age, and per-stage time from board and activity log data
- **Staleness & automation** — Configurable policies for stale warnings, auto-close, auto-archive, and trash purging
- **Templates** — Card presets with priority, tags, assignees, due date offsets, and body content
- **Hooks** — Fire post-event scripts with structured environment variables; async-safe notification channel for TUI integration
- **Trash & archive** — Soft-delete with restore, archive management, and timed purge
- **Activity log** — Append-only event log for audit trails and metrics computation

## Usage

```rust
use kando_core::{BoardContext, Board, Card, Column, Priority};

// Discover and load a board
let ctx = kando_core::board::storage::resolve_board(&std::env::current_dir()?)?;
let board = kando_core::board::storage::load_board(&ctx.kando_dir)?;

for col in &board.columns {
    println!("{}  ({} cards)", col.name, col.cards.len());
    for card in &col.cards {
        println!("  [{}] {}", card.id, card.title);
    }
}
```

## Card format

Cards are Markdown files with TOML frontmatter, stored one per file inside column directories:

```markdown
---
id = "1"
title = "Fix login bug"
tags = ["bug", "auth"]
assignees = ["alice"]
priority = "high"
created = "2024-01-15T10:30:00Z"
updated = "2024-01-16T14:22:00Z"
---

Card body in Markdown.
```

## License

Apache-2.0
