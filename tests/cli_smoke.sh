#!/usr/bin/env bash
#
# CLI smoke tests for kando.
# Runs through the full offline CLI surface in a temp directory.
# Run from the repo root: bash tests/cli_smoke.sh
#
set -euo pipefail

PASS=0
FAIL=0
TMPDIR=""

cleanup() {
    if [ -n "$TMPDIR" ] && [ -d "$TMPDIR" ]; then
        rm -rf "$TMPDIR"
    fi
    echo
    echo "================================"
    echo "  Passed: $PASS   Failed: $FAIL"
    echo "================================"
    if [ "$FAIL" -gt 0 ]; then
        exit 1
    fi
}
trap cleanup EXIT

assert() {
    local name="$1"
    shift
    if "$@" >/dev/null 2>&1; then
        echo "  PASS  $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $name"
        FAIL=$((FAIL + 1))
    fi
}

assert_output() {
    local name="$1"
    local pattern="$2"
    shift 2
    local output
    if output=$("$@" 2>&1) && echo "$output" | grep -q "$pattern"; then
        echo "  PASS  $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $name (expected pattern: $pattern)"
        echo "        got: $output"
        FAIL=$((FAIL + 1))
    fi
}

assert_json_valid() {
    local name="$1"
    shift
    local output
    if output=$("$@" 2>&1) && echo "$output" | python3 -m json.tool >/dev/null 2>&1; then
        echo "  PASS  $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $name (invalid JSON)"
        echo "        got: $output"
        FAIL=$((FAIL + 1))
    fi
}

# ── Setup ──

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "Building kando..."
cargo build --quiet --manifest-path "$REPO_ROOT/Cargo.toml" -p kando-tui
KANDO="$REPO_ROOT/target/debug/kando"

TMPDIR=$(mktemp -d)
cd "$TMPDIR"

echo "Working directory: $TMPDIR"
echo

# ── Basic CLI ──

echo "--- Basic CLI ---"
assert "version flag"        "$KANDO" --version
assert "help flag"           "$KANDO" --help

# ── Init ──

echo "--- Init ---"
assert "init board"          "$KANDO" init --name "Smoke Test"
assert ".kando dir exists"   test -d .kando
assert "config.toml exists"  test -f .kando/config.toml

# ── Add cards ──
# Card IDs are sequential: 1, 2, 3, 4

echo "--- Add cards ---"
assert_output "add card 1"   "Fix login bug" \
    "$KANDO" add "Fix login bug" -t bug,auth -p high -a alice
assert_output "add card 2"   "Write tests" \
    "$KANDO" add "Write tests" -t test -p normal
assert_output "add card 3"   "Update docs" \
    "$KANDO" add "Update docs" -p low -a bob --due 2099-12-31
assert_output "add card to column" "Deploy fix" \
    "$KANDO" add "Deploy fix" -c in-progress

# ── List ──

echo "--- List ---"
assert_output "list all"         "Fix login bug" "$KANDO" list
assert_output "list by tag"      "Fix login bug" "$KANDO" list --tag bug
assert_output "list by column"   "Deploy fix"    "$KANDO" list --column in-progress
assert_output "list overdue"     ""              "$KANDO" list --overdue

# ── Show ──

echo "--- Show ---"
assert_output "show card"    "Fix login bug" "$KANDO" show 1

# ── Edit ──

echo "--- Edit ---"
assert "edit title"          "$KANDO" edit 1 --title "Fix critical login bug"
assert_output "title changed" "Fix critical login bug" "$KANDO" show 1
assert "edit priority"       "$KANDO" edit 1 --priority urgent
assert "edit add tag"        "$KANDO" edit 1 --tag-add critical
assert "edit remove tag"     "$KANDO" edit 1 --tag-remove auth
assert "edit add assignee"   "$KANDO" edit 1 --assignee-add bob
assert "edit remove assignee" "$KANDO" edit 1 --assignee-remove alice
assert "edit set blocked"    "$KANDO" edit 1 --blocked "waiting on review"
assert "edit clear blocked"  "$KANDO" edit 1 --unblocked
assert "edit set due"        "$KANDO" edit 1 --due 2099-06-15
assert "edit clear due"      "$KANDO" edit 1 --due clear

# ── Move ──

echo "--- Move ---"
assert "move card"           "$KANDO" move 1 in-progress
assert_output "card moved"   "In Progress" "$KANDO" list --column in-progress

# ── Tags ──

echo "--- Tags ---"
assert_output "tags list"    "bug" "$KANDO" tags

# ── Columns ──

echo "--- Columns ---"
assert_output "col list"     "Backlog"     "$KANDO" col list
assert "col add"             "$KANDO" col add "Code Review"
assert_output "col added"    "Code Review" "$KANDO" col list
assert "col rename"          "$KANDO" col rename code-review "Review"
assert_output "col renamed"  "Review"      "$KANDO" col list
assert "col move"            "$KANDO" col move review 2
assert "col hide"            "$KANDO" col hide review
assert "col show"            "$KANDO" col show review
assert "col remove"          "$KANDO" col remove review

# ── Config ──

echo "--- Config ---"
assert "config show"             "$KANDO" config show
assert "config wip"              "$KANDO" config wip in-progress 3
assert "config stale-days"       "$KANDO" config stale-days 14
assert "config auto-close-days"  "$KANDO" config auto-close-days 60
assert "config auto-close-target" "$KANDO" config auto-close-target archive
assert "config archive-after-days" "$KANDO" config archive-after-days 7
assert "config trash-purge-days" "$KANDO" config trash-purge-days 30
assert "config nerd-font"        "$KANDO" config nerd-font on

# ── Metrics ──

echo "--- Metrics ---"
assert "metrics"             "$KANDO" metrics
assert "metrics with weeks"  "$KANDO" metrics --weeks 4

# ── Doctor ──

echo "--- Doctor ---"
assert "doctor"              "$KANDO" doctor

# ── Log ──

echo "--- Log ---"
assert "log"                 "$KANDO" log
assert "log stream"          "$KANDO" log --stream

# ── Archive ──

echo "--- Archive ---"
assert "archive list"         "$KANDO" archive list
assert "archive search"       "$KANDO" archive search "login"
# Move a card to done, then to archive
assert "move to done"         "$KANDO" move 2 done
assert "move to archive"      "$KANDO" move 2 archive
assert_output "archived card" "Write tests" "$KANDO" archive list
assert "archive restore"      "$KANDO" archive restore 2 --column backlog

# ── Delete & Trash ──

echo "--- Delete & Trash ---"
assert "delete card"          "$KANDO" delete 2
assert "trash list"           "$KANDO" trash
assert_output "trashed card"  "Write tests" "$KANDO" trash
assert "trash restore"        "$KANDO" trash restore 2
assert_output "restored"      "Write tests" "$KANDO" list
assert "delete again"         "$KANDO" delete 2
assert "trash purge"          "$KANDO" trash purge

# ── Templates ──

echo "--- Templates ---"
# template add/edit open $EDITOR, so we skip interactive commands
assert "template list (empty)" "$KANDO" template list

# ── Hooks ──

echo "--- Hooks ---"
assert "hooks list"           "$KANDO" hooks list

# ── JSON output ──
# Card 5 is created by "json add" below

echo "--- JSON output ---"
assert_json_valid "json list"        "$KANDO" --json list
assert_json_valid "json tags"        "$KANDO" --json tags
assert_json_valid "json col list"    "$KANDO" --json col list
assert_json_valid "json show"        "$KANDO" --json show 1
assert_json_valid "json metrics"     "$KANDO" --json metrics
assert_json_valid "json config show" "$KANDO" --json config show
assert_json_valid "json doctor"      "$KANDO" --json doctor
assert_json_valid "json log"         "$KANDO" --json log
assert_json_valid "json add"         "$KANDO" --json add "JSON card" -t test
assert_json_valid "json move"        "$KANDO" --json move 5 done
assert_json_valid "json delete"      "$KANDO" --json delete 5
assert_json_valid "json trash"       "$KANDO" --json trash
assert_json_valid "json archive list" "$KANDO" --json archive list
assert_json_valid "json template list" "$KANDO" --json template list
assert_json_valid "json hooks list"  "$KANDO" --json hooks list

# ── CSV output ──

echo "--- CSV output ---"
assert "csv list"             "$KANDO" list --csv
assert "csv tags"             "$KANDO" tags --csv
assert "csv col list"         "$KANDO" col list --csv
assert "csv metrics"          "$KANDO" metrics --csv
assert "csv trash"            "$KANDO" trash --csv
assert "csv archive list"     "$KANDO" archive list --csv
assert "csv template list"    "$KANDO" template list --csv

echo
echo "All smoke tests completed."
