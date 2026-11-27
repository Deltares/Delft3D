#!/usr/bin/env python3
"""Generate Markdown changelog files from structured JSON changelog files."""

from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from data.ini_spec_changelog import Change, ChangelogDocument, ChangeType

REPO_ROOT = Path(__file__).resolve().parent.parent
CHANGE_LOG_FOLDER = REPO_ROOT / "changelogs"

INCLUDED_FIELDS = {
    "default_value": "Default value",
    "status.value"  : "Status"
}

def iter_changelogs() -> list[Path]:
    return sorted(CHANGE_LOG_FOLDER.glob("**/*_changelog.json"))


def format_value(value: object) -> str:
    """Render values for changelog text."""
    return f'"{value}"' if value is not None else '""'


def format_changelog_line(change: Change) -> str:
    """Convert into a human-readable changelog line."""

    line = ""

    if change.field:
        formatted_field = INCLUDED_FIELDS.get(change.field, change.field)
        line = f"{formatted_field} of "

    line += f"`{change.target.identifier}` {change.type.value}"

    if change.field:
        if change.type is ChangeType.CHANGED:
            line += f" from {format_value(change.old_value)} to {format_value(change.new_value)}"
        if change.type is ChangeType.ADDED:
            line += f": {format_value(change.new_value)}"
        if change.type is ChangeType.REMOVED:
            line += f": {format_value(change.old_value)}"

    return line


def render_changelog_lines(changes: list[Change]) -> list[str]:
    """Return human-readable change strings from structured change entries."""
    lines = []

    for change in changes:
        if change.field and change.field not in INCLUDED_FIELDS:
            continue

        line = format_changelog_line(change)
        lines.append(line)

    return lines


SECTION_TITLES: list[tuple[str, ChangeType]] = [
    ("Changes", ChangeType.CHANGED),
    ("Removals", ChangeType.REMOVED),
    ("Additions", ChangeType.ADDED),
]


def format_document(document: ChangelogDocument) -> list[str]:
    lines = [f"# Changes between {document.source_ref} and {document.target_ref}\n"]

    for title, change_type in SECTION_TITLES:
        section_changes = [c for c in document.changes if c.type is change_type]
        section_lines = sorted(render_changelog_lines(section_changes))

        lines.append(f"\n## {title}\n\n")
        if section_lines:
            lines.extend(f"- {change}\n" for change in section_lines)
        else:
            lines.append("_None_\n")

    return lines


def write_markdown(change_file: Path):
    with open(change_file, encoding="utf-8") as handle:
        document = ChangelogDocument.from_dict(json.load(handle))

    lines = format_document(document)

    markdown_path = change_file.with_suffix(".md")
    with open(markdown_path, "w", encoding="utf-8") as handle:
        handle.writelines(lines)

    print(f"Wrote {markdown_path}")


def run():
    changelogs = iter_changelogs()

    for changelog in changelogs:
        write_markdown(changelog)


if __name__ == "__main__":
    try:
        run()
    except Exception as e:
        print(e)
        sys.exit(1)
