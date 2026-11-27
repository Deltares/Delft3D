#!/usr/bin/env python3
"""Regenerate structured spec changelog JSON files by diffing two git refs.

Compares the specification files as they exist at two arbitrary refs
(commits, tags, branches) directly, e.g. a release tag and the tip of main.
The result is the absolute/net diff between the two snapshots, not a list of
the intermediate commits.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from data.ini_spec import IniFileSpec
from data.ini_spec_changelog import ChangelogDocument
from compare_ini_specs import create_change_list

REPO_ROOT = Path(__file__).resolve().parent.parent
SPECIFICATIONS_FOLDER = REPO_ROOT / "specifications"
CHANGELOG_FOLDER = REPO_ROOT / "changelogs"
CHANGELOG_SCHEMA_VERSION = "1.0.0"


def is_spec_json(filepath: str) -> bool:
    return (
        filepath.startswith("specifications/")
        and filepath.endswith(".json")
        and not filepath.startswith("specifications/json-schemas/")
    )


def git(*args):
    return subprocess.run(
        ["git", *args],
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=True,
        cwd=REPO_ROOT,
    )


def get_file_at_ref(filepath: str, ref: str) -> str | None:
    result = git("show", f"{ref}:./{filepath}")
    return result.stdout if result.returncode == 0 else None


def get_changed_spec_files(source_ref: str, target_ref: str) -> list[str]:
    """Return specification JSON files that differ between two refs (snapshot diff)."""

    diff = git("diff", "--relative", "--name-only", source_ref, target_ref, "--", "specifications/")
    return [f for f in diff.stdout.strip().splitlines() if is_spec_json(f)]


def _sanitize_ref(ref: str) -> str:
    """Make a git ref safe for use in a filename."""

    return re.sub(r"[^A-Za-z0-9._-]+", "-", ref).strip("-")


def _changelog_path(filepath: str, suffix: str) -> Path:
    spec_path = REPO_ROOT / filepath
    relative_path = spec_path.relative_to(SPECIFICATIONS_FOLDER)
    return (
        CHANGELOG_FOLDER / relative_path.parent / f"{suffix}_{spec_path.stem}_changelog.json"
    )


def _write_changelogs(documents_by_file: dict[str, ChangelogDocument], suffix: str):
    for filepath, document in documents_by_file.items():
        changelog_path = _changelog_path(filepath, suffix)
        changelog_path.parent.mkdir(parents=True, exist_ok=True)

        with open(changelog_path, "w", encoding="utf-8") as handle:
            json.dump(document.to_dict(), handle, indent=2, ensure_ascii=True)
            handle.write("\n")

        print(f"Wrote changelog to {changelog_path}")


def run(
    source_ref: str,
    target_ref: str,
    cicd: bool = False,
    source_label: str | None = None,
):
    """Diff the specification files at two refs directly, ignoring intermediate commits."""

    changed_spec_files = get_changed_spec_files(source_ref, target_ref)

    changelog_per_spec_file: dict[str, ChangelogDocument] = {}

    source_ref_name = source_label if source_label is not None else source_ref
    target_ref_name = "main" if cicd else target_ref

    for spec_file_path in changed_spec_files:
        old_content = get_file_at_ref(spec_file_path, source_ref)
        new_content = get_file_at_ref(spec_file_path, target_ref)

        if old_content is None or new_content is None:
            print(f"  skipping non-diffable file lifecycle change in {spec_file_path}")
            continue

        old_spec = IniFileSpec.from_dict(json.loads(old_content))
        new_spec = IniFileSpec.from_dict(json.loads(new_content))
        changes = create_change_list(old_spec, new_spec)

        changelog_per_spec_file[spec_file_path] = ChangelogDocument(
            schema_version=CHANGELOG_SCHEMA_VERSION,
            specification_file=spec_file_path,
            source_ref=source_ref_name,
            target_ref=target_ref_name,
            changes=changes,
        )

    suffix = f"{_sanitize_ref(source_ref_name)}_to_{_sanitize_ref(target_ref_name)}"
    _write_changelogs(changelog_per_spec_file, suffix=suffix)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source_ref", help="The older git ref (commit, tag, or branch) to compare from.")
    parser.add_argument("target_ref", help="The newer git ref (commit, tag, or branch) to compare to.")
    parser.add_argument("--cicd", action="store_true", help="Enable CI/CD mode, which sets the target ref to main.")
    parser.add_argument("--source-label", help="Display name for the source ref in changelog content and filenames.")
    return parser.parse_args(argv)


if __name__ == "__main__":
    try:
        args = parse_args(sys.argv[1:])
        run(args.source_ref, args.target_ref, args.cicd, args.source_label)
    except Exception as e:
        print(e)
        sys.exit(1)