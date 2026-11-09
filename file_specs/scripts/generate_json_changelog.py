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
from data.ini_spec_changelog import Change, ChangelogDocument, Commit
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
    result = git("show", f"{ref}:{filepath}")
    return result.stdout if result.returncode == 0 else None


def get_commit(ref: str) -> Commit | None:
    result = git("log", "-1", "--pretty=format:%H|%s|%ci", ref)

    if result.returncode != 0 or not result.stdout.strip():
        print(f"Could not resolve ref: {ref}")
        sys.exit(1)

    sha, subject, commit_date = result.stdout.strip().split("|", 2)

    return Commit(sha=sha, subject=subject, date=commit_date, ref=ref)


def get_changed_spec_files(from_ref: str, to_ref: str) -> list[str]:
    """Return specification JSON files that differ between two refs (snapshot diff)."""

    diff = git("diff", "--name-only", from_ref, to_ref, "--", "specifications/")
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


def run(from_ref: str, to_ref: str, cicd: bool = False):
    """Diff the specification files at two refs directly, ignoring intermediate commits."""

    from_commit = get_commit(from_ref)
    to_commit = get_commit(to_ref)

    changed_spec_files = get_changed_spec_files(from_ref, to_ref)

    changelog_per_spec_file: dict[str, ChangelogDocument] = {}

    for spec_file_path in changed_spec_files:
        old_content = get_file_at_ref(spec_file_path, from_ref)
        new_content = get_file_at_ref(spec_file_path, to_ref)

        if old_content is None or new_content is None:
            print(f"  skipping non-diffable file lifecycle change in {spec_file_path}")
            continue

        old_spec = IniFileSpec.from_dict(json.loads(old_content))
        new_spec = IniFileSpec.from_dict(json.loads(new_content))
        changes = create_change_list(old_spec, new_spec)

        changelog_per_spec_file[spec_file_path] = ChangelogDocument(
            schema_version=CHANGELOG_SCHEMA_VERSION,
            specification_file=spec_file_path,
            source_commit=from_commit,
            target_commit=to_commit,
            changes=changes,
        )

    if cicd:
        to_ref = "main"

    suffix = f"{_sanitize_ref(from_ref)}_to_{_sanitize_ref(to_ref)}"
    _write_changelogs(changelog_per_spec_file, suffix=suffix)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("from_ref", help="The older git ref (commit, tag, or branch) to compare from.")
    parser.add_argument("to_ref", help="The newer git ref (commit, tag, or branch) to compare to.")
    parser.add_argument("--cicd", action="store_true", help="Enable CI/CD mode, which sets the target ref to main.")
    return parser.parse_args(argv)


if __name__ == "__main__":
    try:
        args = parse_args(sys.argv[1:])
        run(args.from_ref, args.to_ref, args.cicd)
    except Exception as e:
        print(e)
        sys.exit(1)