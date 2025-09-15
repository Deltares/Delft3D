#!/usr/bin/env python3
import re
import subprocess
import requests
import os
import sys
from datetime import date
from pathlib import Path
import argparse

parser = argparse.ArgumentParser(description='Publish DIMRset release notes')
parser.add_argument('--output_dir', type=str, required=True, help='The output dir of the changelog', default="/opt/repository/Delft3D-DIMRset")
parser.add_argument('--dry_run', action='store_true', help='Do not write file, only show result')
args = parser.parse_args()

# --- Configuration ---
JIRA_BASE_URL = "https://publicwiki.deltares.nl"
JIRA_USER = os.getenv("JIRA_USER")
JIRA_API_TOKEN = os.getenv("JIRA_API_TOKEN")
JIRA_PROJECT_KEYS = [
    "DEVOPSDSC", "UNST", "DELFT3D", "RTCTOOLS", "ECMODULE", "SOFTSUP",
    "SWAN", "ESIWACE3", "COMPCORE", "DELWAQ"
]

CHANGELOG_DIR = args.output_dir
CHANGELOG_FILE = Path(f"{CHANGELOG_DIR}/dimrset_release_changelog.txt")

# Regex to capture JIRA issue prefixes like DEVOPSDSC-123
ISSUE_KEY_PATTERN = re.compile(rf"\b({'|'.join(JIRA_PROJECT_KEYS)})-\d+\b")

def run_git_command(cmd):
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return result.stdout.strip()

def get_tags():
    return run_git_command(["git", "tag", "--sort=creatordate"]).splitlines()

def get_last_two_tags():
    tags = get_tags()
    if len(tags) < 2:
        raise RuntimeError("Not enough tags found in repository.")
    return tags[-2], tags[-1]

def get_commits(from_tag, to_tag):
    log = run_git_command(["git", "log", f"{from_tag}..{to_tag}", "--pretty=format:%s"])
    return log.splitlines()

def fetch_jira_summary(issue_key):
    url = f"{JIRA_BASE_URL}/rest/api/3/issue/{issue_key}"
    try:
        response = requests.get(url, auth=(JIRA_USER, JIRA_API_TOKEN))
        if response.status_code == 200:
            return response.json()["fields"]["summary"]
    except Exception as e:
        print(f"Error fetching {issue_key}: {e}")
    return None

def build_changelog(commits):
    changelog = []
    for commit in commits:
        match = ISSUE_KEY_PATTERN.search(commit)
        if match:
            issue_key = match.group(0)
            title = fetch_jira_summary(issue_key)
            if title:
                changelog.append(f"- {issue_key}: {title}")
            else:
                changelog.append(f"- {commit}")
        else:
            changelog.append(f"- {commit}")
    return changelog

def prepend_or_replace_in_changelog(tag, changes, dry_run=False):
    new_entry = [
        f"## {tag} - {date.today().isoformat()}",
        "",
        *changes,
        "",
    ]
    new_text = "\n".join(new_entry)

    if CHANGELOG_FILE.exists():
        content = CHANGELOG_FILE.read_text(encoding="utf-8")
    else:
        content = "# Changelog\n\n"

    pattern = re.compile(rf"^## {re.escape(tag)} - .*?(?=^## |\Z)", re.S | re.M)

    if pattern.search(content):
        updated = pattern.sub(new_text, content, count=1)
        action = f"Would replace existing section for {tag}"
    else:
        updated = new_text + "\n" + content
        action = f"Would prepend new section for {tag}"

    if dry_run:
        print(f"[DRY-RUN] {action}\n")
        print("----- NEW ENTRY -----")
        print(new_text)
        print("----- FINAL RESULT -----")
        print(updated)
    else:
        CHANGELOG_FILE.parent.mkdir(parents=True, exist_ok=True)
        CHANGELOG_FILE.write_text(updated, encoding="utf-8")
        print(f"Changelog updated in {CHANGELOG_FILE}")

def main():
    prev_tag, current_tag = get_last_two_tags()
    print(f"Generating changelog from {prev_tag} to {current_tag}...\n")

    commits = get_commits(prev_tag, current_tag)
    changelog = build_changelog(commits)

    prepend_or_replace_in_changelog(current_tag, changelog, dry_run=args.dry_run)

if __name__ == "__main__":
    try:
        main()
        sys.exit(0)
    except Exception as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)
