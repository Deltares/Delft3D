#!/usr/bin/env python3
"""Generate and publish DIMR release notes (changelog)."""

import sys
import re
import subprocess
from datetime import date
from pathlib import Path
from typing import List

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class ReleaseNotesPublisher(StepExecutorInterface):
    """
    Generates a DIMR release changelog and updates the changelog file.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.__context = context
        self.__jira = services.jira
        self.__changelog_file = Path("ci_tools/dimrset_delivery/output/dimrset_release_changelog.txt")

    def __run_git_command(self, cmd: List[str]) -> str:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return result.stdout.strip()

    def __get_last_two_tags(self) -> (str, str):
        tags = self.__run_git_command(["git", "tag", "--sort=creatordate"]).splitlines()
        if len(tags) < 2:
            raise RuntimeError("Not enough tags found in repository.")
        return tags[-2], tags[-1]

    def __get_commits(self, from_tag: str, to_tag: str) -> List[str]:
        log = self.__run_git_command(["git", "log", f"{from_tag}..{to_tag}", "--pretty=format:%s"])
        return log.splitlines()

    def __build_changelog(self, commits: List[str], issue_number_pattern: re.Pattern) -> List[str]:
        changelog = []
        for commit in commits:
            match = issue_number_pattern.search(commit)
            if match:
                issue_number = match.group(0)
                issue = self.__jira.get_issue(issue_number) if self.__jira else None
                if issue:
                    summary = issue["fields"]["summary"]
                    changelog.append(f"- {issue_number}: {summary}")
                else:
                    changelog.append(f"- {commit}")
            else:
                changelog.append(f"- {commit}")
        return changelog

    def __prepend_or_replace_in_changelog(self, tag: str, changes: List[str], dry_run: bool) -> None:
        new_entry = [
            f"## {tag} - {date.today().isoformat()}",
            "",
            *changes,
            "",
        ]
        new_text = "\n".join(new_entry)

        if self.__changelog_file.exists():
            content = self.__changelog_file.read_text(encoding="utf-8")
        else:
            content = "# Changelog\n\n"

        pattern = re.compile(rf"^## {re.escape(tag)} - .*?(?=^## |\Z)", re.S | re.M)

        if pattern.search(content):
            updated = pattern.sub(new_text, content, count=1)
            action = f"Replaced existing section for {tag}"
        else:
            updated = new_text + "\n" + content
            action = f"Prepended new section for {tag}"

        if dry_run:
            self.__context.log(f"[DRY-RUN] {action}")
            self.__context.log("----- NEW ENTRY -----")
            self.__context.log(new_text)
            self.__context.log("----- FINAL RESULT -----")
            self.__context.log(updated)
        else:
            self.__changelog_file.parent.mkdir(parents=True, exist_ok=True)
            self.__changelog_file.write_text(updated, encoding="utf-8")
            self.__context.log(f"Changelog updated in {self.__changelog_file}")

    def execute_step(self) -> bool:
        self.__context.log("Generating DIMR release notes...")

        if self.__jira is None:
            self.__context.log("Jira client is required but not initialized", severity=LogLevel.ERROR)
            return False

        prev_tag, current_tag = self.__get_last_two_tags()
        self.__context.log(f"Generating changelog from {prev_tag} to {current_tag}")

        commits = self.__get_commits(prev_tag, current_tag)

        project_keys = [
            "DEVOPSDSC", "UNST", "DELFT3D", "RTCTOOLS", "ECMODULE", "SOFTSUP",
            "SWAN", "ESIWACE3", "COMPCORE", "DELWAQ"
        ]
        issue_number_pattern = re.compile(rf"\b({'|'.join(project_keys)})-\d+\b")

        changelog = self.__build_changelog(commits, issue_number_pattern)

        self.__prepend_or_replace_in_changelog(current_tag, changelog, dry_run=self.__context.dry_run)

        self.__context.log("Release notes generation completed successfully!")
        return True

def main() -> None:
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False, require_teamcity=False, require_ssh=False)
    services = Services(context)

    step = ReleaseNotesPublisher(context, services)
    success = step.execute_step()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
