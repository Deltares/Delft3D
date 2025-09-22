#!/usr/bin/env python3
"""Generate and publish DIMR release notes (changelog)."""

import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import List

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class ReleaseNotesPublisher(StepExecutorInterface):
    """Generates a DIMR release changelog and updates the changelog file."""

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.__context = context
        self.__settings = context.settings
        self.__jira = services.jira
        self.__git = services.git
        current_dir = Path(__file__)
        path_to_output_folder = Path(current_dir.parents[0], self.__settings.relative_path_to_output_folder)
        self.__changelog_file = Path(path_to_output_folder, self.__settings.path_to_release_changelog_artifact)

    def __issue_number_pattern(self) -> re.Pattern:
        """Return the regex pattern for matching issue numbers based on teamcity_project_keys."""
        project_keys = self.__settings.teamcity_project_keys
        if not project_keys:
            self.__context.log("No project keys found in settings, issue matching may fail", severity=LogLevel.WARNING)
            return re.compile(r"\b(a|b)-\d+\b")  # Fallback pattern
        return re.compile(rf"\b({'|'.join(project_keys)})-\d+\b")

    def __build_changelog(self, commits: List[str], issue_number_pattern: re.Pattern) -> List[str]:
        """
        Build a changelog from a list of commits by extracting Jira issue numbers.

        Args:
            commits (List[str]): List of commit messages to process.
            issue_number_pattern (re.Pattern): Compiled regex pattern to match Jira issue numbers
                (e.g., 'DEVOPSDSC-123') based on project keys from settings.

        Returns
        -------
            List[str]: List of changelog entries, where each entry is either a formatted
                issue summary (e.g., '- DEVOPSDSC-123: Summary') or the raw commit message
                if no issue is found or Jira lookup fails.

        If an issue number is found in a commit and Jira is available, the issue's summary
        is retrieved and included in the changelog. If no issue number is found or Jira
        lookup fails, the raw commit message is used. Logs warnings for commits with
        issue numbers that cannot be resolved in Jira.
        """
        changelog = []
        for commit in commits:
            match = issue_number_pattern.search(commit)
            if match:
                issue_number = match.group(0)
                if self.__jira:
                    try:
                        issue = self.__jira.get_issue(issue_number)
                        if issue and "fields" in issue and "summary" in issue["fields"]:
                            summary = issue["fields"]["summary"]
                            changelog.append(f"- {issue_number}: {summary}")
                        else:
                            self.__context.log(
                                f"Failed to retrieve issue {issue_number} from Jira, using raw commit",
                                severity=LogLevel.WARNING,
                            )
                            changelog.append(f"- {commit}")
                    except Exception as e:
                        self.__context.log(
                            f"Error retrieving issue {issue_number} from Jira: {str(e)}", severity=LogLevel.WARNING
                        )
                        changelog.append(f"- {commit}")
                else:
                    changelog.append(f"- {commit}")
            else:
                changelog.append(f"- {commit}")
        return changelog

    def __prepend_or_replace_in_changelog(self, tag: str, changes: List[str], dry_run: bool) -> None:
        r"""
        Prepends a new changelog section or replaces an existing one for the given tag.

        Args:
            tag (str): The version tag for the changelog section (e.g., '1.0.0').
            changes (List[str]): List of change descriptions to include in the section.
            dry_run (bool): If True, logs the changes without writing to the file.

        The method reads the existing changelog file (if it exists) or creates a new one.
        It uses a regular expression to identify and replace an existing section for the
        given tag or prepend a new section.

        Explanation of the regex pattern:
        - `^`: Matches the start of a line (with `re.M` flag).
        - `[ \t]*`: Matches zero or more spaces or tabs before the section header.
        - `## `: Matches the literal Markdown header marker for a changelog section.
        - `{re.escape(tag)}`: Matches the escaped tag string (e.g., 'DIMRset_2.29.23').
        - `- `: Matches a hyphen and space, typically separating the tag from the date.
        - `.*?`: Non-greedy match of any characters (including newlines, due to `re.S`) until the lookahead.
        - `(?=^[ \t]*## |\Z)`: Positive lookahead to match until either the start of another
        section (with optional whitespace before `##`) or the end of the string (`\Z`).
        - Flags: `re.S` (dot matches newlines), `re.M` (multiline mode for `^` and `$`).

        If a section with the given tag exists, it is replaced with the new section, ensuring
        a blank line after the section. If no section exists, the new section is prepended after
        the '# Changelog' header with consistent spacing. In dry-run mode, changes are logged
        without modifying the file.
        """
        new_entry = [
            f"## {tag} - {datetime.now(timezone.utc).date().isoformat()}",
            "",
            *changes,
            "",
        ]
        new_text = "\n".join(new_entry)

        if self.__changelog_file.exists():
            content = self.__changelog_file.read_text(encoding="utf-8")
            self.__context.log(f"Found existing changelog at {self.__changelog_file}, will prepend/replace.")
        else:
            content = "# Changelog\n"
            self.__context.log(f"No existing changelog found at {self.__changelog_file}, creating a new one.")

        changelog_section_pattern = re.compile(rf"^[ \t]*## {re.escape(tag)} - .*?(?=^[ \t]*## |\Z)", re.S | re.M)

        if changelog_section_pattern.search(content):
            updated = changelog_section_pattern.sub(new_text + "\n", content, count=1)
            action = f"Replaced existing section for {tag}"
        else:
            updated = new_text + "\n" + content.lstrip("\n")
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
        """Execute the release notes publishing step."""
        self.__context.log("Generating DIMR release notes...")

        if self.__jira is None:
            self.__context.log("Jira client is required but not initialized", severity=LogLevel.ERROR)
            return False

        if self.__git is None:
            self.__context.log("Git client is required but not initialized", severity=LogLevel.ERROR)
            return False

        prev_tag, current_tag = self.__git.get_last_two_tags()
        self.__context.log(f"Generating changelog from {prev_tag} to {current_tag}")

        commits = self.__git.get_commits(prev_tag, current_tag)
        changelog = self.__build_changelog(commits, self.__issue_number_pattern())

        self.__prepend_or_replace_in_changelog(current_tag, changelog, dry_run=self.__context.dry_run)

        self.__context.log("Release notes generation completed successfully!")
        return True


def main() -> None:
    """Entry point for the release notes publisher."""
    args = parse_common_arguments()
    context = create_context_from_args(
        args,
        require_teamcity=False,
        require_ssh=False,
    )
    services = Services(context)

    step = ReleaseNotesPublisher(context, services)
    success = step.execute_step()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
