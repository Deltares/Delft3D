#!/usr/bin/env python3
"""Generate and publish DIMRset release changelog."""

import re
import sys
from pathlib import Path
from typing import List, Tuple

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.ssh_client import Direction
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class ChangeLogPublisher(StepExecutorInterface):
    """Generates a DIMRset changelog and updates the changelog file."""

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.__context = context
        self.__settings = context.settings
        self.__jira = services.jira
        self.__git = services.git
        self.__ssh = services.ssh
        current_dir = Path(__file__)
        self.__path_to_output_folder = Path(current_dir.parents[0], self.__settings.relative_path_to_output_folder)
        self.__changelog_file = Path(self.__path_to_output_folder, self.__settings.path_to_release_changelog_artifact)

    def __issue_number_pattern(self) -> re.Pattern:
        """Return the regex pattern for matching issue numbers based on teamcity_project_keys."""
        project_keys = self.__settings.teamcity_project_keys
        if not project_keys:
            self.__context.log("No project keys found in settings, issue matching may fail", severity=LogLevel.WARNING)
            return re.compile(r"\b(a|b)-\d+\b")  # Fallback pattern
        return re.compile(rf"\b({'|'.join(project_keys)})-\d+(?=\b|[^A-Za-z0-9])")

    def __normalize_issue_keys(self, commits: List[Tuple[str, str]], project_keys: List[str]) -> List[Tuple[str, str]]:
        """
        Normalize issue keys in commit messages by ensuring they match the format PROJECT-123.

        Args
        ----
            commits (List[Tuple[str, str]]): List of (commit_hash, commit_message).
            project_keys (List[str]): List of valid project keys (e.g., ['DEVOPSDSC', 'UNST']).

        Returns
        -------
            List[Tuple[str, str]]: Normalized commits with corrected issue keys in the message.
        """
        if not project_keys:
            return commits

        pattern = re.compile(rf"\b((?:{'|'.join(project_keys)}))\s*(\d+)\b", re.IGNORECASE)

        normalized_commits: List[Tuple[str, str]] = []
        for commit_hash, message in commits:
            normalized_message = pattern.sub(lambda m: f"{m.group(1).upper()}-{m.group(2)}", message)
            normalized_commits.append((commit_hash, normalized_message))

        return normalized_commits

    def __build_changelog(self, commits: list[tuple[str, str]], issue_number_pattern: re.Pattern) -> list[str]:
        """
        Build an HTML-formatted changelog from a list of commits by extracting and resolving Jira issue keys.

        Args
        ----
            commits (list[tuple[str, str]]): A list of (commit_hash, commit_message) tuples.
            issue_number_pattern (re.Pattern): Compiled regex pattern to detect Jira issue keys
                (e.g., 'UNST-9186', 'DEVOPSDSC-123') based on configured project keys.

        Returns
        -------
            list[str]: A list of HTML `<li>` elements, where each entry represents a commit.
                - If a Jira key is found and can be resolved, the entry is formatted as:
                `<li><a href="{jira_url}/{key}">{key}</a>: {summary} ({commit_hash})</li>`
                - If a Jira key is found but cannot be resolved, the raw commit message is used instead.
                - If no Jira key is found, the raw commit message is included unchanged.

        Behavior
        --------
            - Multiple Jira keys in a single commit message are all detected and hyperlinked.
            - Merge commits are normalized so they only include the issue key(s) and summary,
            rather than the full merge message.
            - Falls back gracefully to the original commit message if Jira lookups fail.
            - Logs warnings for missing or unresolved Jira issues.
        """
        changelog = []
        jira_base_url = getattr(self.__settings, "issuetracker_url", "").rstrip("/")

        for commit_hash, message in commits:
            match = issue_number_pattern.search(message)
            if match:
                issue_number = match.group(0)
                if self.__jira:
                    try:
                        issue = self.__jira.get_issue(issue_number)
                        if issue and "fields" in issue and "summary" in issue["fields"]:
                            summary = issue["fields"]["summary"]
                            changelog.append(
                                f"<li>"
                                f'<a href="{jira_base_url}/{issue_number}">{issue_number}</a>: '
                                f"{summary} ({commit_hash})"
                                f"</li>"
                            )
                        else:
                            self.__context.log(
                                f"Failed to retrieve issue {issue_number} from Jira, using raw commit",
                                severity=LogLevel.WARNING,
                            )
                            changelog.append(f"<li>{message} ({commit_hash})</li>")
                    except Exception as e:
                        self.__context.log(
                            f"Error retrieving issue {issue_number} from Jira: {str(e)}",
                            severity=LogLevel.WARNING,
                        )
                        changelog.append(f"<li>{message} ({commit_hash})</li>")
                else:
                    changelog.append(f"<li>{message} ({commit_hash})</li>")
            else:
                changelog.append(f"<li>{message} ({commit_hash})</li>")

        return changelog

    def __prepend_or_replace_in_changelog(self, tag: str, changes: List[str], dry_run: bool) -> None:
        r"""
        Insert or update a changelog section for the given tag in an HTML-formatted changelog file.

        Parameters
        ----------
        tag : str
            The version tag for the changelog section (e.g., 'DIMRset_2.29.25').
        changes : List[str]
            List of change entries in HTML `<li>` format.
        dry_run : bool
            If True, only logs the intended changes without modifying the file.

        Behavior
        --------
        - If the changelog file exists, it is read; otherwise, a new file is initialized
        with a top-level `<h1>Changelog</h1>` header.
        - A new section consists of:
            * `<h2>{tag}</h2>` as the section header.
            * A `<ul>` block containing the list of changes.
        - If a section for the given tag already exists, it is fully replaced.
        - If no section exists, the new section is inserted directly below the
        `<h1>Changelog</h1>` header, ensuring the newest release appears first.

        Regex Details
        -------------
        - `<h2>{tag} - ...</h2>`: Matches the section header for the given tag.
        - `.*?</ul>`: Non-greedy match capturing the full `<ul>` block.
        - Flags:
            * `re.S` → allows `.` to span multiple lines.
            * `re.M` → enables `^` and `$` to work across lines.

        Notes
        -----
        - In `dry_run` mode, the generated section and final file content are logged
        but not written to disk.
        - Guarantees the changelog remains valid, structured HTML.
        """
        new_entry = [
            f"<h2>{tag}</h2>",
            "<ul>",
            *changes,
            "</ul>",
            "",
        ]
        new_text = "\n".join(new_entry)

        if self.__changelog_file.exists():
            content = self.__changelog_file.read_text(encoding="utf-8")
        else:
            content = "<h1>DIMRset weekly changelog</h1>\n\n"

        # Regex to detect an existing section by tag
        changelog_section_pattern = re.compile(rf"<h2>{re.escape(tag)}.*?</ul>", re.DOTALL)

        if changelog_section_pattern.search(content):
            updated = changelog_section_pattern.sub(new_text, content, count=1)
            action = f"Replaced existing section for {tag}"
        else:
            # Insert new release *just after the main header*
            header_pattern = re.compile(r"(<h1>.*?<\/h1>\s*)", re.S | re.M)
            if header_pattern.search(content):
                updated = header_pattern.sub(r"\1" + new_text + "\n", content, count=1)
            else:
                updated = "<h1>DIMRset weekly changelog</h1>\n\n" + new_text + "\n" + content
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

    def __sync_changelog_file(self, current_tag: str, changelog: list[str]) -> bool:
        """
        Synchronize the changelog file with the remote location.

        Behavior
        --------
        - Attempts to download the existing changelog from the remote path.
        - If no changelog exists remotely, falls back to the local file.
        - Prepends or replaces the changelog section for the given tag.
        - Uploads the updated changelog back to the remote location.
        """
        if self.__ssh is None:
            self.__context.log("SSH client is required but not initialized", severity=LogLevel.ERROR)
            return False

        path_to_release_notes_file = f"/p/d-hydro/dimrset/{self.__context.settings.path_to_release_changelog_artifact}"

        self.__context.log(f"Downloading changelog from {path_to_release_notes_file}")
        try:
            self.__ssh.secure_copy(
                str(self.__path_to_output_folder),
                path_to_release_notes_file,
                Direction.FROM,
            )
        except AssertionError:
            self.__context.log(
                f"No existing changelog found at {path_to_release_notes_file}, using local file",
                severity=LogLevel.WARNING,
            )

        self.__context.log("Prepending/replacing changelog section")
        self.__prepend_or_replace_in_changelog(current_tag, changelog, dry_run=self.__context.dry_run)

        self.__context.log(f"Copying {self.__changelog_file.name} to {path_to_release_notes_file}")
        self.__ssh.secure_copy(
            str(self.__changelog_file),
            path_to_release_notes_file,
            Direction.TO,
        )
        self.__context.log(f"Changelog copied successfully to {path_to_release_notes_file}")
        return True

    def execute_step(self) -> bool:
        """Execute the changelog publishing step."""
        self.__context.log("Generating DIMRset changelog...")

        if self.__jira is None:
            self.__context.log("Jira client is required but not initialized", severity=LogLevel.ERROR)
            return False

        if self.__git is None:
            self.__context.log("Git client is required but not initialized", severity=LogLevel.ERROR)
            return False

        prev_tag, current_tag = self.__git.get_last_two_tags()
        self.__context.log(f"Generating changelog from {prev_tag} to {current_tag}")

        commits = self.__git.get_commits(prev_tag, current_tag)
        commits = self.__normalize_issue_keys(commits, self.__settings.teamcity_project_keys)

        changelog = self.__build_changelog(commits, self.__issue_number_pattern())

        self.__context.log("DIMRset changelog generation completed successfully!")

        if self.__context.dry_run:
            self.__context.log("Dry run mode, skipping file sync")
            return True

        if not self.__sync_changelog_file(current_tag, changelog):
            return False

        return True


def main() -> None:
    """Entry point for the changelog publisher."""
    args = parse_common_arguments()
    context = create_context_from_args(args, require_teamcity=False)
    services = Services(context)

    step = ChangeLogPublisher(context, services)
    success = step.execute_step()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
