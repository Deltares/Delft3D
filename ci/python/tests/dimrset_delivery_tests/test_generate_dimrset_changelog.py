"""Tests for publish_release_changelog.py."""

import re
import sys
from pathlib import Path
from unittest.mock import Mock

import pytest

from ci_tools.dimrset_delivery import publish_release_changelog as changelog_module
from ci_tools.dimrset_delivery.publish_release_changelog import ChangeLogPublisher
from ci_tools.dimrset_delivery.services import Services
from ci_tools.example_utils.logger import LogLevel


class TestPublishReleaseChangelog:
    """Test cases for the publish_release_changelog step."""

    def setup_method(self) -> None:
        """Set up test fixtures before each test method."""
        # Arrange - Common test data
        self.mock_context = Mock()
        self.mock_context.dry_run = False
        self.mock_context.settings.path_to_release_changelog_artifact = "dimrset_release_changelog.html"
        self.mock_context.settings.relative_path_to_output_folder = "output"
        self.mock_context.settings.teamcity_project_keys = ["UNST", "DEVOPSDSC"]
        self.mock_context.settings.issuetracker_url = "https://jira.example.com/browse"

        self.mock_services = Mock(spec=Services)
        self.mock_services.jira = Mock()
        self.mock_services.git = Mock()
        self.mock_services.ssh = Mock()

        self.helper = ChangeLogPublisher(context=self.mock_context, services=self.mock_services)

    # ---------------- execute_step ----------------

    def test_execute_step_successful_execution(self) -> None:
        """Test successful execution of publish_release_changelog step."""
        # Arrange
        self.mock_services.git.get_last_two_tags.return_value = ("tag1", "tag2")
        self.mock_services.git.get_commits.return_value = [("abc", "UNST-123 Fix bug")]
        self.mock_services.ssh.secure_copy.side_effect = [None, None]  # download, upload

        # Act
        result = self.helper.execute_step()

        # Assert
        assert result is True
        self.mock_context.log.assert_any_call("DIMRset changelog generation completed successfully!")

    def test_execute_step_dry_run(self) -> None:
        """Test publish_release_changelog in dry run mode."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_services.git.get_last_two_tags.return_value = ("tag1", "tag2")
        self.mock_services.git.get_commits.return_value = [("abc", "UNST-123 Test")]

        # Act
        result = self.helper.execute_step()

        # Assert
        assert result is True
        self.mock_context.log.assert_any_call("Dry run mode, skipping file sync")

    def test_execute_step_ssh_missing(self) -> None:
        """Test execution fails if SSH client is missing."""
        # Arrange
        self.mock_services.ssh = None
        self.mock_services.git.get_last_two_tags.return_value = ("tag1", "tag2")
        self.mock_services.git.get_commits.return_value = [("abc", "UNST-123 Test")]
        helper = ChangeLogPublisher(context=self.mock_context, services=self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("SSH client is required but not initialized", severity=LogLevel.ERROR)

    def test_execute_step_jira_missing(self) -> None:
        """Test execution fails if Jira client is missing."""
        # Arrange
        self.mock_services.jira = None
        helper = ChangeLogPublisher(context=self.mock_context, services=self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("Jira client is required but not initialized", severity=LogLevel.ERROR)

    def test_execute_step_git_missing(self) -> None:
        """Test execution fails if Git client is missing."""
        # Arrange
        self.mock_services.git = None
        helper = ChangeLogPublisher(context=self.mock_context, services=self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("Git client is required but not initialized", severity=LogLevel.ERROR)

    def test_execute_step_download_assertionerror(self) -> None:
        """Test execution when secure_copy download raises AssertionError."""
        # Arrange
        self.mock_services.git.get_last_two_tags.return_value = ("tag1", "tag2")
        self.mock_services.git.get_commits.return_value = [("abc", "UNST-456 Another fix")]
        self.mock_services.ssh.secure_copy.side_effect = [AssertionError("not found"), None]

        # Act
        result = self.helper.execute_step()

        # Assert
        assert result is True
        self.mock_context.log.assert_any_call(
            "No existing changelog found at /p/d-hydro/dimrset/dimrset_release_changelog.html, using local file",
            severity=LogLevel.WARNING,
        )

    # ---------------- __build_changelog ----------------

    def test_build_changelog_with_jira_summary(self) -> None:
        """Test __build_changelog when Jira issue has a summary."""
        self.mock_services.jira.get_issue.return_value = {"fields": {"summary": "Fix login"}}
        commits = [("abc123", "UNST-100 Fix login bug")]
        result = self.helper._ChangeLogPublisher__build_changelog(commits, re.compile(r"(?:UNST|DEVOPSDSC)-\d+"))
        assert '<a href="https://jira.example.com/browse/UNST-100">UNST-100</a>' in result[0]

    def test_build_changelog_without_summary(self) -> None:
        """Test __build_changelog falls back when no Jira summary."""
        self.mock_services.jira.get_issue.return_value = {"fields": {}}
        commits = [("abc123", "DEVOPSDSC-200 Commit msg")]
        result = self.helper._ChangeLogPublisher__build_changelog(commits, re.compile(r"(?:UNST|DEVOPSDSC)-\d+"))
        assert "Commit msg" in result[0]

    def test_build_changelog_no_issue_number(self) -> None:
        """Test __build_changelog with no issue key in commit."""
        commits = [("abc123", "Some commit without issue")]
        result = self.helper._ChangeLogPublisher__build_changelog(commits, re.compile(r"(?:UNST|DEVOPSDSC)-\d+"))
        assert "Some commit" in result[0]

    def test_build_changelog_exception(self) -> None:
        """Test __build_changelog handles Jira exception gracefully."""
        self.mock_services.jira.get_issue.side_effect = Exception("boom")
        commits = [("abc123", "UNST-999 Breaks")]
        result = self.helper._ChangeLogPublisher__build_changelog(commits, re.compile(r"(?:UNST|DEVOPSDSC)-\d+"))
        assert "Breaks" in result[0]

    def test_build_changelog_returns_empty_when_no_commits(self) -> None:
        """No commits should return an empty changelog list."""
        result = self.helper._ChangeLogPublisher__build_changelog([], re.compile(r"(?:UNST|DEVOPSDSC)-\d+"))
        assert result == []

    # ---------------- __prepend_or_replace_in_changelog ----------------

    def test_prepend_or_replace_new_section(self, tmp_path: Path) -> None:
        """Test prepending a new section into changelog."""
        changelog_file = tmp_path / "changelog.html"
        self.helper._ChangeLogPublisher__changelog_file = changelog_file

        self.helper._ChangeLogPublisher__prepend_or_replace_in_changelog(
            "DIMRset_2.29.25", ["<li>Test entry</li>"], dry_run=False
        )

        content = changelog_file.read_text()
        assert "DIMRset_2.29.25" in content
        assert "Test entry" in content

    def test_prepend_or_replace_existing_section(self, tmp_path: Path) -> None:
        """Test replacing existing section in changelog."""
        changelog_file = tmp_path / "changelog.html"
        changelog_file.write_text("<h1>Changelog</h1>\n<h2>DIMRset_2.29.25 - 2025-09-29</h2>\n<ul><li>Old</li></ul>\n")
        self.helper._ChangeLogPublisher__changelog_file = changelog_file

        self.helper._ChangeLogPublisher__prepend_or_replace_in_changelog(
            "DIMRset_2.29.25", ["<li>New entry</li>"], dry_run=False
        )

        content = changelog_file.read_text()
        assert "New entry" in content
        assert "Old" not in content

    def test_prepend_or_replace_dry_run(self) -> None:
        """Test dry-run mode only logs."""
        self.helper._ChangeLogPublisher__prepend_or_replace_in_changelog(
            "DIMRset_2.29.25", ["<li>DryRun entry</li>"], dry_run=True
        )

        # Assert: at least one log entry contains DRY-RUN
        assert any(
            "DRY-RUN" in str(call) or "[DRY-RUN]" in str(call) for call in self.mock_context.log.call_args_list
        ), f"Expected a DRY-RUN log, got: {self.mock_context.log.call_args_list}"

    def test_prepend_or_replace_new_file(self, tmp_path: Path) -> None:
        """If changelog file doesn't exist, header should be prepended."""
        changelog_file = tmp_path / "CHANGELOG.html"
        self.helper._ChangeLogPublisher__changelog_file = changelog_file

        self.helper._ChangeLogPublisher__prepend_or_replace_in_changelog(
            "DIMRset_2.99.99", ["<li>New entry</li>"], dry_run=False
        )

        content = changelog_file.read_text()
        assert content.startswith("<h1>DIMRset weekly changelog</h1>")
        assert "DIMRset_2.99.99" in content

    # ---------------- __issue_number_pattern ----------------

    def test_fallback_project_keys_regex(self) -> None:
        """If no project keys are configured, fallback regex is returned and warning is logged."""
        # Arrange: no keys before creating the publisher
        self.mock_context.settings.teamcity_project_keys = []

        # Act
        publisher = ChangeLogPublisher(self.mock_context, self.mock_services)
        regex = publisher._ChangeLogPublisher__issue_number_pattern()

        # Force evaluation: actually try to match something
        assert regex.match("a-123")
        assert regex.match("b-999")
        assert not regex.match("XYZ-1")

        # Assert: warning logged
        found = any("No project keys found in settings" in str(call) for call in self.mock_context.log.call_args_list)
        assert found, f"Expected warning log, got: {self.mock_context.log.call_args_list}"


# ---------------- main ----------------


def test_main_entrypoint(monkeypatch: pytest.MonkeyPatch) -> None:
    """Cover the __main__ function."""
    monkeypatch.setattr(sys, "argv", ["prog", "--dry-run"])

    monkeypatch.setattr(
        changelog_module,
        "parse_common_arguments",
        lambda: ["--dry-run"],
    )
    monkeypatch.setattr(
        changelog_module,
        "create_context_from_args",
        lambda *a, **kw: changelog_module.Services(Mock()),
    )
    monkeypatch.setattr(
        changelog_module,
        "Services",
        lambda context: Mock(),
    )
    monkeypatch.setattr(
        changelog_module,
        "ChangeLogPublisher",
        lambda context, services: Mock(execute_step=lambda: True),
    )

    with pytest.raises(SystemExit) as excinfo:
        changelog_module.main()
    assert excinfo.value.code == 0
