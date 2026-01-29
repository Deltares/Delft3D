"""Tests for git_tagging.py."""

from unittest.mock import Mock

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.git_tagging import GitTagger
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.example_utils.logger import LogLevel


class TestGitTagging:
    """Test cases for git_tagging function."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.build_id = "12345"
        self.mock_context.dry_run = False
        self.mock_context.settings = Mock(spec=Settings)
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        self.mock_context.log = Mock()
        self.mock_context.kernel_versions = {"build.vcs.number": "abc123def"}
        self.mock_context.dimr_version = "1.2.3"

        self.mock_services = Mock(spec=Services)
        self.mock_services.git = Mock(spec=GitClient)
        self.mock_services.teamcity = Mock(spec=TeamCity)

    def test_git_tagging_success(self) -> None:
        step = GitTagger(self.mock_context, self.mock_services)
        result = step.execute_step()

        assert result is True
        self.mock_services.git.tag_commit.assert_called_once_with("abc123def", "DIMRset_1.2.3")

    def test_git_tagging_missing_git_client(self) -> None:
        """Test error when Git client is None."""
        # Arrange
        self.mock_services.git = None
        helper = GitTagger(self.mock_context, self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("Git client is required but not initialized.", severity=LogLevel.ERROR)

    def test_git_tagging_git_client_exception(self) -> None:
        """Test when Git client raises an exception during tagging."""
        # Arrange
        self.mock_services.git.tag_commit.side_effect = Exception("Git error")
        helper = GitTagger(self.mock_context, self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("Error during tagging: Git error", severity=LogLevel.ERROR)

    def test_git_tagging_dry_run_git_client_required(self) -> None:
        """Test that dry run without git client."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_services.git = None
        helper = GitTagger(self.mock_context, self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("Git client is required but not initialized.", severity=LogLevel.ERROR)

    def test_git_tagging_missing_teamcity_client(self) -> None:
        """Test error when TeamCity client is None."""
        # Arrange
        self.mock_services.teamcity = None
        helper = GitTagger(self.mock_context, self.mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call(
            "TeamCity client is required but not initialized.", severity=LogLevel.ERROR
        )
