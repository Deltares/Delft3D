"""Tests for assert_preconditions.py."""

from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.dimrset_delivery.step_0_assert_preconditions import PreconditionsChecker
from ci_tools.example_utils.logger import LogLevel


class TestAssertPreconditionsFunction:
    """Test cases for the assert_preconditions function."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.dry_run = False
        self.mock_context.settings = Mock(spec=Settings)
        self.mock_context.settings.linux_address = "test_host"
        self.mock_context.settings.dry_run_prefix = "[TEST]"

        self.mock_services = Mock(Services)
        self.mock_services.teamcity = Mock(spec=TeamCity)
        self.mock_services.git = Mock(spec=GitClient)
        self.mock_services.ssh = Mock(spec=SshClient)

    def test_assert_preconditions_success(self) -> None:
        """Test successful preconditions check."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = True
        self.mock_services.ssh.test_connection.return_value = True
        self.mock_services.git.test_connection.return_value = True

        # Act
        checker = PreconditionsChecker(self.mock_context, self.mock_services)
        result = checker.execute_step()

        # Assert
        assert result
        self.mock_services.teamcity.test_connection.assert_called_once()
        self.mock_services.ssh.test_connection.assert_called_once()
        self.mock_services.git.test_connection.assert_called_once()

    def test_assert_preconditions_teamcity_failure(self) -> None:
        """Test preconditions check fails when TeamCity connection fails."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = False
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act
        result = checker.execute_step()

        # Assert
        assert not result
        self.mock_context.log.assert_any_call("Failed to connect to the TeamCity REST API.", severity=LogLevel.ERROR)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_ssh_failure(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when SSH connection fails."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_services.ssh.test_connection.side_effect = ConnectionError("SSH connection failed")
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act
        result = checker.execute_step()

        # Assert
        assert not result
        self.mock_context.log.assert_any_call(
            "Exception during connection check: SSH connection failed", severity=LogLevel.ERROR
        )

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_git_failure(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when Git connection fails."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_services.ssh.test_connection.return_value = None
        self.mock_services.git.test_connection.side_effect = ConnectionError("Git connection failed")
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act
        result = checker.execute_step()

        # Assert
        assert not result
        self.mock_context.log.assert_any_call(
            "Exception during connection check: Git connection failed", severity=LogLevel.ERROR
        )

    def test_assert_preconditions_dry_run_mode(self) -> None:
        """Test preconditions check in dry-run mode."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_services.teamcity.test_connection.return_value = True
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act
        checker.execute_step()

        # Assert
        # Verify that all expected log messages are called
        expected_log_calls = [
            "Asserting preconditions...",
            "Checking connections...",
            "Testing TeamCity connection...",
            "TeamCity connection successful",
            "Testing Git connection...",
            "Git connection successful",
            "Testing SSH connection...",
            "SSH connection successful",
            "Testing Jira connection...",
            "Jira connection successful",
            "Asserted all preconditions.",
            "Preconditions check completed and returned 0 errors!",
        ]

        # Check that log was called the expected number of times
        assert self.mock_context.log.call_count == len(expected_log_calls)

        # Check that all expected log messages were called in order
        actual_calls = [call.args[0] for call in self.mock_context.log.call_args_list]
        assert actual_calls == expected_log_calls

        # Verify service method calls with dry_run=True
        self.mock_services.teamcity.test_connection.assert_called_once()
        self.mock_services.ssh.test_connection.assert_called_once()
        self.mock_services.git.test_connection.assert_called_once()

    def test_assert_preconditions_missing_teamcity(self) -> None:
        """Test preconditions assertion fails when TeamCity client is missing."""
        # Arrange
        self.mock_services.teamcity = None
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act & Assert
        result = checker.execute_step()
        assert not result
        self.mock_context.log.assert_any_call("TeamCity client is required but not initialized")

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_missing_ssh_client(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions assertion fails when SSH client is missing."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_services.ssh = None
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act & Assert
        result = checker.execute_step()
        assert not result
        self.mock_context.log.assert_any_call("SSH client is required but not initialized")

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_missing_git_client(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions assertion fails when Git client is missing."""
        # Arrange
        self.mock_services.teamcity.test_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_services.ssh.test_connection.return_value = None
        self.mock_services.git = None
        checker = PreconditionsChecker(self.mock_context, self.mock_services)

        # Act & Assert
        result = checker.execute_step()
        assert not result
        self.mock_context.log.assert_any_call("Git client is required but not initialized")
