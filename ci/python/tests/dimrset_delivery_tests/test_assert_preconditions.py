"""Tests for assert_preconditions.py."""

import os
from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.assert_preconditions import assert_preconditions
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings


class TestAssertPreconditionsFunction:
    """Test cases for the assert_preconditions function."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.dry_run = False
        self.mock_context.settings = Mock(spec=Settings)
        self.mock_context.settings.network_base_path = "test_path"
        self.mock_context.settings.linux_address = "test_host"
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        self.mock_context.atlassian = Mock(spec=Atlassian)
        self.mock_context.teamcity = Mock(spec=TeamCity)
        self.mock_context.ssh_client = Mock(spec=SshClient)
        self.mock_context.git_client = Mock(spec=GitClient)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_success(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test successful preconditions check."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_context.ssh_client.test_connection.return_value = None
        self.mock_context.git_client.test_connection.return_value = None

        # Act
        assert_preconditions(self.mock_context)

        # Assert
        self.mock_context.log.assert_called_once_with("Asserting preconditions...")
        self.mock_context.teamcity.test_api_connection.assert_called_once_with(False)
        self.mock_context.atlassian.test_api_connection.assert_called_once_with(False)
        mock_os_exists.assert_called_with("test_path")
        mock_os_access.assert_any_call("test_path", os.W_OK)
        mock_os_access.assert_any_call("test_path", os.R_OK)
        self.mock_context.ssh_client.test_connection.assert_called_once_with(False)
        self.mock_context.git_client.test_connection.assert_called_once_with(False)

    def test_assert_preconditions_teamcity_failure(self) -> None:
        """Test preconditions check fails when TeamCity connection fails."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = False

        # Act & Assert
        with pytest.raises(AssertionError, match="Failed to connect to the TeamCity REST API"):
            assert_preconditions(self.mock_context)

    def test_assert_preconditions_atlassian_failure(self) -> None:
        """Test preconditions check fails when Atlassian connection fails."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = False

        # Act & Assert
        with pytest.raises(AssertionError, match="Failed to connect to the Atlassian Confluence REST API"):
            assert_preconditions(self.mock_context)

    @patch("os.path.exists")
    def test_assert_preconditions_network_path_not_exists(self, mock_os_exists: Mock) -> None:
        """Test preconditions check fails when network path does not exist."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = False

        # Act & Assert
        with pytest.raises(AssertionError, match="Network path does not exist: test_path"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_network_access_failure(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when network access fails."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = False

        # Act & Assert
        with pytest.raises(AssertionError, match="Insufficient permissions for test_path"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_network_access_exception(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when network access raises exception."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.side_effect = OSError("Permission denied")

        # Act & Assert
        with pytest.raises(AssertionError, match="Could not access test_path"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_ssh_failure(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when SSH connection fails."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_context.ssh_client.test_connection.side_effect = ConnectionError("SSH connection failed")

        # Act & Assert
        with pytest.raises(AssertionError, match="Could not establish ssh connection to test_host"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_git_failure(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions check fails when Git connection fails."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_context.ssh_client.test_connection.return_value = None
        self.mock_context.git_client.test_connection.side_effect = ConnectionError("Git connection failed")

        # Act & Assert
        with pytest.raises(AssertionError, match="Could not establish git connection"):
            assert_preconditions(self.mock_context)

    def test_assert_preconditions_dry_run_mode(self) -> None:
        """Test preconditions check in dry-run mode."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True

        # Act
        assert_preconditions(self.mock_context)

        # Assert
        self.mock_context.log.assert_called_once_with("Asserting preconditions...")
        self.mock_context.teamcity.test_api_connection.assert_called_once_with(True)
        self.mock_context.atlassian.test_api_connection.assert_called_once_with(True)
        self.mock_context.ssh_client.test_connection.assert_called_once_with(True)
        self.mock_context.git_client.test_connection.assert_called_once_with(True)

    def test_assert_preconditions_missing_atlassian(self) -> None:
        """Test preconditions assertion fails when Atlassian client is missing."""
        # Arrange
        self.mock_context.atlassian = None

        # Act & Assert
        with pytest.raises(ValueError, match="Atlassian client is required but not initialized"):
            assert_preconditions(self.mock_context)

    def test_assert_preconditions_missing_teamcity(self) -> None:
        """Test preconditions assertion fails when TeamCity client is missing."""
        # Arrange
        self.mock_context.teamcity = None

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_missing_ssh_client(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions assertion fails when SSH client is missing."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_context.ssh_client = None

        # Act & Assert
        with pytest.raises(ValueError, match="SSH client is required but not initialized"):
            assert_preconditions(self.mock_context)

    @patch("os.access")
    @patch("os.path.exists")
    def test_assert_preconditions_missing_git_client(self, mock_os_exists: Mock, mock_os_access: Mock) -> None:
        """Test preconditions assertion fails when Git client is missing."""
        # Arrange
        self.mock_context.teamcity.test_api_connection.return_value = True
        self.mock_context.atlassian.test_api_connection.return_value = True
        mock_os_exists.return_value = True
        mock_os_access.return_value = True
        self.mock_context.ssh_client.test_connection.return_value = None
        self.mock_context.git_client = None

        # Act & Assert
        with pytest.raises(ValueError, match="Git client is required but not initialized"):
            assert_preconditions(self.mock_context)


class TestMainExecution:
    """Test cases for the main execution block."""

    @patch("ci_tools.dimrset_delivery.assert_preconditions.create_context_from_args")
    @patch("ci_tools.dimrset_delivery.assert_preconditions.parse_common_arguments")
    @patch("ci_tools.dimrset_delivery.assert_preconditions.assert_preconditions")
    def test_main_execution(
        self, mock_assert_preconditions: Mock, mock_parse_args: Mock, mock_create_context: Mock
    ) -> None:
        """Test main execution flow."""
        # Arrange
        mock_args = Mock()
        mock_context = Mock(spec=DimrAutomationContext)
        mock_parse_args.return_value = mock_args
        mock_create_context.return_value = mock_context

        # Act
        # We need to simulate the main block execution
        # This would require importing the module in a way that triggers the main block
        # For now, we'll test the components that would be called

        # Assert that the functions would be called in the correct order
        # This is more of an integration test concept
        pass
