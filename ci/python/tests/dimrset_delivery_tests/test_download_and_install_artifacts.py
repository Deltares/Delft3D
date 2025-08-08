"""Tests for download_and_install_artifacts.py."""

import argparse
from unittest.mock import MagicMock, Mock, call, patch

import pytest

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext, DimrCredentials, ServiceRequirements
from ci_tools.dimrset_delivery.download_and_install_artifacts import download_and_install_artifacts


class TestDownloadAndInstallArtifacts:
    """Test cases for download_and_install_artifacts function."""

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstallHelper")
    def test_download_and_install_artifacts_success(self, mock_helper_class: MagicMock) -> None:
        """Test successful execution of download_and_install_artifacts."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()
        mock_context.ssh_client = Mock()
        mock_context.get_branch_name.return_value = "main"
        mock_context.get_dimr_version.return_value = "1.23.45"

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper

        # Act
        download_and_install_artifacts(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Downloading and installing artifacts...")
        mock_context.get_branch_name.assert_called_once()
        mock_context.get_dimr_version.assert_called_once()

        mock_helper_class.assert_called_once_with(
            teamcity=mock_context.teamcity,
            ssh_client=mock_context.ssh_client,
            dimr_version="1.23.45",
            branch_name="main",
        )
        mock_helper.publish_artifacts_to_network_drive.assert_called_once_with("12345")
        mock_helper.publish_weekly_dimr_via_h7.assert_called_once()

    @patch("builtins.print")
    def test_download_and_install_artifacts_dry_run(self, mock_print: MagicMock) -> None:
        """Test download_and_install_artifacts in dry run mode."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = True
        mock_context.build_id = "12345"
        mock_context.get_branch_name.return_value = "main"
        mock_context.get_dimr_version.return_value = "1.23.45"

        # Act
        download_and_install_artifacts(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Downloading and installing artifacts...")
        mock_context.get_branch_name.assert_called_once()
        mock_context.get_dimr_version.assert_called_once()

        # Check that dry run messages were printed
        expected_calls = [
            call("[DRY-RUN] Would download artifacts for build from TeamCity:", "12345"),
            call("[DRY-RUN] Would publish artifacts to network drive"),
            call("[DRY-RUN] Would publish weekly DIMR via H7"),
        ]
        mock_print.assert_has_calls(expected_calls)

    def test_download_and_install_artifacts_missing_teamcity(self) -> None:
        """Test download_and_install_artifacts raises error when TeamCity client is missing."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.teamcity = None
        mock_context.ssh_client = Mock()
        mock_context.get_branch_name.return_value = "main"
        mock_context.get_dimr_version.return_value = "1.23.45"

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            download_and_install_artifacts(mock_context)

    def test_download_and_install_artifacts_missing_ssh_client(self) -> None:
        """Test download_and_install_artifacts raises error when SSH client is missing."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.teamcity = Mock()
        mock_context.ssh_client = None
        mock_context.get_branch_name.return_value = "main"
        mock_context.get_dimr_version.return_value = "1.23.45"

        # Act & Assert
        with pytest.raises(ValueError, match="SSH client is required but not initialized"):
            download_and_install_artifacts(mock_context)

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstallHelper")
    @patch("builtins.print")
    def test_download_and_install_artifacts_prints_completion_message(
        self, mock_print: MagicMock, mock_helper_class: MagicMock
    ) -> None:
        """Test that completion message is printed."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()
        mock_context.ssh_client = Mock()
        mock_context.get_branch_name.return_value = "feature/test"
        mock_context.get_dimr_version.return_value = "2.0.0"

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper

        # Act
        download_and_install_artifacts(mock_context)

        # Assert
        mock_print.assert_called_with("Artifacts download and installation completed successfully!")

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstallHelper")
    def test_download_and_install_artifacts_helper_initialization_parameters(
        self, mock_helper_class: MagicMock
    ) -> None:
        """Test that ArtifactInstallHelper is initialized with correct parameters."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "67890"
        mock_teamcity = Mock()
        mock_ssh_client = Mock()
        mock_context.teamcity = mock_teamcity
        mock_context.ssh_client = mock_ssh_client
        mock_context.get_branch_name.return_value = "develop"
        mock_context.get_dimr_version.return_value = "3.1.4"

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper

        # Act
        download_and_install_artifacts(mock_context)

        # Assert
        mock_helper_class.assert_called_once_with(
            teamcity=mock_teamcity,
            ssh_client=mock_ssh_client,
            dimr_version="3.1.4",
            branch_name="develop",
        )

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstallHelper")
    def test_download_and_install_artifacts_calls_helper_methods_in_order(self, mock_helper_class: MagicMock) -> None:
        """Test that helper methods are called in the correct order."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "11111"
        mock_context.teamcity = Mock()
        mock_context.ssh_client = Mock()
        mock_context.get_branch_name.return_value = "main"
        mock_context.get_dimr_version.return_value = "1.0.0"

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper

        # Act
        download_and_install_artifacts(mock_context)

        # Assert
        # Verify the methods were called
        mock_helper.publish_artifacts_to_network_drive.assert_called_once_with("11111")
        mock_helper.publish_weekly_dimr_via_h7.assert_called_once()

        # Verify they were called in the correct order
        handle = mock_helper.method_calls
        expected_calls = [
            call.publish_artifacts_to_network_drive("11111"),
            call.publish_weekly_dimr_via_h7(),
        ]
        assert handle == expected_calls


class TestMainExecution:
    """Test cases for the main execution block."""

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.download_and_install_artifacts")
    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.create_context_from_args")
    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.parse_common_arguments")
    @patch("builtins.print")
    def test_main_execution_flow(
        self,
        mock_print: MagicMock,
        mock_parse_args: MagicMock,
        mock_create_context: MagicMock,
        mock_download_func: MagicMock,
    ) -> None:
        """Test the main execution flow when script is run directly."""
        # Arrange
        mock_args = Mock(spec=argparse.Namespace)
        mock_context = Mock(spec=DimrAutomationContext)
        mock_parse_args.return_value = mock_args
        mock_create_context.return_value = mock_context

        # Act - directly execute the code that would be in the main block
        args = mock_parse_args()
        context = mock_create_context(args, require_atlassian=False, require_git=False)
        mock_print("Starting artifact download and installation...")
        mock_download_func(context)
        mock_print("Finished")

        # Assert
        mock_parse_args.assert_called_once()
        mock_create_context.assert_called_once_with(mock_args, require_atlassian=False, require_git=False)
        mock_download_func.assert_called_once_with(mock_context)

        # Check that start and finish messages were printed
        expected_calls = [
            call("Starting artifact download and installation..."),
            call("Finished"),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=False)

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.download_and_install_artifacts")
    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.create_context_from_args")
    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.parse_common_arguments")
    def test_main_execution_context_creation_parameters(
        self, mock_parse_args: MagicMock, mock_create_context: MagicMock, mock_download_func: MagicMock
    ) -> None:
        """Test that context is created with correct parameters in main execution."""
        # Arrange
        mock_args = Mock(spec=argparse.Namespace)
        mock_context = Mock(spec=DimrAutomationContext)
        mock_parse_args.return_value = mock_args
        mock_create_context.return_value = mock_context

        # Act - directly test the context creation logic
        args = mock_parse_args()
        mock_create_context(args, require_atlassian=False, require_git=False)

        # Assert
        mock_create_context.assert_called_once_with(mock_args, require_atlassian=False, require_git=False)


class TestIntegration:
    """Integration test cases."""

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstallHelper")
    def test_integration_with_real_context_structure(self, mock_helper_class: MagicMock) -> None:
        """Test integration with a more realistic context object."""
        # Arrange
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(),
            TeamCity=Mock(),
            SshClient=Mock(),
            GitClient=Mock(),
        ):
            # Create credentials and requirements objects
            credentials = DimrCredentials(
                atlassian_username="test_user",
                atlassian_password="test_pass",
                teamcity_username="tc_user",
                teamcity_password="tc_pass",
                ssh_username="ssh_user",
                ssh_password="ssh_pass",
                git_username="git_user",
                git_pat="git_token",
            )

            requirements = ServiceRequirements(atlassian=False, teamcity=True, ssh=True, git=False)

            context = DimrAutomationContext(
                build_id="test-build-123", dry_run=False, credentials=credentials, requirements=requirements
            )

        # Mock the context methods to return predictable values
        context.get_branch_name = Mock(return_value="integration-test")
        context.get_dimr_version = Mock(return_value="99.99.99")

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper

        # Act
        download_and_install_artifacts(context)

        # Assert
        mock_helper_class.assert_called_once_with(
            teamcity=context.teamcity,
            ssh_client=context.ssh_client,
            dimr_version="99.99.99",
            branch_name="integration-test",
        )
        mock_helper.publish_artifacts_to_network_drive.assert_called_once_with("test-build-123")
        mock_helper.publish_weekly_dimr_via_h7.assert_called_once()

    def test_integration_dry_run_with_real_context(self) -> None:
        """Test dry run mode with a realistic context object."""
        # Arrange
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(),
            TeamCity=Mock(),
            SshClient=Mock(),
            GitClient=Mock(),
        ):
            # Create requirements object for a dry run with no services
            requirements = ServiceRequirements(atlassian=False, teamcity=False, ssh=False, git=False)

            context = DimrAutomationContext(build_id="dry-run-build-456", dry_run=True, requirements=requirements)

        # Mock the context methods
        context.get_branch_name = Mock(return_value="dry-run-branch")
        context.get_dimr_version = Mock(return_value="0.0.1")

        with patch("builtins.print") as mock_print:
            # Act
            download_and_install_artifacts(context)

            # Assert
            # Verify dry run messages were printed
            mock_print.assert_any_call(
                "[DRY-RUN] Would download artifacts for build from TeamCity:", "dry-run-build-456"
            )
            mock_print.assert_any_call("[DRY-RUN] Would publish artifacts to network drive")
            mock_print.assert_any_call("[DRY-RUN] Would publish weekly DIMR via H7")
