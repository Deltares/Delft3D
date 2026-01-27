"""Tests for download_and_install_artifacts.py."""

from unittest.mock import MagicMock, Mock, call, patch

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.download_and_install_artifacts import ArtifactInstaller
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings, TeamcityIds
from ci_tools.example_utils.logger import LogLevel


class TestDownloadAndInstallArtifacts:
    """Test cases for download_and_install_artifacts function."""

    def test_download_and_install_artifacts_success(self) -> None:
        """Test successful execution of download_and_install_artifacts."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.dimr_version = "1.23.45"
        mock_context.branch_name = "main"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "test_host"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact.zip"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact.tar.gz"

        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {
            "file": [{"name": "windows_artifact.zip"}, {"name": "linux_artifact.tar.gz"}]
        }
        mock_services.teamcity.get_build_artifact.return_value = b"fake_content"
        mock_services.teamcity.get_branch_name.return_value = "main"
        mock_services.ssh = Mock(spec=SshClient)

        installer = ArtifactInstaller(mock_context, mock_services)

        # Act
        with patch.object(installer, "_ArtifactInstaller__extract_archive"):
            installer.execute_step()

        # Assert
        expected_calls = [
            call("Downloading and installing artifacts..."),
            call("Artifacts download and installation completed successfully!"),
        ]
        mock_context.log.assert_has_calls(expected_calls)

    @patch("builtins.print")
    def test_download_and_install_artifacts_dry_run(self, mock_print: MagicMock) -> None:
        """Test download_and_install_artifacts in dry run mode."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = True
        mock_context.build_id = "12345"
        mock_context.dimr_version = "1.23.45"
        mock_context.branch_name = "main"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.dry_run_prefix = "[TEST]"

        mock_services = Mock(spec=Services)

        # Act
        installer = ArtifactInstaller(mock_context, mock_services)
        installer.execute_step()

        # Assert
        expected_calls = [
            call("Downloading and installing artifacts..."),
            call(f"Download artifacts for TeamCity build ID: {mock_context.build_id}"),
            call("Publish downloaded artifacts to the designated network drive location"),
            call("Publish the weekly DIMR build via H7 deployment process"),
        ]
        mock_context.log.assert_has_calls(expected_calls)

    def test_download_and_install_artifacts_missing_teamcity(self) -> None:
        """Test download_and_install_artifacts raises error when TeamCity client is missing."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.branch_name = "main"
        mock_context.dimr_version = "1.23.45"

        mock_services = Mock(spec=Services)
        mock_services.teamcity = None
        mock_services.ssh = Mock(spec=SshClient)
        installer = ArtifactInstaller(mock_context, mock_services)

        # Act
        result = installer.execute_step()

        # Assert
        assert not result
        mock_context.log.assert_called_with("TeamCity client is required but not initialized", severity=LogLevel.ERROR)

    def test_download_and_install_artifacts_missing_ssh_client(self) -> None:
        """Test download_and_install_artifacts raises error when SSH client is missing."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.branch_name = "main"
        mock_context.dimr_version = "1.23.45"

        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.ssh = None
        installer = ArtifactInstaller(mock_context, mock_services)

        # Act
        result = installer.execute_step()

        # Assert
        assert not result
        mock_context.log.assert_called_with("SSH client is required but not initialized", severity=LogLevel.ERROR)

    @patch("ci_tools.dimrset_delivery.download_and_install_artifacts.ArtifactInstaller")
    @patch("builtins.print")
    def test_download_and_install_artifacts_prints_completion_message(
        self, mock_print: MagicMock, mock_helper_class: MagicMock
    ) -> None:
        """Test that completion message is printed."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.branch_name = "feature/test"
        mock_context.dimr_version = "2.0.0"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "test_host"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact.zip"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact.tar.gz"

        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {
            "file": [{"name": "windows_artifact.zip"}, {"name": "linux_artifact.tar.gz"}]
        }
        mock_services.teamcity.get_build_artifact.return_value = b"fake_content"
        mock_services.ssh = Mock(spec=SshClient)

        mock_helper = Mock()
        mock_helper_class.return_value = mock_helper
        installer = ArtifactInstaller(mock_context, mock_services)

        # Act
        with patch.object(installer, "_ArtifactInstaller__extract_archive"):
            installer.execute_step()

        # Assert
        mock_context.log.assert_called_with("Artifacts download and installation completed successfully!")


class TestArtifactInstaller:
    """Test cases for ArtifactInstaller class."""

    def test_download_and_deploy_artifacts_calls_teamcity(self) -> None:
        """Test that download_and_deploy_artifacts calls TeamCity for dependent builds."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "chain_build_789"
        mock_context.dimr_version = "1.0.0"
        mock_context.branch_name = "main"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"

        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {"file": []}
        mock_services.ssh = Mock(spec=SshClient)

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        # Act
        helper.execute_step()

        # Assert
        assert mock_services.teamcity.get_dependent_build_id.call_count == 2
        mock_services.teamcity.get_dependent_build_id.assert_any_call(
            mock_context.build_id, mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id
        )
        mock_services.teamcity.get_dependent_build_id.assert_any_call(
            mock_context.build_id, mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id
        )

    @patch("builtins.print")
    def test_install_dimr_on_remote_system_executes_ssh_command(self, mock_print: Mock) -> None:
        """Test that install_dimr_on_remote_system executes SSH commands."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dimr_version = "1.2.3"
        mock_context.branch_name = "main"
        mock_context.build_id = "chain_build_789"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_services = Mock(spec=Services)
        mock_services.ssh = Mock(spec=SshClient)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {"file": []}

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        # Act
        helper.execute_step()

        # Assert
        mock_services.ssh.execute.assert_called_once()
        args, kwargs = mock_services.ssh.execute.call_args
        assert "command" in kwargs
        assert "1.2.3" in kwargs["command"]
        assert "libtool_install.sh" in kwargs["command"]

    @patch("builtins.print")
    def test_install_dimr_on_remote_system_main_branch_creates_symlinks(self, mock_print: Mock) -> None:
        """Test that install_dimr_on_remote_system creates symlinks on main branch."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dimr_version = "1.2.3"
        mock_context.branch_name = "main"
        mock_context.build_id = "chain_build_789"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_services = Mock(spec=Services)
        mock_services.ssh = Mock(spec=SshClient)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {"file": []}

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        # Act
        helper.execute_step()

        # Assert
        args, kwargs = mock_services.ssh.execute.call_args
        command = kwargs["command"]
        assert "ln -s 1.2.3 latest;" in command
        assert "ln -s weekly/1.2.3 latest;" in command

    @patch("builtins.print")
    def test_install_dimr_on_remote_system_non_main_branch_no_symlinks(self, mock_print: Mock) -> None:
        """Test that install_dimr_on_remote_system doesn't create symlinks on non-main branches."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dimr_version = "1.2.3"
        mock_context.branch_name = "feature/test-branch"
        mock_context.build_id = "chain_build_789"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_services = Mock(spec=Services)
        mock_services.ssh = Mock(spec=SshClient)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_build_artifact_names.return_value = {"file": []}

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        # Act
        helper.execute_step()

        # Assert
        args, kwargs = mock_services.ssh.execute.call_args
        command = kwargs["command"]
        assert "ln -s" not in command
        assert "unlink latest" not in command

    def test_download_and_deploy_artifacts_handles_none_build_ids(self) -> None:
        """Test that download_and_deploy_artifacts handles None build IDs gracefully."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "chain_build_789"
        mock_context.dimr_version = "1.0.0"
        mock_context.branch_name = "main"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "windows_artifact"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "linux_artifact"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_dependent_build_id.side_effect = [None, None]
        mock_services.teamcity.get_build_artifact_names.return_value = {"file": []}

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        # Act & Assert - Should not raise exception
        helper.execute_step()

        # Verify artifact names were still requested (with empty string build IDs)
        assert mock_services.teamcity.get_build_artifact_names.call_count == 2

    def test_download_and_unpack_integration(self) -> None:
        """Integration test for the download and unpack workflow."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "chain_build_789"
        mock_context.dimr_version = "1.0.0"
        mock_context.branch_name = "main"
        mock_context.dry_run = False
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.linux_address = "H123456789"
        mock_context.settings.name_of_dimr_release_signed_windows_artifact = "dimrset_x64.zip"
        mock_context.settings.name_of_dimr_release_signed_linux_artifact = "dimrset_lnx64.tar.gz"
        mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.ssh = Mock(spec=SshClient)

        # Mock artifact names with matching artifacts
        mock_services.teamcity.get_dependent_build_id.side_effect = ["windows_build_123", "linux_build_456"]

        def get_artifact_names_side_effect(build_id: str) -> dict:
            if build_id == "windows_build_123":
                return {"file": [{"name": "dimrset_x64.zip"}]}
            elif build_id == "linux_build_456":
                return {"file": [{"name": "dimrset_lnx64.tar.gz"}]}
            else:
                return {"file": []}

        mock_services.teamcity.get_build_artifact_names.side_effect = get_artifact_names_side_effect
        mock_services.teamcity.get_build_artifact.return_value = b"fake_content"

        helper = ArtifactInstaller(
            mock_context,
            mock_services,
        )

        with patch.object(helper, "_ArtifactInstaller__extract_archive") as mock_extract:
            # Act
            helper.execute_step()

            # Assert - Verify the flow executed without errors
            assert mock_services.teamcity.get_build_artifact_names.call_count == 2
            assert mock_services.teamcity.get_build_artifact.call_count == 2
            assert mock_extract.call_count == 2
            assert mock_services.ssh.secure_copy.call_count == 2


class TestIntegration:
    """Integration test cases."""

    def test_integration_dry_run_with_real_context(self) -> None:
        """Test dry run mode with a realistic context object."""
        # Arrange
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = DimrAutomationContext(build_id="dry-run-build-456", dry_run=True)
            services = Services(context)

        context.branch_name = "dry-run-branch"
        context.dimr_version = "0.0.1"
        context.log = Mock(wraps=context.log)

        # Act
        installer = ArtifactInstaller(context, services)
        installer.execute_step()

        # Assert
        expected_calls = [
            call("Downloading and installing artifacts..."),
            call("Download artifacts for TeamCity build ID: dry-run-build-456"),
            call("Publish downloaded artifacts to the designated network drive location"),
            call("Publish the weekly DIMR build via H7 deployment process"),
        ]
        context.log.assert_has_calls(expected_calls)
