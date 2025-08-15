"""Tests for dimr_context.py."""

import argparse
from typing import Optional
from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    DimrCredentials,
    ServiceRequirements,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity


class TestDimrAutomationContext:
    """Test cases for DimrAutomationContext class."""

    def _create_context(
        self,
        build_id: str = "12345",
        dry_run: bool = False,
        atlassian_username: Optional[str] = None,
        atlassian_password: Optional[str] = None,
        teamcity_username: Optional[str] = None,
        teamcity_password: Optional[str] = None,
        ssh_username: Optional[str] = None,
        ssh_password: Optional[str] = None,
        git_username: Optional[str] = None,
        git_pat: Optional[str] = None,
        require_atlassian: bool = True,
        require_teamcity: bool = True,
        require_ssh: bool = True,
        require_git: bool = True,
        credentials: Optional[DimrCredentials] = None,
        requirements: Optional[ServiceRequirements] = None,
    ) -> DimrAutomationContext:
        """Create a DimrAutomationContext with specified parameters."""
        # If credentials and requirements are provided directly, use them
        if credentials is not None and requirements is not None:
            return DimrAutomationContext(
                build_id=build_id, dry_run=dry_run, credentials=credentials, requirements=requirements
            )

        # Otherwise, create them from the individual parameters
        if credentials is None:
            credentials = DimrCredentials(
                atlassian_username=atlassian_username,
                atlassian_password=atlassian_password,
                teamcity_username=teamcity_username,
                teamcity_password=teamcity_password,
                ssh_username=ssh_username,
                ssh_password=ssh_password,
                git_username=git_username,
                git_pat=git_pat,
            )

        if requirements is None:
            requirements = ServiceRequirements(
                atlassian=require_atlassian, teamcity=require_teamcity, ssh=require_ssh, git=require_git
            )

        return DimrAutomationContext(
            build_id=build_id, dry_run=dry_run, credentials=credentials, requirements=requirements
        )

    def test_init_with_all_credentials_provided(self) -> None:
        """Test initialization when all credentials are provided."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="atlas_user",
                atlassian_password="atlas_pass",
                teamcity_username="tc_user",
                teamcity_password="tc_pass",
                ssh_username="ssh_user",
                ssh_password="ssh_pass",
                git_username="git_user",
                git_pat="git_token",
            )

            assert context.build_id == "12345"
            assert context.dry_run is False
            assert context.atlassian is not None
            assert context.teamcity is not None
            assert context.ssh_client is not None
            assert context.git_client is not None

    def test_init_dry_run_mode(self) -> None:
        """Test initialization in dry run mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                atlassian_username="atlas_user",
                atlassian_password="atlas_pass",
                teamcity_username="tc_user",
                teamcity_password="tc_pass",
                ssh_username="ssh_user",
                ssh_password="ssh_pass",
                git_username="git_user",
                git_pat="git_token",
            )

            assert context.dry_run is True

    def test_init_with_missing_credentials_prompts_input(self) -> None:
        """Test initialization prompts for missing credentials."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
            input=Mock(side_effect=["atlas_user", "tc_user", "ssh_user", "git_user"]),
            getpass=Mock(side_effect=["atlas_pass", "tc_pass", "ssh_pass", "git_token"]),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
            )

            assert context.build_id == "12345"
            assert context.atlassian is not None
            assert context.teamcity is not None
            assert context.ssh_client is not None
            assert context.git_client is not None

    def test_init_with_partial_credentials_prompts_missing(self) -> None:
        """Test initialization prompts only for missing credentials."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
            input=Mock(side_effect=["tc_user", "ssh_user"]),
            getpass=Mock(side_effect=["tc_pass", "ssh_pass"]),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="atlas_user",
                atlassian_password="atlas_pass",
                git_username="git_user",
                git_pat="git_token",
            )

            assert context.build_id == "12345"
            assert context.atlassian is not None
            assert context.teamcity is not None
            assert context.ssh_client is not None
            assert context.git_client is not None

    def test_init_with_require_flags_disabled(self) -> None:
        """Test initialization with some services disabled."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_atlassian=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )

        assert context.build_id == "12345"
        assert context.atlassian is None
        assert context.teamcity is None
        assert context.ssh_client is None
        assert context.git_client is None

    def test_init_raises_error_for_missing_required_credentials(self) -> None:
        """Test initialization raises error when required credentials are missing."""
        # Mock input to return empty strings (simulating user not providing credentials)
        with (
            patch.multiple(
                "ci_tools.dimrset_delivery.dimr_context",
                input=Mock(return_value=""),
                getpass=Mock(return_value=""),
            ),
            pytest.raises(ValueError, match="Atlassian credentials are required but not provided"),
        ):
            self._create_context(
                build_id="12345",
                dry_run=False,
                require_atlassian=True,
                require_teamcity=False,
                require_ssh=False,
                require_git=False,
            )

    @patch("builtins.print")
    def test_log_dry_run_mode(self, mock_print: Mock) -> None:
        """Test log method in dry run mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            context.log("Test message")
            mock_print.assert_called_with(f"{context.settings.dry_run_prefix} Test message")

    @patch("builtins.print")
    def test_log_normal_mode(self, mock_print: Mock) -> None:
        """Test log method in normal mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            context.log("Test message")
            mock_print.assert_called_with("Test message")

    def test_get_kernel_versions_dry_run_mode(self) -> None:
        """Test get_kernel_versions method in dry run mode."""
        with (
            patch.multiple(
                "ci_tools.dimrset_delivery.dimr_context",
                Atlassian=Mock(spec=Atlassian),
                TeamCity=Mock(spec=TeamCity),
                SshClient=Mock(spec=SshClient),
                GitClient=Mock(spec=GitClient),
            ),
            patch("builtins.print"),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            versions = context.get_kernel_versions()

            # Should return mock data in dry run mode
            assert "DIMRset_ver" in versions
            assert "build.vcs.number" in versions
            assert versions["DIMRset_ver"] == "1.23.45"
            assert versions["build.vcs.number"] == "abcdefghijklmnopqrstuvwxyz01234567890123"

    def test_get_kernel_versions_normal_mode(self) -> None:
        """Test get_kernel_versions method in normal mode."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                    {"name": "build.vcs.number", "value": "abc123def456"},
                    {"name": "other_property", "value": "other_value"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            versions = context.get_kernel_versions()

            assert versions["DIMRset_ver"] == "5.10.00.12345"
            assert versions["build.vcs.number"] == "abc123def456"
            mock_teamcity.get_build_info_for_build_id.assert_called_once_with("12345")

    def test_get_kernel_versions_caching(self) -> None:
        """Test that get_kernel_versions caches results."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                    {"name": "build.vcs.number", "value": "abc123def456"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            # Call twice
            versions1 = context.get_kernel_versions()
            versions2 = context.get_kernel_versions()

            # Should be called only once due to caching
            mock_teamcity.get_build_info_for_build_id.assert_called_once_with("12345")
            assert versions1 == versions2

    def test_get_kernel_versions_no_teamcity_client(self) -> None:
        """Test get_kernel_versions raises error when TeamCity client is not initialized."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_atlassian=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )

        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            context.get_kernel_versions()

    def test_get_kernel_versions_no_build_info(self) -> None:
        """Test get_kernel_versions raises error when build info cannot be retrieved."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_teamcity.get_build_info_for_build_id.return_value = None

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            with pytest.raises(ValueError, match="Could not retrieve build info from TeamCity"):
                context.get_kernel_versions()

    def test_get_dimr_version(self) -> None:
        """Test get_dimr_version method."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                    {"name": "build.vcs.number", "value": "abc123def456"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            dimr_version = context.get_dimr_version()
            assert dimr_version == "5.10.00.12345"

    def test_get_dimr_version_no_kernel_versions(self) -> None:
        """Test get_dimr_version raises error when kernel versions are not available."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_atlassian=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )

        # Directly test the assertion by setting _kernel_versions to None
        context._kernel_versions = None

        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            context.get_dimr_version()

    def test_get_dimr_version_assertion_error(self) -> None:
        """Test get_dimr_version raises AssertionError when kernel versions extraction fails."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_atlassian=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )

        # Mock get_kernel_versions to return None to trigger the AssertionError
        with patch.object(context, "get_kernel_versions", return_value=None):
            with pytest.raises(AssertionError, match="Could not extract the DIMR version"):
                context.get_dimr_version()

    def test_get_branch_name_dry_run_mode(self) -> None:
        """Test get_branch_name method in dry run mode."""
        with (
            patch.multiple(
                "ci_tools.dimrset_delivery.dimr_context",
                Atlassian=Mock(spec=Atlassian),
                TeamCity=Mock(spec=TeamCity),
                SshClient=Mock(spec=SshClient),
                GitClient=Mock(spec=GitClient),
            ),
            patch("builtins.print"),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            branch_name = context.get_branch_name()
            assert branch_name == "main"

    def test_get_branch_name_normal_mode(self) -> None:
        """Test get_branch_name method in normal mode."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "teamcity.build.branch", "value": "feature/test-branch"},
                    {"name": "other_property", "value": "other_value"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            branch_name = context.get_branch_name()
            assert branch_name == "feature/test-branch"

    def test_get_branch_name_no_teamcity_client(self) -> None:
        """Test get_branch_name raises error when TeamCity client is not initialized."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_atlassian=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )

        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            context.get_branch_name()

    def test_get_branch_name_no_build_info(self) -> None:
        """Test get_branch_name raises error when build info cannot be retrieved."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_teamcity.get_build_info_for_build_id.return_value = None

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            with pytest.raises(ValueError, match="Could not retrieve build info from TeamCity"):
                context.get_branch_name()

    def test_get_branch_name_no_branch_property(self) -> None:
        """Test get_branch_name raises error when branch property is not found."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "other_property", "value": "other_value"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            with pytest.raises(ValueError, match="Could not find branch name in build properties"):
                context.get_branch_name()

    def test_get_branch_name_caching(self) -> None:
        """Test that get_branch_name caches results."""
        mock_teamcity = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "teamcity.build.branch", "value": "feature/test-branch"},
                ]
            }
        }
        mock_teamcity.get_build_info_for_build_id.return_value = mock_build_info

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(return_value=mock_teamcity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            # Call twice
            branch1 = context.get_branch_name()
            branch2 = context.get_branch_name()

            # Should be called only once due to caching
            mock_teamcity.get_build_info_for_build_id.assert_called_once_with("12345")
            assert branch1 == branch2 == "feature/test-branch"

    def test_extract_kernel_versions(self) -> None:
        """Test _extract_kernel_versions method."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                atlassian_username="user",
                atlassian_password="pass",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_pat="token",
            )

            build_info = {
                "resultingProperties": {
                    "property": [
                        {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                        {"name": "build.vcs.number", "value": "abc123def456"},
                        {"name": "other_property", "value": "other_value"},
                    ]
                }
            }

            versions = context._extract_kernel_versions(build_info)

            assert versions["DIMRset_ver"] == "5.10.00.12345"
            assert versions["build.vcs.number"] == "abc123def456"


class TestParseCommonArguments:
    """Test cases for parse_common_arguments function."""

    def test_parse_common_arguments_required_only(self) -> None:
        """Test parsing with only required arguments."""
        test_args = ["--build_id", "12345"]

        with patch("sys.argv", ["script_name"] + test_args):
            args = parse_common_arguments()

            assert args.build_id == "12345"
            assert args.dry_run is False
            assert args.atlassian_username is None
            assert args.atlassian_password is None
            assert args.teamcity_username is None
            assert args.teamcity_password is None
            assert args.ssh_username is None
            assert args.ssh_password is None
            assert args.git_username is None
            assert getattr(args, "git_PAT", None) is None

    def test_parse_common_arguments_all_optional(self) -> None:
        """Test parsing with all optional arguments."""
        test_args = [
            "--build_id",
            "12345",
            "--dry-run",
            "--atlassian-username",
            "atlas_user",
            "--atlassian-password",
            "atlas_pass",
            "--teamcity-username",
            "tc_user",
            "--teamcity-password",
            "tc_pass",
            "--ssh-username",
            "ssh_user",
            "--ssh-password",
            "ssh_pass",
            "--git-username",
            "git_user",
            "--git-PAT",
            "git_token",
        ]

        with patch("sys.argv", ["script_name"] + test_args):
            args = parse_common_arguments()

            assert args.build_id == "12345"
            assert args.dry_run is True
            assert args.atlassian_username == "atlas_user"
            assert args.atlassian_password == "atlas_pass"
            assert args.teamcity_username == "tc_user"
            assert args.teamcity_password == "tc_pass"
            assert args.ssh_username == "ssh_user"
            assert args.ssh_password == "ssh_pass"
            assert args.git_username == "git_user"
            assert getattr(args, "git_PAT", None) == "git_token"

    def test_parse_common_arguments_missing_required(self) -> None:
        """Test parsing fails when required arguments are missing."""
        test_args = ["--dry-run"]

        with patch("sys.argv", ["script_name"] + test_args):
            with pytest.raises(SystemExit):
                parse_common_arguments()


class TestCreateContextFromArgs:
    """Test cases for create_context_from_args function."""

    def test_create_context_from_args_all_services_required(self) -> None:
        """Test creating context with all services required."""
        args = argparse.Namespace(
            build_id="12345",
            dry_run=False,
            atlassian_username="atlas_user",
            atlassian_password="atlas_pass",
            teamcity_username="tc_user",
            teamcity_password="tc_pass",
            ssh_username="ssh_user",
            ssh_password="ssh_pass",
            git_username="git_user",
        )
        args.git_PAT = "git_token"

        with patch.multiple(
            "ci_tools.dimrset_delivery.dimr_context",
            Atlassian=Mock(spec=Atlassian),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = create_context_from_args(args)

            assert context.build_id == "12345"
            assert context.dry_run is False
            assert context.atlassian is not None
            assert context.teamcity is not None
            assert context.ssh_client is not None
            assert context.git_client is not None

    def test_create_context_from_args_some_services_disabled(self) -> None:
        """Test creating context with some services disabled."""
        args = argparse.Namespace(
            build_id="12345",
            dry_run=True,
            atlassian_username=None,
            atlassian_password=None,
            teamcity_username=None,
            teamcity_password=None,
            ssh_username=None,
            ssh_password=None,
            git_username=None,
        )
        args.git_PAT = None

        context = create_context_from_args(
            args,
            require_atlassian=False,
            require_git=False,
            require_teamcity=False,
            require_ssh=False,
        )

        assert context.build_id == "12345"
        assert context.dry_run is True
        assert context.atlassian is None
        assert context.teamcity is None
        assert context.ssh_client is None
        assert context.git_client is None

    def test_create_context_from_args_missing_git_pat_attribute(self) -> None:
        """Test creating context when git_PAT attribute is missing from args."""
        args = argparse.Namespace(
            build_id="12345",
            dry_run=False,
            atlassian_username=None,
            atlassian_password=None,
            teamcity_username=None,
            teamcity_password=None,
            ssh_username=None,
            ssh_password=None,
            git_username=None,
        )
        # Don't set git_PAT attribute

        context = create_context_from_args(
            args,
            require_atlassian=False,
            require_git=False,
            require_teamcity=False,
            require_ssh=False,
        )

        assert context.build_id == "12345"
        assert context.dry_run is False
