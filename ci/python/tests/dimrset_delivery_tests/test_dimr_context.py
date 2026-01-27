"""Tests for dimr_context.py."""

import argparse
from typing import Optional
from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import (
    CredentialEntry,
    Credentials,
    DimrAutomationContext,
    ServiceAuthenticateStore,
    ServiceName,
)
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.jira import Jira
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services


class TestDimrAutomationContext:
    """Test cases for DimrAutomationContext class."""

    def _create_context(
        self,
        build_id: str = "12345",
        dry_run: bool = False,
        jira_username: str = "",
        jira_password: str = "",
        teamcity_username: str = "",
        teamcity_password: str = "",
        ssh_username: str = "",
        ssh_password: str = "",
        git_username: str = "",
        git_password: str = "",
        require_jira: bool = True,
        require_teamcity: bool = True,
        require_ssh: bool = True,
        require_git: bool = True,
        credentials: Optional[ServiceAuthenticateStore] = None,
    ) -> DimrAutomationContext:
        """Create a DimrAutomationContext with specified parameters."""
        # If credentials are provided directly, use them
        if credentials is not None:
            return DimrAutomationContext(build_id=build_id, dry_run=dry_run, credentials=credentials)

        # Otherwise, create them from the individual parameters
        if credentials is None:
            credentials = ServiceAuthenticateStore()
            credentials.add(
                ServiceName.JIRA,
                CredentialEntry(
                    required=require_jira,
                    credential=Credentials(username=jira_username, password=jira_password),
                ),
            )
            credentials.add(
                ServiceName.TEAMCITY,
                CredentialEntry(
                    required=require_teamcity,
                    credential=Credentials(username=teamcity_username, password=teamcity_password),
                ),
            )
            credentials.add(
                ServiceName.SSH,
                CredentialEntry(
                    required=require_ssh,
                    credential=Credentials(username=ssh_username, password=ssh_password),
                ),
            )
            credentials.add(
                ServiceName.GIT,
                CredentialEntry(
                    required=require_git,
                    credential=Credentials(username=git_username, password=git_password),
                ),
            )

        return DimrAutomationContext(build_id=build_id, dry_run=dry_run, credentials=credentials)

    def test_init_with_all_credentials_provided(self) -> None:
        """Test initialization when all credentials are provided."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            Jira=Mock(spec=Jira),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                jira_username="jira_user",
                jira_password="jira_token",
                teamcity_username="tc_user",
                teamcity_password="tc_pass",
                ssh_username="ssh_user",
                ssh_password="ssh_pass",
                git_username="git_user",
                git_password="git_token",
            )
            services = Services(context)

            assert context.build_id == "12345"
            assert context.dry_run is False
            assert services.jira is not None
            assert services.teamcity is not None
            assert services.ssh is not None
            assert services.git is not None

    def test_init_dry_run_mode(self) -> None:
        """Test initialization in dry run mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            Jira=Mock(spec=Jira),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                jira_username="jira_user",
                jira_password="jira_token",
                teamcity_username="tc_user",
                teamcity_password="tc_pass",
                ssh_username="ssh_user",
                ssh_password="ssh_pass",
                git_username="git_user",
                git_password="git_token",
            )

            assert context.dry_run is True

    def test_init_with_missing_credentials_prompts_input(self) -> None:
        """Test initialization prompts for missing credentials."""
        with (
            patch("ci_tools.dimrset_delivery.services.TeamCity") as teamcity_patch,
            patch.multiple(
                "ci_tools.dimrset_delivery.services",
                Jira=Mock(spec=Jira),
                SshClient=Mock(spec=SshClient),
                GitClient=Mock(spec=GitClient),
            ),
            patch.multiple(
                "ci_tools.dimrset_delivery.dimr_context",
                input=Mock(side_effect=["jira_user", "tc_user", "ssh_user", "git_user"]),
                getpass=Mock(side_effect=["jira_token", "tc_pass", "ssh_pass", "git_token"]),
            ),
        ):
            teamcity_mock = Mock(spec=TeamCity)
            teamcity_mock.get_build_info_for_build_id.return_value = {
                "resultingProperties": {
                    "property": [
                        {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                        {"name": "build.vcs.number", "value": "abc123def456"},
                    ]
                }
            }
            teamcity_patch.return_value = teamcity_mock

            context = self._create_context(
                build_id="12345",
                dry_run=False,
            )
            services = Services(context)

            assert context.build_id == "12345"
            assert services.jira is not None
            assert services.teamcity is not None
            assert services.ssh is not None
            assert services.git is not None

    def test_init_with_partial_credentials_prompts_missing(self) -> None:
        """Test initialization prompts only for missing credentials."""
        with (
            patch.multiple(
                "ci_tools.dimrset_delivery.services",
                TeamCity=Mock(spec=TeamCity),
                Jira=Mock(spec=Jira),
                SshClient=Mock(spec=SshClient),
                GitClient=Mock(spec=GitClient),
            ),
            patch.multiple(
                "ci_tools.dimrset_delivery.dimr_context",
                input=Mock(side_effect=["tc_user", "ssh_user"]),
                getpass=Mock(side_effect=["tc_pass", "ssh_pass"]),
            ),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                jira_username="jira_user",
                jira_password="jira_token",
                git_username="git_user",
                git_password="git_token",
            )
            services = Services(context)

            assert context.build_id == "12345"
            assert services.jira is not None
            assert services.teamcity is not None
            assert services.ssh is not None
            assert services.git is not None

    def test_init_with_require_flags_disabled(self) -> None:
        """Test initialization with some services disabled."""
        context = self._create_context(
            build_id="12345",
            dry_run=False,
            require_jira=False,
            require_teamcity=False,
            require_ssh=False,
            require_git=False,
        )
        services = Services(context)
        assert context.build_id == "12345"
        assert services.jira is None
        assert services.teamcity is None
        assert services.ssh is None
        assert services.git is None

    @patch("builtins.print")
    def test_log_dry_run_mode(self, mock_print: Mock) -> None:
        """Test log method in dry run mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            Jira=Mock(spec=Jira),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                jira_username="user",
                jira_password="token",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_password="token",
            )

            context.log("Test message")
            mock_print.assert_called_with(f"{context.settings.dry_run_prefix} Test message")

    @patch("builtins.print")
    def test_log_normal_mode(self, mock_print: Mock) -> None:
        """Test log method in normal mode."""
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            Jira=Mock(spec=Jira),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                jira_username="user",
                jira_password="token",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_password="token",
            )

            context.log("Test message")
            mock_print.assert_called_with("Test message")

    def test_get_kernel_versions_dry_run_mode(self) -> None:
        """Test get_kernel_versions method in dry run mode."""
        # Only patch the service classes. In dry run mode, TeamCity.get_kernel_versions returns mock data directly.
        teamcity_mock = Mock(spec=TeamCity)
        teamcity_mock.get_kernel_versions.return_value = {
            "DIMRset_ver": "1.23.45",
            "build.vcs.number": "abcdefghijklmnopqrstuvwxyz01234567890123",
        }
        teamcity_mock.get_dimr_version.return_value = "1.23.45"
        teamcity_mock.get_branch_name.return_value = "mock_branch"

        with (
            patch("ci_tools.dimrset_delivery.services.TeamCity", return_value=teamcity_mock),
            patch("ci_tools.dimrset_delivery.services.Jira", Mock(spec=Jira)),
            patch("ci_tools.dimrset_delivery.services.SshClient", Mock(spec=SshClient)),
            patch("ci_tools.dimrset_delivery.services.GitClient", Mock(spec=GitClient)),
            patch("builtins.print"),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=True,
                jira_username="user",
                jira_password="token",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_password="token",
            )
            Services(context)

            # Should return mock data in dry run mode
            assert "DIMRset_ver" in context.kernel_versions
            assert "build.vcs.number" in context.kernel_versions
            assert context.kernel_versions["DIMRset_ver"] == "1.23.45"
            assert context.kernel_versions["build.vcs.number"] == "abcdefghijklmnopqrstuvwxyz01234567890123"
            assert context.branch_name == "mock_branch"

    def test_get_kernel_versions_normal_mode(self) -> None:
        """Test get_kernel_versions method in normal mode."""
        teamcity_mock = Mock(spec=TeamCity)
        mock_build_info = {
            "resultingProperties": {
                "property": [
                    {"name": "DIMRset_ver", "value": "5.10.00.12345"},
                    {"name": "build.vcs.number", "value": "abc123def456"},
                ]
            }
        }
        teamcity_mock.get_build_info_for_build_id.return_value = mock_build_info
        teamcity_mock.get_dimr_version.return_value = "1.23.45"
        teamcity_mock.get_kernel_versions.return_value = {
            "DIMRset_ver": "1.23.45",
            "build.vcs.number": "abcdefghijklmnopqrstuvwxyz01234567890123",
        }

        with (
            patch("ci_tools.dimrset_delivery.services.TeamCity", return_value=teamcity_mock),
            patch("ci_tools.dimrset_delivery.services.Jira", Mock(spec=Jira)),
            patch("ci_tools.dimrset_delivery.services.SshClient", Mock(spec=SshClient)),
            patch("ci_tools.dimrset_delivery.services.GitClient", Mock(spec=GitClient)),
            patch("builtins.print"),
        ):
            context = self._create_context(
                build_id="12345",
                dry_run=False,
                jira_username="user",
                jira_password="token",
                teamcity_username="user",
                teamcity_password="pass",
                ssh_username="user",
                ssh_password="pass",
                git_username="user",
                git_password="token",
            )
            Services(context)

            assert context.kernel_versions["DIMRset_ver"] == "1.23.45"
            assert context.kernel_versions["build.vcs.number"] == "abcdefghijklmnopqrstuvwxyz01234567890123"


class TestParseCommonArguments:
    """Test cases for parse_common_arguments function."""

    def test_parse_common_arguments_required_only(self) -> None:
        """Test parsing with only required arguments."""
        test_args = ["--build_id", "12345"]

        with patch("sys.argv", ["script_name"] + test_args):
            args = parse_common_arguments()

            assert args.build_id == "12345"
            assert args.dry_run is False
            assert args.jira_username is None
            assert args.jira_PAT is None
            assert args.teamcity_username is None
            assert args.teamcity_password is None
            assert args.ssh_username is None
            assert args.ssh_password is None
            assert args.git_username is None
            assert getattr(args, "git_password", None) is None

    def test_parse_common_arguments_all_optional(self) -> None:
        """Test parsing with all optional arguments."""
        test_args = [
            "--build_id",
            "12345",
            "--dry-run",
            "--jira-username",
            "jira_user",
            "--jira-PAT",
            "jira_token",
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
            assert args.jira_username == "jira_user"
            assert args.jira_PAT == "jira_token"
            assert args.teamcity_username == "tc_user"
            assert args.teamcity_password == "tc_pass"
            assert args.ssh_username == "ssh_user"
            assert args.ssh_password == "ssh_pass"
            assert args.git_username == "git_user"
            assert args.git_PAT == "git_token"

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
        # Arrange
        args = argparse.Namespace(
            build_id="12345",
            dry_run=False,
            jira_username="jira_user",
            jira_PAT="jira_token",
            teamcity_username="tc_user",
            teamcity_password="tc_pass",
            ssh_username="ssh_user",
            ssh_password="ssh_pass",
            git_username="git_user",
            git_PAT="git_token",
        )
        # Act
        with patch.multiple(
            "ci_tools.dimrset_delivery.services",
            Jira=Mock(spec=Jira),
            TeamCity=Mock(spec=TeamCity),
            SshClient=Mock(spec=SshClient),
            GitClient=Mock(spec=GitClient),
        ):
            context = create_context_from_args(args)
            services = Services(context)

            # Assert
            assert context.build_id == "12345"
            assert context.dry_run is False
            assert services.jira is not None
            assert services.teamcity is not None
            assert services.ssh is not None
            assert services.git is not None

    def test_create_context_from_args_some_services_disabled(self) -> None:
        """Test creating context with some services disabled."""
        args = argparse.Namespace(
            build_id="12345",
            dry_run=True,
            jira_username=None,
            jira_PAT=None,
            teamcity_username=None,
            teamcity_password=None,
            ssh_username=None,
            ssh_password=None,
            git_username=None,
            git_PAT=None,
        )

        context = create_context_from_args(
            args,
            require_jira=False,
            require_git=False,
            require_teamcity=False,
            require_ssh=False,
        )
        services = Services(context)

        assert context.build_id == "12345"
        assert context.dry_run is True
        assert services.jira is None
        assert services.teamcity is None
        assert services.ssh is None
        assert services.git is None

    def test_create_context_from_args_missing_git_password_attribute(self) -> None:
        """Test creating context when git_password attribute is missing from args."""
        args = argparse.Namespace(
            build_id="12345",
            dry_run=False,
            jira_username=None,
            jira_PAT=None,
            teamcity_username=None,
            teamcity_password=None,
            ssh_username=None,
            ssh_password=None,
            git_username=None,
        )
        # Act & Assert
        with pytest.raises(AttributeError):
            create_context_from_args(
                args,
                require_jira=False,
                require_git=False,
                require_teamcity=False,
                require_ssh=False,
            )
