import argparse
import json
import os
from dataclasses import dataclass
from getpass import getpass
from typing import Any, Dict, Optional

from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.teamcity_settings import (
    KERNELS,
    Settings,
)


@dataclass
class DimrCredentials:
    """Dataclass for storing credentials for DIMR automation."""

    atlassian_username: Optional[str] = None
    atlassian_password: Optional[str] = None
    teamcity_username: Optional[str] = None
    teamcity_password: Optional[str] = None
    ssh_username: Optional[str] = None
    ssh_password: Optional[str] = None
    git_username: Optional[str] = None
    git_pat: Optional[str] = None


@dataclass
class ServiceRequirements:
    """Dataclass for specifying which services are required."""

    atlassian: bool = True
    teamcity: bool = True
    ssh: bool = True
    git: bool = True


class DimrAutomationContext:
    """Shared context for DIMR automation steps."""

    def __init__(
        self,
        build_id: str,
        dry_run: bool = False,
        credentials: Optional[DimrCredentials] = None,
        requirements: Optional[ServiceRequirements] = None,
    ) -> None:
        """Initialize DIMR Automation Context.

        Parameters
        ----------
        build_id : str
            The TeamCity build ID
        dry_run : bool, optional
            Whether to run in dry-run mode, by default False
        credentials : Optional[DimrCredentials], optional
            Credentials for various services, by default None
        requirements : Optional[ServiceRequirements], optional
            Requirements for various services, by default None
        """
        self.build_id = build_id
        self.dry_run = dry_run

        # Initialize credentials if not provided
        if credentials is None:
            credentials = DimrCredentials()

        # Initialize requirements if not provided
        if requirements is None:
            requirements = ServiceRequirements()

        # Prompt for any missing required credentials
        self._prompt_for_credentials(credentials, requirements)

        settings_path = os.path.join(os.path.dirname(__file__), "settings", "teamcity_settings.json")
        self.settings = Settings(settings_path)

        self._initialize_services(credentials, requirements, self.settings)

        # Cache for commonly needed data
        self._kernel_versions: Optional[Dict[str, str]] = None
        self._dimr_version: Optional[str] = None
        self._branch_name: Optional[str] = None

    def log(self, *args, **kwargs) -> None:
        """Print status message with dry-run prefix if applicable.

        Parameters
        ----------
        *args
            Positional arguments to pass to print.
        **kwargs
            Keyword arguments to pass to print.
        """
        if self.dry_run:
            # If sep is specified in kwargs, use it; otherwise default to ' '
            sep = kwargs.get("sep", " ")
            # Join all args with the separator and add dry-run prefix
            message = sep.join(str(arg) for arg in args)
            print(f"{self.settings.dry_run_prefix} {message}", **{k: v for k, v in kwargs.items() if k != "sep"})
        else:
            print(*args, **kwargs)

    def get_kernel_versions(self) -> Dict[str, str]:
        """Get kernel versions (cached).

        Returns
        -------
        Dict[str, str]
            Dictionary mapping kernel names to their versions.
        """
        if self._kernel_versions is None:
            if self.dry_run:
                self.log(f"Get build info of build_id {self.build_id}, then extract kernel versions from properties.")
                self._kernel_versions = {
                    KERNELS[0].name_for_extracting_revision: "1.23.45",
                    KERNELS[1].name_for_extracting_revision: "abcdefghijklmnopqrstuvwxyz01234567890123",
                }
            else:
                if self.teamcity is None:
                    raise ValueError("TeamCity client is required but not initialized")
                publish_build_info = self.teamcity.get_build_info_for_build_id(self.build_id)
                if publish_build_info is None:
                    raise ValueError("Could not retrieve build info from TeamCity")
                self._kernel_versions = self._extract_kernel_versions(publish_build_info)
        return self._kernel_versions

    def get_dimr_version(self) -> str:
        """Get DIMR version (cached).

        Returns
        -------
        str
            The DIMR version string.

        Raises
        ------
        AssertionError
            If kernel versions have not been extracted.
        """
        if self._dimr_version is None:
            kernel_versions = self.get_kernel_versions()
            if kernel_versions is None:
                raise AssertionError(
                    "Could not extract the DIMR version: the kernel versions have not yet been extracted"
                )
            self._dimr_version = kernel_versions["DIMRset_ver"]

        if self._dimr_version is None:
            raise AssertionError("DIMR version is unexpectedly None after extraction")

        return self._dimr_version

    def get_branch_name(self) -> str:
        """Get branch name (cached).

        Returns
        -------
        str
            The branch name.

        Raises
        ------
        ValueError
            If branch name could not be retrieved.
        """
        if self._branch_name is None:
            if self.dry_run:
                self.log(f"Get build info of build_id {self.build_id}, then get branch name from properties.")
                self._branch_name = "main"
                self.log(f"simulating '{self._branch_name}' branch")
            else:
                if self.teamcity is None:
                    raise ValueError("TeamCity client is required but not initialized")
                latest_publish_build_info = self.teamcity.get_build_info_for_build_id(self.build_id)
                if latest_publish_build_info is None:
                    raise ValueError("Could not retrieve build info from TeamCity")
                branch_name_property = next(
                    (
                        prop
                        for prop in latest_publish_build_info["resultingProperties"]["property"]
                        if prop["name"] == "teamcity.build.branch"
                    ),
                    None,
                )
                if branch_name_property is None:
                    raise ValueError("Could not find branch name in build properties")
                self._branch_name = branch_name_property["value"]

        if self._branch_name is None:
            raise ValueError("Branch name is unexpectedly None after retrieval")

        return self._branch_name

    def _prompt_for_credentials(self, credentials: DimrCredentials, requirements: ServiceRequirements) -> None:
        """Prompt for any missing required credentials.

        Parameters
        ----------
        credentials : DimrCredentials
            The credentials object to update
        requirements : ServiceRequirements
            The service requirements
        """
        if requirements.atlassian and (not credentials.atlassian_username or not credentials.atlassian_password):
            print("Atlassian/Confluence credentials:")
            credentials.atlassian_username = credentials.atlassian_username or input("Enter your Atlassian username:")
            credentials.atlassian_password = credentials.atlassian_password or getpass(
                prompt="Enter your Atlassian password:", stream=None
            )

        if requirements.teamcity and (not credentials.teamcity_username or not credentials.teamcity_password):
            print("TeamCity credentials:")
            credentials.teamcity_username = credentials.teamcity_username or input("Enter your TeamCity username:")
            credentials.teamcity_password = credentials.teamcity_password or getpass(
                prompt="Enter your TeamCity password:", stream=None
            )

        if requirements.ssh and (not credentials.ssh_username or not credentials.ssh_password):
            print("SSH (H7) credentials:")
            credentials.ssh_username = credentials.ssh_username or input("Enter your SSH username:")
            credentials.ssh_password = credentials.ssh_password or getpass(
                prompt="Enter your SSH password:", stream=None
            )

        if requirements.git and (not credentials.git_username or not credentials.git_pat):
            print("Git credentials:")
            credentials.git_username = credentials.git_username or input("Enter your Git username:")
            credentials.git_pat = credentials.git_pat or getpass(prompt="Enter your Git PAT:", stream=None)

    def _initialize_services(
        self, credentials: DimrCredentials, requirements: ServiceRequirements, settings: Settings
    ) -> None:
        """Initialize services based on requirements.

        Parameters
        ----------
        credentials : DimrCredentials
            The credentials to use for initialization
        requirements : ServiceRequirements
            The service requirements
        """
        self.atlassian = None
        if requirements.atlassian:
            if not credentials.atlassian_username or not credentials.atlassian_password:
                raise ValueError("Atlassian credentials are required but not provided")
            self.atlassian = Atlassian(
                username=credentials.atlassian_username,
                password=credentials.atlassian_password,
                settings=settings,
            )

        self.teamcity = None
        if requirements.teamcity:
            if not credentials.teamcity_username or not credentials.teamcity_password:
                raise ValueError("TeamCity credentials are required but not provided")
            self.teamcity = TeamCity(
                username=credentials.teamcity_username,
                password=credentials.teamcity_password,
                settings=settings,
            )

        self.ssh_client = None
        if requirements.ssh:
            if not credentials.ssh_username or not credentials.ssh_password:
                raise ValueError("SSH credentials are required but not provided")
            self.ssh_client = SshClient(
                username=credentials.ssh_username,
                password=credentials.ssh_password,
                settings=settings,
            )

        self.git_client = None
        if requirements.git:
            if not credentials.git_username or not credentials.git_pat:
                raise ValueError("Git credentials are required but not provided")
            self.git_client = GitClient(
                username=credentials.git_username,
                password=credentials.git_pat,
                settings=settings,
            )

    def _extract_kernel_versions(self, build_info: Dict[str, Any]) -> Dict[str, str]:
        """Extract kernel versions from build info."""
        kernel_versions: Dict[str, str] = {}
        for kernel in KERNELS:
            kernel_versions[kernel.name_for_extracting_revision] = ""

        for kernel_prop in build_info["resultingProperties"]["property"]:
            if any(k.name_for_extracting_revision == kernel_prop["name"] for k in KERNELS):
                kernel_versions[kernel_prop["name"]] = kernel_prop["value"]

        return kernel_versions


def parse_common_arguments() -> argparse.Namespace:
    """Parse common command line arguments for DIMR automation scripts."""
    parser = argparse.ArgumentParser(description="DIMR Automation Script")

    parser.add_argument("--build_id", type=str, required=True, help="Build ID chain for the DIMR release")
    parser.add_argument(
        "--dry-run", action="store_true", default=False, help="Run in dry-run mode without making any changes"
    )

    parser.add_argument("--atlassian-username", type=str, default=None, help="Atlassian/Confluence username")
    parser.add_argument("--atlassian-password", type=str, default=None, help="Atlassian/Confluence password")

    parser.add_argument("--teamcity-username", type=str, default=None, help="TeamCity username")
    parser.add_argument("--teamcity-password", type=str, default=None, help="TeamCity password")

    parser.add_argument("--ssh-username", type=str, default=None, help="SSH username for H7 server")
    parser.add_argument("--ssh-password", type=str, default=None, help="SSH password for H7 server")

    parser.add_argument("--git-username", type=str, default=None, help="Git username")
    parser.add_argument("--git-PAT", type=str, default=None, help="Git Personal Access Token")

    return parser.parse_args()


def create_context_from_args(
    args: argparse.Namespace,
    require_atlassian: bool = True,
    require_git: bool = True,
    require_teamcity: bool = True,
    require_ssh: bool = True,
) -> DimrAutomationContext:
    """Create automation context from parsed arguments."""
    credentials = DimrCredentials(
        atlassian_username=args.atlassian_username,
        atlassian_password=args.atlassian_password,
        teamcity_username=args.teamcity_username,
        teamcity_password=args.teamcity_password,
        ssh_username=args.ssh_username,
        ssh_password=args.ssh_password,
        git_username=args.git_username,
        git_pat=getattr(args, "git_PAT", None),
    )

    requirements = ServiceRequirements(
        atlassian=require_atlassian, teamcity=require_teamcity, ssh=require_ssh, git=require_git
    )

    return DimrAutomationContext(
        build_id=args.build_id, dry_run=args.dry_run, credentials=credentials, requirements=requirements
    )
