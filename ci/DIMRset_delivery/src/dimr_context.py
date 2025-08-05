"""
Shared context for DIMR automation scripts.
Handles initialization and provides common functionality.
"""

import argparse
from getpass import getpass
from typing import Dict, Optional

from helpers.GitClient import GitClient
from helpers.SshClient import SshClient
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import DELFT3D_GIT_REPO, DRY_RUN_PREFIX
from settings.teamcity_settings import KERNELS


class DimrAutomationContext:
    """Shared context for DIMR automation steps."""
    
    def __init__(self, build_id: str, dry_run: bool = False, 
                 atlassian_username: Optional[str] = None, atlassian_password: Optional[str] = None,
                 teamcity_username: Optional[str] = None, teamcity_password: Optional[str] = None,
                 ssh_username: Optional[str] = None, ssh_password: Optional[str] = None,
                 git_username: Optional[str] = None, git_pat: Optional[str] = None,
                 require_atlassian: bool = True, require_git: bool = True,
                 require_teamcity: bool = True, require_ssh: bool = True):
        self.build_id = build_id
        self.dry_run = dry_run
        
        # Get Atlassian credentials only if required
        if require_atlassian and (not atlassian_username or not atlassian_password):
            print("Atlassian/Confluence credentials:")
            atlassian_username = atlassian_username or input("Enter your Atlassian username:")
            atlassian_password = atlassian_password or getpass(prompt="Enter your Atlassian password:", stream=None)
        
        # Get TeamCity credentials
        if require_teamcity and (not teamcity_username or not teamcity_password):
            print("TeamCity credentials:")
            teamcity_username = teamcity_username or input("Enter your TeamCity username:")
            teamcity_password = teamcity_password or getpass(prompt="Enter your TeamCity password:", stream=None)
        
        # Get SSH credentials
        if require_ssh and (not ssh_username or not ssh_password):
            print("SSH (H7) credentials:")
            ssh_username = ssh_username or input("Enter your SSH username:")
            ssh_password = ssh_password or getpass(prompt="Enter your SSH password:", stream=None)
        
        # Get Git credentials
        if require_git and (not git_username or not git_pat):
            print("Git credentials:")
            git_username = git_username or input("Enter your Git username:")
            git_pat = git_pat or getpass(prompt="Enter your Git PAT:", stream=None)
        
        # Initialize clients
        self.atlassian = Atlassian(username=atlassian_username, password=atlassian_password) if require_atlassian else None
        self.teamcity = TeamCity(username=teamcity_username, password=teamcity_password) if require_teamcity else None
        self.ssh_client = SshClient(username=ssh_username, password=ssh_password, connect_timeout=30) if require_ssh else None
        self.git_client = GitClient(DELFT3D_GIT_REPO, git_username, git_pat) if require_git else None
        
        # Cache for commonly needed data
        self._kernel_versions = None
        self._dimr_version = None
        self._branch_name = None
    
    def print_status(self, message: str) -> None:
        """Print status message with dry-run prefix if applicable."""
        if self.dry_run:
            print(f"{DRY_RUN_PREFIX} {message}")
        else:
            print(message)
    
    def get_kernel_versions(self) -> Dict[str, str]:
        """Get kernel versions (cached)."""
        if self._kernel_versions is None:
            if self.dry_run:
                self.print_status(f"Get build info of build_id {self.build_id}, then extract kernel versions from properties.")
                self._kernel_versions = {
                    KERNELS[0].name_for_extracting_revision: "1.23.45",
                    KERNELS[1].name_for_extracting_revision: "abcdefghijklmnopqrstuvwxyz01234567890123"
                }
            else:
                publish_build_info = self.teamcity.get_build_info_for_build_id(self.build_id)
                self._kernel_versions = self._extract_kernel_versions(publish_build_info)
        return self._kernel_versions
    
    def get_dimr_version(self) -> str:
        """Get DIMR version (cached)."""
        if self._dimr_version is None:
            kernel_versions = self.get_kernel_versions()
            if kernel_versions is None:
                raise AssertionError("Could not extract the DIMR version: the kernel versions have not yet been extracted")
            self._dimr_version = kernel_versions["DIMRset_ver"]
        return self._dimr_version
    
    def get_branch_name(self) -> str:
        """Get branch name (cached)."""
        if self._branch_name is None:
            if self.dry_run:
                self.print_status(f"Get build info of build_id {self.build_id}, then get branch name from properties.")
                self._branch_name = "main"
                self.print_status(f"simulating '{self._branch_name}' branch")
            else:
                latest_publish_build_info = self.teamcity.get_build_info_for_build_id(self.build_id)
                branch_name_property = next(
                    (
                        prop
                        for prop in latest_publish_build_info["resultingProperties"]["property"]
                        if prop["name"] == "teamcity.build.branch"
                    ),
                    None,
                )
                self._branch_name = branch_name_property["value"]
        return self._branch_name
    
    def _extract_kernel_versions(self, build_info) -> Dict[str, str]:
        """Extract kernel versions from build info."""
        kernel_versions = {}
        for KERNEL in KERNELS:
            kernel_versions[KERNEL.name_for_extracting_revision] = None

        for kernel in build_info["resultingProperties"]["property"]:
            if any(KERNEL.name_for_extracting_revision == kernel["name"] for KERNEL in KERNELS):
                kernel_versions[kernel["name"]] = kernel["value"]

        return kernel_versions


def parse_common_arguments() -> argparse.Namespace:
    """Parse common command line arguments for DIMR automation scripts."""
    parser = argparse.ArgumentParser(
        description="DIMR Automation Script"
    )
    
    # General arguments
    parser.add_argument("--build_id", type=str, required=True,
                        help="Build ID chain for the DIMR release")
    parser.add_argument("--dry-run", action="store_true", default=False,
                        help="Run in dry-run mode without making any changes")
    
    # Atlassian/Confluence credentials
    parser.add_argument("--atlassian-username", type=str, default=None,
                        help="Atlassian/Confluence username")
    parser.add_argument("--atlassian-password", type=str, default=None,
                        help="Atlassian/Confluence password")
    
    # TeamCity credentials
    parser.add_argument("--teamcity-username", type=str, default=None,
                        help="TeamCity username")
    parser.add_argument("--teamcity-password", type=str, default=None,
                        help="TeamCity password")
    
    # SSH credentials
    parser.add_argument("--ssh-username", type=str, default=None,
                        help="SSH username for H7 server")
    parser.add_argument("--ssh-password", type=str, default=None,
                        help="SSH password for H7 server")
    
    # Git credentials
    parser.add_argument("--git-username", type=str, default=None,
                        help="Git username")
    parser.add_argument("--git-PAT", type=str, default=None,
                        help="Git Personal Access Token")
    
    # Legacy arguments (for backwards compatibility)
    parser.add_argument("--username", type=str, default=None,
                        help="Username (used for all services if specific credentials not provided)")
    parser.add_argument("--password", type=str, default=None,
                        help="Password (used for all services if specific credentials not provided)")
    
    return parser.parse_args()


def create_context_from_args(args: argparse.Namespace, require_atlassian: bool = True, require_git: bool = True, require_teamcity: bool = True, require_ssh: bool = True) -> DimrAutomationContext:
    """Create automation context from parsed arguments."""
    # Use specific credentials if provided, otherwise fall back to general credentials
    atlassian_username = args.atlassian_username or args.username
    atlassian_password = args.atlassian_password or args.password
    teamcity_username = args.teamcity_username or args.username
    teamcity_password = args.teamcity_password or args.password
    ssh_username = args.ssh_username or args.username
    ssh_password = args.ssh_password or args.password
    git_username = args.git_username or args.username
    git_pat = getattr(args, 'git_PAT', None)
    dry_run = args.dry_run

    return DimrAutomationContext(
        build_id=args.build_id,
        dry_run=dry_run,
        atlassian_username=atlassian_username,
        atlassian_password=atlassian_password,
        teamcity_username=teamcity_username,
        teamcity_password=teamcity_password,
        ssh_username=ssh_username,
        ssh_password=ssh_password,
        git_username=git_username,
        git_pat=git_pat,
        require_atlassian=require_atlassian,
        require_git=require_git,
        require_teamcity=require_teamcity,
        require_ssh=require_ssh
    )
