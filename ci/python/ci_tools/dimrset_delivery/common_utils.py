"""
Common utilities for DIMR automation scripts.

Provides shared initialization and helper functions.
"""

import argparse
from getpass import getpass
from typing import Tuple

from .helpers.GitClient import GitClient
from .helpers.SshClient import SshClient
from .lib.Atlassian import Atlassian
from .lib.TeamCity import TeamCity
from .settings.general_settings import DELFT3D_GIT_REPO, DRY_RUN_PREFIX


def parse_common_arguments() -> argparse.Namespace:
    """Parse common command line arguments for DIMR automation scripts."""
    parser = argparse.ArgumentParser(description="--username and --password are optional for automation")

    parser.add_argument("--username", type=str, default=None)
    parser.add_argument("--password", type=str, default=None)
    parser.add_argument("--git-PAT", type=str, default=None)
    parser.add_argument("--build_id", type=str, required=True, help="Build ID chain for the DIMR release")
    parser.add_argument(
        "--dry-run", type=str, default="false", help="Run in dry-run mode without making any changes (true/false)"
    )

    return parser.parse_args()


def get_credentials(args: argparse.Namespace) -> Tuple[str, str, str]:
    """Get credentials from arguments or prompt user."""
    username = args.username
    password = args.password
    personal_access_token = args.git_PAT

    if not username or not password or not personal_access_token:
        username = input("Enter your Deltares username:")
        password = getpass(prompt="Enter your Deltares password:", stream=None)
        personal_access_token = getpass(prompt="Enter your git PAT:", stream=None)

    return username, password, personal_access_token


def initialize_clients(
    username: str, password: str, personal_access_token: str
) -> Tuple[Atlassian, TeamCity, SshClient, GitClient]:
    """Initialize all required client wrappers."""
    atlassian_wrapper = Atlassian(username=username, password=password)
    teamcity_wrapper = TeamCity(username=username, password=password)
    ssh_client_wrapper = SshClient(username=username, password=password, connect_timeout=30)
    git_client_wrapper = GitClient(DELFT3D_GIT_REPO, username, personal_access_token)

    return atlassian_wrapper, teamcity_wrapper, ssh_client_wrapper, git_client_wrapper


def print_dry_run_message(dry_run: bool) -> None:
    """Print dry run message if applicable."""
    if dry_run:
        print(f"{DRY_RUN_PREFIX} - no changes will be made")
