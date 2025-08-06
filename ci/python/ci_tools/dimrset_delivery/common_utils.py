"""
Common utilities for DIMR automation scripts.

Provides shared initialization and helper functions.
"""

from typing import Tuple

from ci_tools.dimrset_delivery.helpers.git_client import GitClient
from ci_tools.dimrset_delivery.helpers.result_testbank_parser import ResultTestBankParser
from ci_tools.dimrset_delivery.helpers.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.general_settings import DELFT3D_GIT_REPO, DRY_RUN_PREFIX
from ci_tools.dimrset_delivery.settings.teamcity_settings import PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT


def initialize_clients(
    username: str, password: str, personal_access_token: str
) -> Tuple[Atlassian, TeamCity, SshClient, GitClient]:
    """Initialize all required client wrappers.

    Parameters
    ----------
    username : str
        Username for authentication.
    password : str
        Password for authentication.
    personal_access_token : str
        Personal access token for Git authentication.

    Returns
    -------
    Tuple[Atlassian, TeamCity, SshClient, GitClient]
        Initialized client wrappers.
    """
    atlassian_wrapper = Atlassian(username=username, password=password)
    teamcity_wrapper = TeamCity(username=username, password=password)
    ssh_client_wrapper = SshClient(username=username, password=password, connect_timeout=30)
    git_client_wrapper = GitClient(DELFT3D_GIT_REPO, username, personal_access_token)

    return atlassian_wrapper, teamcity_wrapper, ssh_client_wrapper, git_client_wrapper


def get_testbank_result_parser() -> ResultTestBankParser:
    """Get a new ResultTestBankParser for the latest test bench results from a local file.

    Returns
    -------
    ResultTestBankParser
        Parser instance for the test results.
    """
    with open(PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT, "rb") as f:
        artifact = f.read()
    return ResultTestBankParser(artifact.decode())


def print_dry_run_message(dry_run: bool) -> None:
    """Print dry run message if applicable.

    Parameters
    ----------
    dry_run : bool
        Whether running in dry-run mode.
    """
    if dry_run:
        print(f"{DRY_RUN_PREFIX} - no changes will be made")
