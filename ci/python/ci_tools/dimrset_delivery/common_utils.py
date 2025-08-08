"""
Common utilities for DIMR automation scripts.

Provides shared initialization and helper functions.
"""

from typing import Optional, Tuple

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
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


def get_previous_testbank_result_parser(context: DimrAutomationContext) -> Optional[ResultTestBankParser]:
    """Get a new ResultTestBankParser for the previous versioned tagged test bench results.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Returns
    -------
    Optional[ResultTestBankParser]
        Parser for previous test results, or None if not found.
    """
    if not context.teamcity:
        raise ValueError("TeamCity client is required but not initialized")

    current_build_info = context.teamcity.get_full_build_info_for_build_id(context.build_id)
    if not current_build_info:
        return None

    build_type_id = current_build_info.get("buildTypeId")
    if not build_type_id:
        return None

    current_tag_name = get_tag_from_build_info(current_build_info)

    # Get all builds for the publish build type
    latest_builds = context.teamcity.get_builds_for_build_type_id(
        build_type_id=build_type_id,
        limit=50,
        include_failed_builds=False,
    )

    if latest_builds is None:
        return None

    previous_build_id = None
    previous_version = None

    # Find previous versioned tagged build (major.minor.patch < current)
    builds_list = latest_builds.get("build", []) if isinstance(latest_builds, dict) else []
    for build in builds_list:
        build_id = build.get("id")
        if build_id == int(context.build_id):
            continue

        loop_build_info = context.teamcity.get_full_build_info_for_build_id(str(build_id))
        if not loop_build_info:
            continue

        loop_tag_name = get_tag_from_build_info(loop_build_info)

        if loop_tag_name and loop_tag_name != (0, 0, 0) and current_tag_name and loop_tag_name < current_tag_name:
            if previous_version is None or loop_tag_name > previous_version:
                previous_build_id = build_id
                previous_version = loop_tag_name

    # Previous version not found
    if previous_build_id is None:
        return None

    # Download artifact for previous build
    artifact = context.teamcity.get_build_artifact(
        build_id=f"{previous_build_id}",
        path_to_artifact=PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT,
    )
    if artifact is None:
        return None

    return ResultTestBankParser(artifact.decode())


def get_tag_from_build_info(current_build_info: dict) -> tuple:
    """Extract tag information from build info.

    Parameters
    ----------
    current_build_info : dict
        Build information dictionary from TeamCity.

    Returns
    -------
    tuple
        Tuple containing version numbers (major, minor, patch).
    """
    current_tag_name = (0, 0, 0)
    tags = current_build_info.get("tags", {}).get("tag", [])
    for tag in tags:
        tag_name = tag.get("name")
        if tag_name and tag_name.startswith("DIMRset_"):
            parsed_version = parse_version(tag_name)
            if parsed_version is not None:
                current_tag_name = parsed_version
    return current_tag_name


def print_dry_run_message(dry_run: bool) -> None:
    """Print dry run message if applicable.

    Parameters
    ----------
    dry_run : bool
        Whether running in dry-run mode.
    """
    if dry_run:
        print(f"{DRY_RUN_PREFIX} - no changes will be made")


def parse_version(tag: str) -> Optional[tuple]:
    """Parse version string from tag.

    Parameters
    ----------
    tag : str
        Tag string to parse (e.g., 'DIMRset_1.2.3').

    Returns
    -------
    Optional[tuple]
        Tuple of version numbers (major, minor, patch) or None if parsing fails.
    """
    if tag and tag.startswith("DIMRset_"):
        try:
            return tuple(map(int, tag[len("DIMRset_") :].split(".")))
        except ValueError:
            return None
    return None
