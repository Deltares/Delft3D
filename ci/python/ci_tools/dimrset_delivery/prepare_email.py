#!/usr/bin/env python3
"""Prepare a mail template for the release notification."""

from typing import Optional

from ci_tools.dimrset_delivery.common_utils import get_testbank_result_parser
from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.helpers.email_helper import EmailHelper
from ci_tools.dimrset_delivery.helpers.result_testbank_parser import ResultTestBankParser
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX
from ci_tools.dimrset_delivery.settings.teamcity_settings import PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT


def prepare_email(context: DimrAutomationContext) -> None:
    """Prepare a mail template for the release notification.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Preparing email template...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would prepare email template for DIMR version:", dimr_version)
        return

    parser = get_testbank_result_parser()
    previous_parser = get_previous_testbank_result_parser(context)

    helper = EmailHelper(
        dimr_version=dimr_version,
        kernel_versions=kernel_versions,
        current_parser=parser,
        previous_parser=previous_parser,
    )
    helper.generate_template()

    print("Email template preparation completed successfully!")


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
        return tuple(map(int, tag[len("DIMRset_") :].split(".")))
    return None


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False, require_ssh=False)

    print("Starting email template preparation...")
    prepare_email(context)
    print("Finished")
