#!/usr/bin/env python3
"""
DIMR Automation Step 4: Prepare Email
This script prepares a mail template for the release notification.
"""

from typing import Optional

from dimr_context import DimrAutomationContext, parse_common_arguments, create_context_from_args
from helpers.EmailHelper import EmailHelper
from helpers.TestbankResultParser import TestbankResultParser
from settings.general_settings import DRY_RUN_PREFIX
from settings.teamcity_settings import PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT


def prepare_email(context: DimrAutomationContext) -> None:
    """Prepares a mail template for the release notification."""
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


def get_testbank_result_parser() -> TestbankResultParser:
    """Gets a new TestbankResultParser for the latest test bench results from a local file."""
    with open(PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT, "rb") as f:
        artifact = f.read()
    return TestbankResultParser(artifact.decode())


def get_previous_testbank_result_parser(context: DimrAutomationContext) -> Optional[TestbankResultParser]:
    """Gets a new TestbankResultParser for the previous versioned tagged test bench results."""
    current_build_info = context.teamcity.get_full_build_info_for_build_id(context.build_id)
    build_type_id = current_build_info.get("buildTypeId")
    current_tag_name = get_tag_from_build_info(current_build_info)

    # Get all builds for the publish build type
    latest_builds = context.teamcity.get_builds_for_build_type_id(
        build_type_id=build_type_id,
        limit=50,
        include_failed_builds=False,
    )

    if latest_builds is None:
        latest_builds = []

    previous_build_id = None
    previous_version = None

    # Find previous versioned tagged build (major.minor.patch < current)
    for build in latest_builds.get("build", {}):
        build_id = build.get("id")
        if build_id == int(context.build_id):
            continue

        loop_build_info = context.teamcity.get_full_build_info_for_build_id(build_id)
        loop_tag_name = get_tag_from_build_info(loop_build_info)

        if (
            loop_tag_name
            and loop_tag_name != (0, 0, 0)
            and current_tag_name
            and loop_tag_name < current_tag_name
        ):
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
    return TestbankResultParser(artifact.decode())


def get_tag_from_build_info(current_build_info):
    """Extract tag information from build info."""
    current_tag_name = (0, 0, 0)
    tags = current_build_info.get("tags", {}).get("tag", [])
    for tag in tags:
        tag_name = tag.get("name")
        if tag_name.startswith("DIMRset_"):
            current_tag_name = parse_version(tag_name)
    return current_tag_name


def parse_version(tag):
    """Parse version string from tag."""
    if tag and tag.startswith("DIMRset_"):
        return tuple(map(int, tag[len("DIMRset_") :].split(".")))
    return None


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args)
    
    print("Starting email template preparation...")
    prepare_email(context)
    print("Finished")
