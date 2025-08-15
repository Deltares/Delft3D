"""
Common utilities for DIMR automation scripts.

Provides shared initialization and helper functions.
"""

import re
from typing import Optional

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext


class ResultTestBankParser:
    """Object responsible for parsing a specific testbank result artifact."""

    def __init__(self, testbank_result: str) -> None:
        """
        Create a new instance of ResultTestBankParser.

        Parameters
        ----------
        testbank_result : str
            The testbank result as a string.
        """
        self.testbank_result = testbank_result

    def get_percentage_total_passing(self) -> str:
        """Get the total percentage of passing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Percentage\D*([0-9.]*)", substring)
        percentage: str = matches[0]
        return percentage

    def get_total_tests(self) -> str:
        """Get the total number of tests."""
        matches = re.findall(r"Total tests\D*([0-9.]*)", self.testbank_result)
        total_number: str = matches[0]
        return total_number

    def get_total_passing(self) -> str:
        """Get the total number of passing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Passed\D*([0-9.]*)", substring)
        total_number: str = matches[0]
        return total_number

    def get_total_failing(self) -> str:
        """Get the total number of failing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Not passed\D*([0-9.]*)", substring)
        total_number: str = matches[0]
        return total_number

    def get_total_exceptions(self) -> str:
        """Get the total number of exceptions that occurred."""
        matches = re.findall(r"Exception\D*:\D*([0-9.]*)", self.testbank_result)
        total_number: str = matches[0]
        return total_number


def get_testbank_result_parser(path_to_release_test_results_artifact: str) -> ResultTestBankParser:
    """Get a new ResultTestBankParser for the latest test bench results from a local file.

    Returns
    -------
    ResultTestBankParser
        Parser instance for the test results.
    """
    with open(path_to_release_test_results_artifact, "rb") as f:
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

    # Get all builds for the publish build configuration
    latest_builds = context.teamcity.get_builds_for_build_configuration_id(
        build_configuration_id=build_type_id,
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
        path_to_artifact=context.settings.path_to_release_test_results_artifact,
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
        try:
            return tuple(map(int, tag[len("DIMRset_") :].split(".")))
        except ValueError:
            return None
    return None
