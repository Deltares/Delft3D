"""
Common utilities for DIMR automation scripts.

Provides shared initialization and helper functions for DIMR automation.
"""

import re
from enum import Enum
from typing import Optional

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services


class SummaryResults(str, Enum):
    """Enum representing summary result keys for DIMR testbank results."""

    TOTAL_TESTS = "Total tests"
    PASSED = "Passed"
    NOT_PASSED = "Not passed"
    EXCEPTION = "Exception"
    PERCENTAGE = "Percentage"


# Mock data for dry-run mode
MOCK_CURRENT_TEST_RESULTS = f"""
Summary: All
{SummaryResults.TOTAL_TESTS.value}   :   2000
    {SummaryResults.PASSED.value}    :   2000
    {SummaryResults.NOT_PASSED.value}:      0
    Failed    :      0
    {SummaryResults.EXCEPTION.value} :      0
    Ignored   :      0
    Muted     :      0
    {SummaryResults.PERCENTAGE.value}: 100.00
"""

MOCK_PREVIOUS_TEST_RESULTS = f"""
Summary: All
{SummaryResults.TOTAL_TESTS.value}   :   1900
    {SummaryResults.PASSED.value}    :   1800
    {SummaryResults.NOT_PASSED.value}:      20
    Failed    :      20
    {SummaryResults.EXCEPTION.value} :      20
    Ignored   :      20
    Muted     :      20
    {SummaryResults.PERCENTAGE.value}: 94.74
"""


class ResultTestBankParser:
    """
    Parses a specific testbank result artifact.

    Use this class to extract test statistics from DIMR testbank result strings.
    """

    def __init__(self, testbank_result: str) -> None:
        """
        Initialize the parser with a testbank result string.

        Parameters
        ----------
        testbank_result : str
            The testbank result as a string.
        """
        start_index = testbank_result.find("Summary")
        self.summary = testbank_result[start_index:]  # get all text from "Summary" to end of file.

    # public uses an enum
    def get_value(self, key: SummaryResults) -> str:
        """
        Get the value given a key.

        Returns
        -------
        str
            The key.
        """
        pattern = rf"{re.escape(key)}\s*:\s*([0-9.]+)"
        matches = re.findall(pattern, self.summary)
        if not matches:
            raise KeyError(f"Key '{key}' not found in summary.")
        return str(matches[0])


def get_testbank_result_parser(context: DimrAutomationContext) -> ResultTestBankParser:
    """
    Get a new ResultTestBankParser for the latest test bench results from a local file.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing configuration and state.

    Returns
    -------
    ResultTestBankParser
        Parser instance for the test results.
    """
    if context.dry_run:
        context.log("Create mock parsers with sensible default values for dry-run")
        return ResultTestBankParser(MOCK_CURRENT_TEST_RESULTS.strip())

    with open(context.settings.path_to_release_test_results_artifact, "rb") as f:
        artifact = f.read()
    return ResultTestBankParser(artifact.decode())


def get_previous_testbank_result_parser(
    context: DimrAutomationContext, services: Services
) -> Optional[ResultTestBankParser]:
    """
    Get a new ResultTestBankParser for the previous versioned tagged test bench results.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    services : Services
        Services object providing access to TeamCity and other APIs.

    Returns
    -------
    Optional[ResultTestBankParser]
        Parser for previous test results, or None if not found.

    Raises
    ------
    ValueError
        If TeamCity client is not initialized.
    """
    if context.dry_run:
        context.log("Create mock parsers with sensible default values for dry-run")
        return ResultTestBankParser(MOCK_PREVIOUS_TEST_RESULTS.strip())

    if not services.teamcity:
        raise ValueError("TeamCity client is required but not initialized")

    current_build_info = services.teamcity.get_full_build_info_for_build_id(context.build_id)
    if not current_build_info:
        return None

    build_type_id = current_build_info.get("buildTypeId")
    if not build_type_id:
        return None

    current_tag_name = get_tag_from_build_info(current_build_info)

    # Get all builds for the publish build configuration
    number_of_builds = 50
    latest_builds = services.teamcity.get_builds_for_build_configuration_id(
        build_configuration_id=build_type_id,
        limit=number_of_builds,
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

        loop_build_info = services.teamcity.get_full_build_info_for_build_id(str(build_id))
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
    artifact = services.teamcity.get_build_artifact(
        build_id=f"{previous_build_id}",
        path_to_artifact=context.settings.path_to_release_test_results_artifact,
    )
    if artifact is None:
        return None

    return ResultTestBankParser(artifact.decode())


def get_tag_from_build_info(current_build_info: dict) -> tuple:
    """
    Extract tag information from build info.

    Parameters
    ----------
    current_build_info : dict
        Build information dictionary from TeamCity.

    Returns
    -------
    tuple
        Tuple containing version numbers (major, minor, patch).
    """
    tags = current_build_info.get("tags", {}).get("tag", [])
    for tag in tags:
        tag_name = tag.get("name")
        if tag_name and tag_name.startswith("DIMRset_"):
            parsed_version = parse_version(tag_name)
            if parsed_version is not None:
                return parsed_version
    return (0, 0, 0)


def parse_version(tag: str) -> Optional[tuple]:
    """
    Parse version string from tag.

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
