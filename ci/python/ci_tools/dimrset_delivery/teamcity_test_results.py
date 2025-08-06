import os
import sys
from datetime import datetime, timezone
from io import TextIOWrapper
from typing import List, Optional

import requests
from pyparsing import Enum

from .dimr_context import DimrAutomationContext, create_context_from_args, parse_common_arguments
from .settings.general_settings import DRY_RUN_PREFIX

"""
This script retrieves test results from TeamCity for DIMR release builds.

It takes a build ID and:
1. Gets the dependency chain of all dependent builds from TeamCity
2. Filters for Windows and Linux test builds (Delft3D_WindowsTest, Delft3D_LinuxTest)
3. Retrieves test results for each dependent build with status: passed, failed, exception, ignored and muted
4. Generates a summary report with test statistics and percentages

The percentage is computed as: passed tests / total tests * 100

Usage examples:
teamcity_test_results.py --build_id 123456 --teamcity-username <user> --teamcity-password <pass> --dry-run

For complete list of arguments and options, run:
teamcity_test_results.py --help

Output: Creates 'teamcity_test_results.txt' with detailed test results
"""
BASE_URL = "https://dpcbuild.deltares.nl"
REST_API_URL = f"{BASE_URL}/httpAuth/app/rest"
HEADER_FMT = "{:>12s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}  ---  {:24s} (#{:s})"


class FilteredList(Enum):
    """Enum for filtering build types in TeamCity dependency chains."""

    DIMRSET_AGGREGATED_RELEASE_RESULTS_LINUX = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsLinux"
    DIMRSET_AGGREGATED_RELEASE_RESULTS_WINDOWS = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsWindows"
    DELFT3D_WINDOWS_TEST = "Delft3D_WindowsTest"
    DELFT3D_LINUX_TEST = "Delft3D_LinuxTest"


class ResultSummary:
    """A class to store summary data for test results."""

    def __init__(self, name: str) -> None:
        self.name = name
        self.sum_passed = 0
        self.sum_failed = 0
        self.sum_exception = 0
        self.sum_ignored = 0
        self.sum_muted = 0


class ResultExecutiveSummary:
    """A class to store data for test result summary.

    Parameters
    ----------
    passed : int
        Number of passed tests.
    failed : int
        Number of failed tests.
    """

    def __init__(self, passed: int, failed: int) -> None:
        self.passed = passed
        self.failed = failed
        self.total = passed + failed
        percentage = 0.0
        if self.total > 0:
            percentage = self.passed / self.total * 100.0
        self.percentage = percentage


class ExecutiveSummary:
    """A class to store executive summary data for test results.

    Parameters
    ----------
    name : str
        Name of the summary.
    summary : list[ResultSummary]
        List of result summaries.
    """

    def __init__(self, name: str, summary: list[ResultSummary]) -> None:
        self.name = name
        self.summary = summary


class ResultInfo:
    """A class to store configuration test results info.

    Parameters
    ----------
    passed : int
        Number of passed tests.
    failed : int
        Number of failed tests.
    ignored : int
        Number of ignored tests.
    muted : int
        Number of muted tests.
    exception : int
        Number of tests with exceptions.
    muted_exception : int
        Number of muted exceptions.
    """

    def __init__(
        self, passed: int, failed: int, ignored: int, muted: int, exception: int, muted_exception: int
    ) -> None:
        self.passed = passed
        self.failed = failed
        self.ignored = ignored
        self.muted = muted
        self.exception = exception
        self.muted_exception = muted_exception

    def get_total(self) -> int:
        """Get total number of testcases.

        Returns
        -------
        int
            Total testcases.
        """
        return self.passed + self.failed + self.exception + self.ignored + self.muted - self.muted_exception

    def get_not_passed_total(self) -> int:
        """Get total number of testcases that did not pass.

        Returns
        -------
        int
            Total testcases that did not pass.
        """
        return self.failed + self.exception + self.ignored + self.muted


class ConfigurationTestResult:
    """A class to store configuration test results info.

    Parameters
    ----------
    name : str
        Name of the configuration.
    build_nr : str
        Build number.
    passed : int
        Number of passed tests.
    failed : int
        Number of failed tests.
    ignored : int
        Number of ignored tests.
    muted : int
        Number of muted tests.
    status_text : str
        Status text description.
    """

    def __init__(
        self,
        name: str,
        build_nr: str,
        passed: int,
        failed: int,
        ignored: int,
        muted: int,
        status_text: str,
    ) -> None:
        self.name = name
        self.build_nr = build_nr
        self.status_text = status_text
        self.exceptions: List[str] = []
        self.test_result = ResultInfo(passed, failed, ignored, muted, 0, 0)

    def get_total(self) -> int:
        """Get total number of testcases.

        Returns
        -------
        int
            Total testcases.
        """
        return self.test_result.get_total()

    def get_not_passed_total(self) -> int:
        """Get total number of testcases that did not pass.

        Returns
        -------
        int
            Total testcases that did not pass.
        """
        return self.test_result.get_not_passed_total()


def get_sum_test_result(test_overview: List[ConfigurationTestResult]) -> ResultInfo:
    """Get sum of the test results.

    Parameters
    ----------
    test_overview : List[ConfigurationTestResult]
        List of configuration test results to aggregate.

    Returns
    -------
    TestResult
        Data object with the aggregated sum of the tests.
    """
    sum_passed = 0
    sum_failed = 0
    sum_exception = 0
    sum_ignored = 0
    sum_muted = 0
    sum_muted_exception = 0
    for test in test_overview:
        sum_passed += test.test_result.passed
        sum_failed += test.test_result.failed
        sum_ignored += test.test_result.ignored
        sum_muted += test.test_result.muted
        sum_exception += test.test_result.exception
        sum_muted_exception += test.test_result.muted_exception
    return ResultInfo(sum_passed, sum_failed, sum_ignored, sum_muted, sum_exception, sum_muted_exception)


def log_to_file(log_file: TextIOWrapper, *args: str) -> None:
    """Write to a log file.

    Parameters
    ----------
    log_file : TextIOWrapper
        The file it logs to.
    *args
        Variable number of arguments to be written to the log file.
    """
    log_file.write(" ".join(map(str, args)) + "\n")


def log_executive_summary(log_file: TextIOWrapper, summarydata: ExecutiveSummary) -> None:
    """Log executive summary to a file.

    Parameters
    ----------
    log_file : TextIOWrapper
        The file to write the executive summary to.
    summarydata : ExecutiveSummary
        The executive summary data to log.
    """
    log_to_file(log_file, f"\nTestbench root: {summarydata.name}")
    for summary in summarydata.summary:
        total = (
            summary.sum_passed + summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        )
        not_passed = summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        percentage = 0.0
        if total > 0:
            percentage = summary.sum_passed / total * 100.0

        log_to_file(log_file, f"\nSummary: {summary.name}")
        log_to_file(log_file, f"Total tests   : {total:6d}")
        log_to_file(log_file, f"    Passed    : {summary.sum_passed:6d}")
        log_to_file(log_file, f"    Not passed: {not_passed:6d}")
        log_to_file(log_file, f"    Failed    : {summary.sum_failed:6d}")
        log_to_file(log_file, f"    Exception : {summary.sum_exception:6d}")
        log_to_file(log_file, f"    Ignored   : {summary.sum_ignored:6d}")
        log_to_file(log_file, f"    Muted     : {summary.sum_muted:6d}")
        log_to_file(log_file, f"    Percentage: {percentage:6.2f}")


def log_result_list(log_file: TextIOWrapper, name: str, engines: List[ConfigurationTestResult]) -> None:
    """Log engine list to a file.

    Parameters
    ----------
    log_file : TextIOWrapper
        The file to write the results to.
    name : str
        Name/title for the result list section.
    engines : List[ConfigurationTestResult]
        List of configuration test results to log.
    """
    log_to_file(log_file, f"{name}")
    log_to_file(
        log_file,
        HEADER_FMT.format("total", "passed", "failed", "except", "ignored", "muted", "%", "test case name", "build"),
    )
    for configuration_line in engines:
        log_coniguration_line(log_file, configuration_line)
    sum_test_result = get_sum_test_result(engines)

    configuration_summary = ResultExecutiveSummary(sum_test_result.passed, sum_test_result.get_not_passed_total())
    log_to_file(log_file, f"    Total     : {configuration_summary.total:6d}")
    log_to_file(log_file, f"    Passed    : {configuration_summary.passed:6d}")
    log_to_file(log_file, f"    Percentage: {configuration_summary.percentage:6.2f}")


def log_coniguration_line(log_file: TextIOWrapper, line: ConfigurationTestResult) -> None:
    """Log configuration line to a file.

    Parameters
    ----------
    log_file : TextIOWrapper
        The file to write the configuration line to.
    line : ConfigurationTestResult
        The configuration test result to log.
    """
    total = line.get_total()
    if total != 0:
        percentage = line.test_result.passed / total * 100.0
    else:
        percentage = 0
    if total > 0:
        log_to_file(
            log_file,
            "{:12d} {:8d} {:8d} {:8d} {:8d} {:8d} {:8.2f}  ---  {:24s} (#{:s})".format(
                total,
                line.test_result.passed,
                line.test_result.failed,
                line.test_result.exception,
                line.test_result.ignored,
                line.test_result.muted,
                percentage,
                line.name,
                line.build_nr,
            ),
        )

    else:
        log_to_file(log_file, HEADER_FMT.format("x", "x", "x", "x", "x", "x", "x", line.name, line.build_nr))
        log_to_file(
            log_file,
            f"                                                                            xxx  {line.status_text}",
        )

    if line.test_result.exception != 0:
        for exception in line.exceptions:
            log_to_file(
                log_file,
                f"                                                           "
                f"                 xxx  Exception {exception}",
            )


def get_build_dependency_chain(
    context: DimrAutomationContext, filtered_list: Optional[list[FilteredList]] = None
) -> list[str]:
    """
    Get dependency chain of all dependent builds for a given build ID from TeamCity.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing TeamCity client and build configuration.
    filtered_list : Optional[list[FilteredList]], optional
        Optional filter to include only builds whose buildTypeId matches one of the values.

    Returns
    -------
    list[str]
        List of dependent build IDs (snapshot dependencies).
    """
    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would get dependency chain for build {context.build_id}")
        if filtered_list:
            filter_values = [item.value for item in filtered_list]
            print(f"{DRY_RUN_PREFIX} Would filter by build types: {filter_values}")
        # Return mock dependency chain for dry run
        return ["123456", "123457", "123458"]

    # Use the existing TeamCity method to get filtered dependent builds
    if filtered_list:
        # For now, we'll use the existing method and then filter manually
        # as the TeamCity class method uses TEAMCITY_IDS enum, not our FilteredList
        endpoint = (
            f"{context.teamcity._TeamCity__rest_uri}builds?locator=defaultFilter:false,"
            f"snapshotDependency(to:(id:{context.build_id})),count:1000&fields=build(id,buildTypeId)"
        )
        result = requests.get(
            url=endpoint, headers=context.teamcity._TeamCity__default_headers, auth=context.teamcity._TeamCity__auth
        )
        if result.status_code == 200:
            build_data = result.json()
            dependency_chain = []
            filter_values = [item.value for item in filtered_list]

            for build in build_data.get("build", []):
                dep_id = build.get("id")
                build_type_id = build.get("buildTypeId")
                if dep_id and build_type_id in filter_values:
                    dependency_chain.append(str(dep_id))
            return dependency_chain
        else:
            print(f"Could not retrieve dependencies for build ID {context.build_id}")
            return []
    else:
        # If no filter, get all dependencies
        return context.teamcity.get_filtered_dependent_build_ids(context.build_id)


def get_build_test_results_from_teamcity(
    context: DimrAutomationContext, build_id: int
) -> Optional[ConfigurationTestResult]:
    """
    Fetch test results for a given build ID using the TeamCity client.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing TeamCity client and build configuration.
    build_id : int
        The build ID to retrieve results for.

    Returns
    -------
    Optional[ConfigurationTestResult]
        An object containing the parsed test results for the build, or None if no test results.
    """
    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would get test results for build {build_id}")
        # Return mock test result for dry run
        return ConfigurationTestResult(
            name=f"{DRY_RUN_PREFIX} Test Configuration / Build {build_id}",
            build_nr=str(build_id),
            passed=85,
            failed=0,
            ignored=0,
            muted=0,
            status_text=f"{DRY_RUN_PREFIX} SUCCESS",
        )

    build_info = context.teamcity.get_full_build_info_for_build_id(build_id)
    if not build_info:
        return None

    # Check if there are test results
    test_occurrences = build_info.get("testOccurrences", {})
    if not test_occurrences or int(test_occurrences.get("count", "0")) == 0:
        return None

    # Extract build information
    build_nr = build_info.get("number", "Unknown build number")
    status_text = build_info.get("status", "No status available")

    # Build up config_name including parent project(s)
    config_name = "Unknown config"
    parent = "Unknown parent"
    build_type = build_info.get("buildType", {})
    if build_type:
        config_name = build_type.get("name", "Unknown config")
        parent = build_type.get("projectName", "Unknown parent")
    config_name = f"{parent} / {config_name}"

    # Extract test counts
    passed = int(test_occurrences.get("passed", "0"))
    failed = int(test_occurrences.get("failed", "0"))
    ignored = int(test_occurrences.get("ignored", "0"))
    muted = int(test_occurrences.get("muted", "0"))

    return ConfigurationTestResult(
        name=config_name,
        build_nr=build_nr,
        passed=passed,
        failed=failed,
        ignored=ignored,
        muted=muted,
        status_text=status_text,
    )


if __name__ == "__main__":
    start_time = datetime.now(timezone.utc)

    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False, require_ssh=False)

    # Extract TeamCity client from context
    if not context.teamcity:
        print("Error: TeamCity credentials are required for this script")
        sys.exit(1)

    build_id = args.build_id
    output_file = "teamcity_test_results.txt"
    if os.path.exists(output_file):
        os.remove(output_file)
    log_file = open(output_file, "a")

    print(f"Start: {start_time}\n")
    log_to_file(log_file, f"Start: {start_time}\n")

    print(f"Listing is written to: {output_file}")

    # 1. Get dependency chain of all dependent builds and Filter on relevant build IDs
    dependency_chain = get_build_dependency_chain(
        context,
        [
            FilteredList.DELFT3D_WINDOWS_TEST,
            FilteredList.DELFT3D_LINUX_TEST,
        ],
    )
    print(f"Dependency chain for build {build_id}: {dependency_chain}")

    # 2. Loop over the builds and retrieve the test results and write to file
    result_list = []
    for dep_build_id in dependency_chain:
        test_result = get_build_test_results_from_teamcity(context, dep_build_id)
        if test_result:
            result_list.append(test_result)

    # 3. Write test results to file
    result_list.sort(key=lambda x: x.name)
    log_result_list(log_file, "DIMR Testbench Release", result_list)

    # 4. Write executive summary to file
    summary = ResultSummary("All")
    for result in result_list:
        summary.sum_passed += result.test_result.passed
        summary.sum_failed += result.test_result.failed
        summary.sum_exception += result.test_result.exception
        summary.sum_ignored += result.test_result.ignored
        summary.sum_muted += result.test_result.muted

    executive_summary = ExecutiveSummary("DIMR Testbench Release", [summary])
    log_executive_summary(log_file, executive_summary)

    tests_failed = sum(result.get_not_passed_total() for result in result_list)
    print(f"Total test failed: {tests_failed}")

    log_to_file(log_file, f"\nStart: {start_time}")
    log_to_file(log_file, f"End  : {datetime.now(timezone.utc)}")
    log_to_file(log_file, "Ready")
    print(f"\nStart: {start_time}")
    print(f"End  : {datetime.now(timezone.utc)}")
    print("Ready")

    sys.exit(tests_failed)
