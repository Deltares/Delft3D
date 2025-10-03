import os
import sys
from datetime import datetime, timezone
from io import TextIOWrapper
from typing import List

from pyparsing import Enum

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.common_utils import SummaryResults
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.dimrset_delivery.teamcity_types import ConfigurationTestResult, ResultInfo
from ci_tools.example_utils.logger import LogLevel

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
"""
Retrieve and summarize TeamCity test results for DIMR release builds.

This script collects test results for dependent builds, filters relevant test builds, and generates a summary report
with statistics and percentages. Output is written to 'teamcity_test_results.txt'.
"""
BASE_URL = "https://dpcbuild.deltares.nl"
REST_API_URL = f"{BASE_URL}/httpAuth/app/rest"
HEADER_FMT = "{:>12s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}  ---  {:24s} (#{:s})"


class FilteredList(Enum):
    """
    Enum for filtering build types in TeamCity dependency chains.

    Usage
    -----
    Used to select relevant build types for test result aggregation.
    """

    DIMRSET_AGGREGATED_RELEASE_RESULTS_LINUX = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsLinux"
    DIMRSET_AGGREGATED_RELEASE_RESULTS_WINDOWS = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsWindows"
    DELFT3D_WINDOWS_TEST = "Delft3D_WindowsTest"
    DELFT3D_LINUX_TEST = "Delft3D_LinuxTest"


class ResultSummary:
    """
    Store summary data for test results.

    Parameters
    ----------
    name : str
        Name of the summary.
    """

    def __init__(self, name: str) -> None:
        self.name = name
        self.sum_passed = 0
        self.sum_failed = 0
        self.sum_exception = 0
        self.sum_ignored = 0
        self.sum_muted = 0


class ResultExecutiveSummary:
    """
    Store executive summary data for test results.

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
    """
    Store executive summary data for test results.

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


class TeamCityTestResultWriter(StepExecutorInterface):
    """
    Retrieve and write TeamCity test results for dependent builds.

    This class interacts with TeamCity to collect, summarize, and log test results for dependent builds.

    Parameters
    ----------
    context : DimrAutomationContext
        Automation context containing build information and logging.
    services : Services
        Service container providing access to TeamCity client and other services.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.__context = context
        self.__services = services

    def execute_step(self) -> bool:
        """
        Retrieve and summarize TeamCity test results for dependent builds.

        Returns
        -------
        bool
            True if all tests passed, False otherwise.

        Raises
        ------
        SystemExit
            If TeamCity credentials are missing or after processing test results.
        """
        start_time = datetime.now(timezone.utc)

        if not self.__services.teamcity:
            self.__context.log("Error: TeamCity credentials are required for this script", severity=LogLevel.ERROR)
            return False

        output_file = "teamcity_test_results.txt"
        if os.path.exists(output_file):
            os.remove(output_file)
        log_file = open(output_file, "a")

        self.__context.log(f"Start: {start_time}\n")
        log_to_file(log_file, f"Start: {start_time}\n")

        self.__context.log(f"Listing is written to: {output_file}")

        # 1. Get dependency chain of all dependent builds and Filter on relevant build IDs
        dependency_chain = self.__services.teamcity.get_dependent_build_ids_with_filter(
            self.__context.build_id,
            filtered_ids=[FilteredList.DELFT3D_WINDOWS_TEST.value, FilteredList.DELFT3D_LINUX_TEST.value],
        )
        self.__context.log(f"Dependency chain for build {self.__context.build_id}: {dependency_chain}")

        # 2. Loop over the builds and retrieve the test results and write to file
        result_list = []
        for dep_build_id in dependency_chain:
            test_result = self.__services.teamcity.get_build_test_results_from_teamcity(dep_build_id)
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
        self.__context.log(f"Total test failed: {tests_failed}")

        log_to_file(log_file, f"\nStart: {start_time}")
        log_to_file(log_file, f"End  : {datetime.now(timezone.utc)}")
        log_to_file(log_file, "Ready")
        self.__context.log(f"\nStart: {start_time}")
        self.__context.log(f"End  : {datetime.now(timezone.utc)}")
        self.__context.log("Ready")
        log_file.close()

        return tests_failed == 0


def log_to_file(log_file: TextIOWrapper, *args: str) -> None:
    """
    Write to a log file.

    Parameters
    ----------
    log_file : TextIOWrapper
        The file to write to.
    *args
        Variable number of arguments to be written to the log file.
    """
    log_file.write(" ".join(map(str, args)) + "\n")


def log_executive_summary(log_file: TextIOWrapper, summarydata: ExecutiveSummary) -> None:
    """
    Log executive summary to a file.

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
        log_to_file(log_file, f"{SummaryResults.TOTAL_TESTS.value}   : {total:6d}")
        log_to_file(log_file, f"    {SummaryResults.PASSED.value}    : {summary.sum_passed:6d}")
        log_to_file(log_file, f"    {SummaryResults.NOT_PASSED.value}: {not_passed:6d}")
        log_to_file(log_file, f"    Failed    : {summary.sum_failed:6d}")
        log_to_file(log_file, f"    {SummaryResults.EXCEPTION.value} : {summary.sum_exception:6d}")
        log_to_file(log_file, f"    Ignored   : {summary.sum_ignored:6d}")
        log_to_file(log_file, f"    Muted     : {summary.sum_muted:6d}")
        log_to_file(log_file, f"    {SummaryResults.PERCENTAGE.value}: {percentage:6.2f}")


def log_result_list(log_file: TextIOWrapper, name: str, engines: List[ConfigurationTestResult]) -> None:
    """
    Log engine list to a file.

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
        _log_configuration_line(log_file, configuration_line)
    sum_test_result = _get_sum_test_result(engines)

    configuration_summary = ResultExecutiveSummary(sum_test_result.passed, sum_test_result.get_not_passed_total())
    log_to_file(log_file, f"    Total     : {configuration_summary.total:6d}")
    log_to_file(log_file, f"    Passed    : {configuration_summary.passed:6d}")
    log_to_file(log_file, f"    Percentage: {configuration_summary.percentage:6.2f}")


def _get_sum_test_result(test_overview: List[ConfigurationTestResult]) -> ResultInfo:
    """
    Get sum of the test results.

    Parameters
    ----------
    test_overview : List[ConfigurationTestResult]
        List of configuration test results to aggregate.

    Returns
    -------
    ResultInfo
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


def _log_configuration_line(log_file: TextIOWrapper, line: ConfigurationTestResult) -> None:
    """
    Log configuration line to a file.

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


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_git=False, require_ssh=False, require_jira=False)
        services = Services(context)

        context.log("Starting Test Result Writer...")
        if TeamCityTestResultWriter(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed Test Result Writer!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nTest Result Writer interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Test Result Writer failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
