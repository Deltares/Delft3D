import argparse
import getpass
import os
import sys
import xml.etree.ElementTree as ET
from datetime import datetime
from io import TextIOWrapper
from typing import List, Optional

import requests
from pyparsing import Enum
from requests.auth import HTTPBasicAuth

"""
This script list the test bench results with status: passed, failed, exception, ignored and muted.
The percentage is computed as follows: the passed tests divide by the total number of tests

The test benchroot need to specified by its projectid.
This can be taken from the web-adress.
Ex. DIMR testbench daily:  https://dpcbuild.deltares.nl/project.html?projectId=Delft3DSobek_DimrTestbench&tab=projectOverview
The project id is: Delft3DSobek_DimrTestbench
See the examples below
Structure of testbench should be: root -> engine test -> functionality tests

teamcity_retrieve_engine_test_status.py --tbroot DFlowFlexibleMesh
teamcity_retrieve_engine_test_status.py --tbroot Dimr_DimrTestbenchRelease  # DIMR testbench release
teamcity_retrieve_engine_test_status.py --tbroot Delft3DSobek_DimrTestbench  # DIMR testbench daily
"""
BASE_URL = "https://dpcbuild.deltares.nl"
REST_API_URL = f"{BASE_URL}/httpAuth/app/rest"
PROJECTS_URL = f"{REST_API_URL}/projects/id:"
TEST_OCCURRENCES = "./testOccurrences"
HEADER_FMT = "{:>12s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}  ---  {:24s} (#{:s})"


class FILTERED_LIST(Enum):
    DIMRSET_AGGREGATED_RELEASE_RESULTS_LINUX = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsLinux"
    DIMRSET_AGGREGATED_RELEASE_RESULTS_WINDOWS = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsWindows"
    DELFT3D_WINDOWS_TEST = "Delft3D_WindowsTest"
    DELFT3D_LINUX_TEST = "Delft3D_LinuxTest"


class TestResultSummary(object):
    """A class to store summary data for test results."""

    def __init__(self, name: str) -> None:
        self.name = name
        self.sum_passed = 0
        self.sum_failed = 0
        self.sum_exception = 0
        self.sum_ignored = 0
        self.sum_muted = 0


class TestResultExecutiveSummary(object):
    """A class to store data for test result summary."""

    def __init__(self, passed: int, failed: int) -> None:
        self.passed = passed
        self.failed = failed
        self.total = passed + failed
        a = 0.0
        if self.total > 0:
            a = float(self.passed) / float(self.total) * 100.0
        self.percentage = a


class ExecutiveSummary(object):
    """A class to store executive summary data for test results."""

    def __init__(self, name: str, summary: list[TestResultSummary]) -> None:
        self.name = name
        self.summary = summary


class TestResult(object):
    """A class to store configuration test results info."""

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


class ConfigurationTestResult(object):
    """A class to store configuration test results info."""

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
        self.test_result = TestResult(passed, failed, ignored, muted, 0, 0)

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


def get_sum_test_result(test_overview: List[ConfigurationTestResult]) -> TestResult:
    """Get sum of the test results.

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
    return TestResult(sum_passed, sum_failed, sum_ignored, sum_muted, sum_exception, sum_muted_exception)


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


def get_status_text(build: ET.Element) -> str:
    """Get status text from xml node.

    Returns
    -------
    str
        The status text.
    """
    status_text = ""
    if build.find(TEST_OCCURRENCES) is None:
        status = build.find("statusText")
        if status is not None:
            return str(status.text)
        else:
            return "Build failed!"
    return status_text


def get_number_of_tests(build: ET.Element, test_result: str) -> int:
    """Get number of tests from xml node.

    Returns
    -------
    int
        Number of tests that match the test result.
    """
    test_occurences = build.find(TEST_OCCURRENCES)
    if test_occurences is not None:
        test_occurences_attrib = test_occurences.attrib
        if test_result in test_occurences_attrib:
            return int(test_occurences_attrib[test_result])

    return 0


def create_configuration_test_result(build: ET.Element, name: str, status_text: str) -> ConfigurationTestResult:
    """Create configuration test result from XML node.

    Returns
    -------
    ConfigurationTestResult
        Configuration test result from XML.
    """
    build_nr = ""
    if "number" in build.attrib:
        build_nr = build.attrib["number"]
    passed = get_number_of_tests(build, "passed")
    failed = get_number_of_tests(build, "failed")
    ignored = get_number_of_tests(build, "ignored")
    muted = get_number_of_tests(build, "muted")
    return ConfigurationTestResult(name, build_nr, passed, failed, ignored, muted, status_text)


def get_request(url: str, username: str, password: str) -> requests.Response:
    """Send an HTTP GET request with authentication.

    Returns
    -------
    requests.Response
        The response object from the request.
    """
    headers = {"Accept": "application/xml"}
    return requests.get(url=url, auth=HTTPBasicAuth(username, password), headers=headers, stream=True, verify=True)


def text_in_xml_message(text: str) -> bool:
    """Check if HTTP GET response has text.

    Parameters
    ----------
    text : str
        The HTTP GET response to check.

    Returns
    -------
    bool
        true or false depending on the text.
    """
    try:
        ET.fromstring(text)
        return True
    except:
        print(f"Text is not in XML format: {text}")
        return False


def project_is_archived(project: ET.Element) -> bool:
    """Determine if project is archived."""
    return bool(project.attrib.get("archived", False))


def log_executive_summary(log_file: TextIOWrapper, summarydata: ExecutiveSummary) -> None:
    """Log executive summary to a file."""
    log_to_file(log_file, f"\nTestbench root: {summarydata.name}")
    for summary in summarydata.summary:
        total = (
            summary.sum_passed + summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        )
        not_passed = summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        percentage = 0.0
        if total > 0:
            percentage = float(summary.sum_passed) / float(total) * 100.0

        log_to_file(log_file, f"\nSummary: {summary.name}")
        log_to_file(log_file, f"Total tests   : {total:6d}")
        log_to_file(log_file, f"    Passed    : {summary.sum_passed:6d}")
        log_to_file(log_file, f"    Not passed: {not_passed:6d}")
        log_to_file(log_file, f"    Failed    : {summary.sum_failed:6d}")
        log_to_file(log_file, f"    Exception : {summary.sum_exception:6d}")
        log_to_file(log_file, f"    Ignored   : {summary.sum_ignored:6d}")
        log_to_file(log_file, f"    Muted     : {summary.sum_muted:6d}")
        log_to_file(log_file, f"    Percentage: {float(percentage):6.2f}")


def log_result_list(log_file: TextIOWrapper, name: str, engines: List[ConfigurationTestResult]) -> None:
    """Log engine list to a file."""
    log_to_file(log_file, f"{name}")
    log_to_file(
        log_file,
        HEADER_FMT.format("total", "passed", "failed", "except", "ignored", "muted", "%", "test case name", "build"),
    )
    for configuration_line in engines:
        log_coniguration_line(log_file, configuration_line)
    sum_test_result = get_sum_test_result(engines)

    configuration_summary = TestResultExecutiveSummary(sum_test_result.passed, sum_test_result.get_not_passed_total())
    log_to_file(log_file, f"    Total     : {configuration_summary.total:6d}")
    log_to_file(log_file, f"    Passed    : {configuration_summary.passed:6d}")
    log_to_file(log_file, f"    Percentage: {configuration_summary.percentage:6.2f}")


def log_coniguration_line(log_file: TextIOWrapper, line: ConfigurationTestResult) -> None:
    """Log configuration line to a file."""
    total = line.get_total()
    if total != 0:
        percentage = float(line.test_result.passed) / float(total) * 100.0
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
                f"                                                                            xxx  Exception {exception}",
            )


def create_argument_parser() -> argparse.ArgumentParser:
    """Create custom argument parser."""
    parser = argparse.ArgumentParser(description="Retrieve status of a testbench running on TeamCity")
    parser.add_argument("-u", "--username", help="Username for accessing TeamCity.", dest="username")
    parser.add_argument(
        "-p",
        "--password",
        help="Password for accessing TeamCity.",
        dest="password",
    )
    parser.add_argument(
        "-i",
        "--interactive",
        help="Must be True to enable username/password via keyboard.",
        dest="interactive",
    )
    parser.add_argument(
        "--build_id",
        help="Build ID of the chain to analyze",
        dest="build_id",
    )

    return parser


def get_build_dependency_chain(
    build_id: str, username: str, password: str, filtered_list: Optional[list[FILTERED_LIST]] = None
) -> list:
    """
    Get dependency chain of all dependent builds for a given build ID from TeamCity.

    Parameters
    ----------
    build_id : str
        The build ID to get dependencies for.
    username : str
        TeamCity username.
    password : str
        TeamCity password.
    filtered_list : FILTERED_LIST, optional
        Optional filter to include only builds whose buildTypeId matches one of the values.

    Returns
    -------
    list
        List of dependent build IDs (snapshot dependencies).
    """
    url = f"{BASE_URL}/httpAuth/app/rest/builds?locator=defaultFilter:false,snapshotDependency(to:(id:{build_id})),count:1000&fields=build(id,buildTypeId)"
    response = get_request(url, username, password)
    if not text_in_xml_message(response.text):
        print(f"Could not retrieve dependencies for build ID {build_id}")
        return []
    xml_root = ET.fromstring(response.text)
    dependency_chain = []
    for dep in xml_root.findall("build"):
        dep_id = dep.attrib.get("id")
        build_type_id = dep.attrib.get("buildTypeId")
        if dep_id:
            if filter is not None:
                # Accept if build_type_id matches any value in filter
                filter_values = [item.value for item in filtered_list]
                if build_type_id in filter_values:
                    dependency_chain.append(dep_id)
            else:
                dependency_chain.append(dep_id)
    return dependency_chain


def get_xml_root_build_test_results(build_id: str, username: str, password: str) -> ET.Element:
    """
    Retrieve the XML root element containing build test results from TeamCity.

    Parameters
    ----------
    build_id : str
        The build ID to retrieve results for.
    username : str
        TeamCity username.
    password : str
        TeamCity password.

    Returns
    -------
    xml.etree.ElementTree.Element
        The XML root element containing the build test results.
    """
    url = f"{BASE_URL}/httpAuth/app/rest/builds/id:{build_id}"
    response = get_request(url, username, password)

    xml_root = ET.fromstring(response.text)
    return xml_root


def has_test_results(xml_root: ET.Element) -> bool:
    """
    Check if the XML root element has test results.

    Parameters
    ----------
    xml_root : ET.Element
        The XML root element to check.

    Returns
    -------
    bool
        True if test results are present, False otherwise.
    """
    test_occurrences = xml_root.find("testOccurrences")
    return test_occurrences is not None and int(test_occurrences.attrib.get("count", "0")) > 0


def get_build_test_results(xml_root: ET.Element) -> ConfigurationTestResult:
    """
    Fetch test results for a given build ID from a TeamCity XML root element.

    Parameters
    ----------
    xml_root : xml.etree.ElementTree.Element
        The XML root element representing a TeamCity build; expected to contain 'buildType' and 'testOccurrences' children.

    Returns
    -------
    ConfigurationTestResult
        An object containing the parsed test results for the build.

    Raises
    ------
    KeyError
        If required attributes are missing from the XML.
    Edge Cases
    ----------
    - If 'buildType' or 'testOccurrences' are missing, default values are used.
    - If test counts are missing, they default to zero.
    - If attributes like 'number' or 'status' are missing, fallback values are used.
    """
    build_nr = xml_root.attrib.get("number", "Unknown build number")
    status_text = xml_root.attrib.get("status", "No status available")

    # Build up config_name including parent project(s)
    config_name = "Unknown config"
    parent = "Unknown parent"
    build_type_elem = xml_root.find("buildType")
    if build_type_elem is not None:
        config_name = build_type_elem.attrib.get("name", "Unknown config")
        parent = build_type_elem.attrib.get("projectName", "Unknown parent")
    config_name = f"{parent} / {config_name}"

    passed = failed = ignored = muted = 0
    test_occurrences = xml_root.find("testOccurrences")

    if test_occurrences is not None:
        passed = int(test_occurrences.attrib.get("passed", "0"))
        failed = int(test_occurrences.attrib.get("failed", "0"))
        ignored = int(test_occurrences.attrib.get("ignored", "0"))
        muted = int(test_occurrences.attrib.get("muted", "0"))
    # TeamCity does not provide "exception" directly, so leave as 0

    return ConfigurationTestResult(
        name=config_name,
        build_nr=build_nr,
        passed=passed,
        failed=failed,
        ignored=ignored,
        muted=muted,
        status_text=status_text,
    )


def get_credentials(args: argparse.Namespace) -> tuple[str, str]:
    """
    Retrieve TeamCity credentials from command-line arguments or interactively.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments.

    Returns
    -------
    tuple[str, str]
        A tuple containing the username and password.
    """
    if args.interactive:
        interactive = args.interactive
    else:
        interactive = False
    if args.username:
        username = args.username
    else:
        if interactive:
            username = input("Username for TeamCity access:")
        else:
            print('No username on commandline. add "-i True" to enable interactive input')
            exit()
    if args.password:
        password = args.password
    else:
        if interactive:
            password = getpass.getpass()
        else:
            print('No password on commandline. add "-i True" to enable interactive input')
            exit()
    return username, password


if __name__ == "__main__":
    start_time = datetime.now()

    parser = create_argument_parser()
    args = parser.parse_args()
    username, password = get_credentials(args)

    build_id = args.build_id
    output_file = "teamcity_retrieve_release_engine_test_status.txt"
    if os.path.exists(output_file):
        os.remove(output_file)
    log_file = open(output_file, "a")

    print("Start: {start_time}\n")
    log_to_file(log_file, f"Start: {start_time}\n")

    print(f"Listing is written to: {output_file}")

    # 1. Get dependency chain of all dependent builds and Filter on relevant build IDs
    dependency_chain = get_build_dependency_chain(
        build_id,
        username,
        password,
        [
            FILTERED_LIST.DELFT3D_WINDOWS_TEST,
            FILTERED_LIST.DELFT3D_LINUX_TEST,
        ],
    )
    print(f"Dependency chain for build {build_id}: {dependency_chain}")

    # 2. Loop over the builds and retrieve the test results and write to file
    result_list = []
    for build_id in dependency_chain:
        xml_root = get_xml_root_build_test_results(build_id, username, password)
        if has_test_results(xml_root):
            result_list.append(get_build_test_results(xml_root))

    # 3. Write test results to file
    result_list.sort(key=lambda x: x.name)
    log_result_list(log_file, "DIMR Testbench Release", result_list)

    # 4. Write executive summary to file
    summary = TestResultSummary("All")
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
    log_to_file(log_file, f"End  : {datetime.now()}")
    log_to_file(log_file, "Ready")
    print(f"\nStart: {start_time}")
    print(f"End  : {datetime.now()}")
    print("Ready")
    sys.exit(tests_failed)
