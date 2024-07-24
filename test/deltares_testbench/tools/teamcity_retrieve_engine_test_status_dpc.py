import argparse
import getpass
import os
import sys
import xml.etree.ElementTree as ET
from datetime import datetime
from io import TextIOWrapper
from typing import List

import requests
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
HEADER_FMT = "{:>20s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s} {:>8s}  ---  {:24s} (#{:s})"


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

    def __init__(self, passed, failed) -> None:
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


class ConfigurationInfo(object):
    """A class to store configuration info."""

    def __init__(self, name: str, identifier: str) -> None:
        self.name = name
        self.identifier = identifier


class EngineCaseList(object):
    """A class to store configuration info."""

    def __init__(self, name: str, case_list: List[ConfigurationInfo]) -> None:
        self.engine_name = name
        self.list = case_list

    def has_cases(self) -> bool:
        """Check if the EngineCaseList has any cases.

        Returns
        -------
        bool
            True if there are cases, False otherwise.
        """
        if len(self.list) != 0:
            return True
        else:
            return False


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


class SubEngineTestResult(object):
    """A class to store sub engine test results info."""

    def __init__(self, name: str, engine_results: List[ConfigurationTestResult]) -> None:
        self.name = name
        self.engine_results = engine_results


class EngineTestResult(object):
    """A class to store engine test results info."""

    def __init__(
        self, name: str, engine_results: List[ConfigurationTestResult], sub_engine_results: List[SubEngineTestResult]
    ) -> None:
        self.name = name
        self.engine_results = engine_results
        self.sub_engine_results = sub_engine_results


class TreeResult(object):
    """A class to store the entire tree test results."""

    def __init__(self, name: str, engine_results: List[EngineTestResult]) -> None:
        self.name = name
        self.engine_results = engine_results

    def get_executive_summary(self) -> ExecutiveSummary:
        """Get executive summary of the test results.

        Returns
        -------
        ExecutiveSummary
        """
        summary_data = TestResultSummary("All")
        for engine_results in self.engine_results:
            for engine_result in engine_results.engine_results:
                summary_data.sum_passed += engine_result.test_result.passed
                summary_data.sum_failed += engine_result.test_result.failed
                summary_data.sum_exception += engine_result.test_result.exception
                summary_data.sum_ignored += engine_result.test_result.ignored
                summary_data.sum_muted += engine_result.test_result.muted

            for sub_engine_results in engine_results.sub_engine_results:
                for engine_result in sub_engine_results.engine_results:
                    summary_data.sum_passed += engine_result.test_result.passed
                    summary_data.sum_failed += engine_result.test_result.failed
                    summary_data.sum_exception += engine_result.test_result.exception
                    summary_data.sum_ignored += engine_result.test_result.ignored
                    summary_data.sum_muted += engine_result.test_result.muted

        summarydata_array = []
        summarydata_array.append(summary_data)
        return ExecutiveSummary(self.name, summarydata_array)


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


def get_engine_cases_from_url(url: str, username: str, password: str, given_build_config: str) -> EngineCaseList:
    """Get name and cases from XML node that is requested via the URL.

    Returns
    -------
    EngineCaseList
        Data object with name and a case list.
    """
    engine_req = get_request(url, username, password)
    if not text_in_xml_message(engine_req.text):
        return EngineCaseList("", [])
    xml_engine_root = ET.fromstring(engine_req.text)
    engine_name = xml_engine_root.attrib["name"]

    if "Experimental" in engine_name:
        print(f"\tSkip {engine_name}")
        return EngineCaseList("", [])
    else:
        print(f"\tRetrieving {engine_name}")

    case_list = get_configuration_info(xml_engine_root, given_build_config)

    return EngineCaseList(engine_name, case_list)


def get_test_result_list(
    log_file: TextIOWrapper, engine_cases: EngineCaseList, username: str, password: str
) -> List[ConfigurationTestResult]:
    """Get test results from the engine case list. Logs message to file in case of serious error.

    Returns
    -------
    List[ConfigurationTestResult]
        List with test results.
    """
    test_overview = []

    for case_info in engine_cases.list:
        identifier = case_info.identifier

        url = f"{BASE_URL}/httpAuth/app/rest/builds?locator=buildType:(id:{identifier}),defaultFilter:false,branch:<default>&count=1&fields=count,build(number,statistics,status,statusText,testOccurrences,agent,lastChange,tags(tag),pinned,revisions(revision))"
        case_req = get_request(url, username, password)
        if not text_in_xml_message(case_req.text):
            return 1

        xml_case_root = ET.fromstring(case_req.text)

        for build in xml_case_root.findall("build"):
            status_text = get_status_text(build)
            test_overview.append(create_configuration_test_result(build, case_info.name, status_text))

        if len(test_overview) == 0:
            log_to_file(log_file, f"ERROR: No data available for project {identifier}")
            continue

        i = test_overview.__len__() - 1
        if test_overview[i].test_result.failed != 0:
            cnt = int(build.find(TEST_OCCURRENCES).attrib["count"])
            href = build.find(TEST_OCCURRENCES).attrib["href"]
            url_1 = f"{BASE_URL}{href},count:{cnt}"
            test_occs_req = get_request(url_1, username, password)
            if not text_in_xml_message(test_occs_req.text):
                return 1
            xml_test_occs = ET.fromstring(test_occs_req.text)
            for t_occ in xml_test_occs.findall("testOccurrence"):
                if t_occ.attrib["status"] == "FAILURE":
                    href = t_occ.attrib["href"]
                    url_2 = f"{BASE_URL}{href}"
                    test_occ_req = get_request(url_2, username, password)
                    if not text_in_xml_message(test_occ_req.text):
                        return 1
                    xml_test_occ = ET.fromstring(test_occ_req.text)
                    txt = xml_test_occ.find("details").text

                    try:
                        if txt.find("Exception occurred") != -1 or txt.find("exception occurred") != -1:
                            if "muted" in t_occ.attrib:
                                test_overview[i].test_result.exception += 1
                                test_overview[i].test_result.muted_exception += 1
                                test_overview[i].exceptions.append("MUTED: " + xml_test_occ.attrib["name"])
                            else:
                                test_overview[i].test_result.failed -= 1
                                test_overview[i].test_result.exception += 1
                                test_overview[i].exceptions.append(xml_test_occ.attrib["name"])
                    except:
                        error_message = f"ERROR retrieving data from last build for {engine_cases.list[i].name} : {xml_test_occ.attrib['name']}."
                        print(error_message)
                        log_to_file(log_file, error_message)

    return test_overview


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


def get_configuration_info(xml_engine_root: ET.Element, given_build_config: str) -> List[ConfigurationInfo]:
    """Get configuration info from xml tree.

    Returns
    -------
    List[ConfigurationInfo]
        List with configurations.
    """
    result = []
    build_types = xml_engine_root.find("buildTypes")
    if build_types is not None:
        for build_type in build_types:
            build_id = build_type.attrib["id"]
            if not given_build_config or build_id in given_build_config:
                build_name = build_type.attrib["name"]
                if "Not in DIMR-Release" in build_name:
                    print(f"\tSkip {build_name}")
                    continue
                result.append(ConfigurationInfo(build_name, build_id))
    return result


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


def get_tree_entire_engine_test_results(
    log_file: TextIOWrapper, project_ids: str, given_build_config: str, username: str, password: str
) -> TreeResult:
    """Get entire tree test results.

    Returns
    -------
    TreeResult
        Entire tree test results.
    """
    project_url = f"{PROJECTS_URL}{project_ids}"

    try:
        project_response = get_request(project_url, username, password)
    except:
        print(f"Given URL does not exist: {project_url}")
        return 1

    if not text_in_xml_message(project_response.text):
        return 1
    project_text = ET.fromstring(project_response.text)
    tree_name = project_text.attrib["name"]

    engines = []
    for projects_node in project_text.findall("projects"):
        for project in projects_node:
            engine_name = project.attrib["name"]
            if project_is_archived(project):
                print(f"Skip archived {engine_name}")
                continue

            engines.append(ConfigurationInfo(engine_name, project.attrib["id"]))

    engine_results = []
    for engine in engines:
        print(f"Retrieving {engine.name}")
        url = f"{PROJECTS_URL}{engine.identifier}"
        engine_req = get_request(url, username, password)
        if not text_in_xml_message(engine_req.text):
            return 1

        test_results = []
        sub_test_result = []

        engine_cases = get_engine_cases_from_url(url, username, password, given_build_config)
        if engine_cases.has_cases():
            test_results = get_test_result_list(log_file, engine_cases, username, password)

        xml_engine_root = ET.fromstring(engine_req.text)
        for projects_node in xml_engine_root.findall("projects"):
            for project in projects_node:
                project_info = ConfigurationInfo(project.attrib["name"], project.attrib["id"])

                url_3 = f"{PROJECTS_URL}{project_info.identifier}"
                level_req = get_request(url_3, username, password)
                if not text_in_xml_message(level_req.text):
                    return 1

                sub_engine_cases = get_engine_cases_from_url(url_3, username, password, given_build_config)
                if sub_engine_cases.has_cases():
                    sub_test_result.append(
                        SubEngineTestResult(
                            project_info.name, get_test_result_list(log_file, sub_engine_cases, username, password)
                        )
                    )

        if engine_cases.has_cases() or sub_engine_cases.has_cases():
            engine_results.append(EngineTestResult(engine.name, test_results, sub_test_result))
    return TreeResult(tree_name, engine_results)

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


def log_tree(log_file: TextIOWrapper, tree_result: TreeResult) -> None:
    """Log project tree to a file."""
    log_to_file(log_file, f"{tree_result.name}")

    for engine_result in tree_result.engine_results:
        log_to_file(log_file, f"    {engine_result.name}")
        if len(engine_result.sub_engine_results) != 0:
            for result in engine_result.sub_engine_results:
                log_engine(log_file, result.name, result.engine_results)
        else:
            log_engine(log_file, engine_result.name, engine_result.engine_results)


def log_engine(log_file: TextIOWrapper, name: str, engines: List[ConfigurationTestResult]) -> None:
    """Log engine list to a file."""
    log_to_file(log_file, f"        {name}")
    log_to_file(
        log_file,
        HEADER_FMT.format("total", "passed", "failed", "except", "ignored", "muted", "%", "test case name", "build"),
    )
    for configuration_line in engines:
        log_coniguration_line(log_file, configuration_line)
    sum_test_result = get_sum_test_result(engines)

    configuration_summary = TestResultExecutiveSummary(sum_test_result.passed, sum_test_result.get_not_passed_total())
    log_to_file(log_file, f"            Total     : {configuration_summary.total:6d}")
    log_to_file(log_file, f"            Passed    : {configuration_summary.passed:6d}")
    log_to_file(log_file, f"            Percentage: {configuration_summary.percentage:6.2f}")


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
            "{:20d} {:8d} {:8d} {:8d} {:8d} {:8d} {:8.2f}  ---  {:24s} (#{:s})".format(
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

    parser.add_argument(
        "-t",
        "--tbroot",
        help="ProjetcId of the testbench root for which the status is needed.",
        dest="tbroot",
        required=True,
    )
    parser.add_argument("-o", "--output", help="Output filename.", dest="out_put")
    parser.add_argument("-b", "--build_config", help="Build configuration ID", dest="build_config")
    parser.add_argument("-u", "--username", help="Username for accessing TeamCity.", dest="username")
    parser.add_argument(
        "-p",
        "--password",
        help="Password belonging to username for accessing TeamCity.",
        dest="password",
    )
    parser.add_argument(
        "-i",
        "--interactive",
        help="Must be True to enable username/password via keyboard.",
        dest="interactive",
    )
    parser.add_argument(
        "-e",
        "--engines",
        help="Specify extra components to be summarized, between double quotes and separated by a comma",
        dest="engines",
    )

    return parser


def get_number_of_failed_tests(tree_result_overview: TreeResult) -> int:
    """Get the number of total tests failed."""
    total_failed_tests = 0
    for engine_result in tree_result_overview.engine_results:
        for result in engine_result.engine_results:
            total_failed_tests += result.get_not_passed_total()
        for sub_engine_result in engine_result.sub_engine_results:
            for result in sub_engine_result.engine_results:
                total_failed_tests += result.get_not_passed_total()

    return total_failed_tests


if __name__ == "__main__":
    start_time = datetime.now()

    parser = create_argument_parser()
    args = parser.parse_args()

    out_put = "teamcity_retrieve_engine_test_status.txt"
    given_build_config = []

    if args.tbroot:
        tbroot = args.tbroot
    if args.build_config:
        bconfig = args.build_config
        given_build_config = bconfig.split(",")
    if args.out_put:
        out_put = args.out_put
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
    if args.engines:
        engines = args.engines
    else:
        engines = None
    if os.path.exists(out_put):
        os.remove(out_put)
    log_file = open(out_put, "a")

    print("Start: {start_time}\n")
    log_to_file(log_file, f"Start: {start_time}\n")

    print(f"Listing is written to: {out_put}")

    tree_result_overview = get_tree_entire_engine_test_results(log_file, tbroot, given_build_config, username, password)
    log_tree(log_file, tree_result_overview)
    executive_summary = tree_result_overview.get_executive_summary()
    log_executive_summary(log_file, executive_summary)

    tests_failed = get_number_of_failed_tests(tree_result_overview)
    print(f"Total test failed: {tests_failed}")

    log_to_file(log_file, f"\nStart: {start_time}")
    log_to_file(log_file, f"End  : {datetime.now()}")
    log_to_file(log_file, "Ready")
    print(f"\nStart: {start_time}")
    print(f"End  : {datetime.now()}")
    print("Ready")
    sys.exit(tests_failed)
