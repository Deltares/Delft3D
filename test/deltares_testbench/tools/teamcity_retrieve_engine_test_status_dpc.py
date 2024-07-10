import argparse
import getpass
import os
import shutil
import xml.etree.ElementTree as ET
from datetime import datetime
from typing import List

import requests
from requests.auth import HTTPBasicAuth

"""
Author: Jan Mooiman
E-Mail: jan.mooiman@deltares.nl
Date  : 10 sep 2017

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
global log_file
TEXT_NOT_IN_XML_MESSAGE = "Text is not in XML format: %s"
BASE_URL = "https://dpcbuild.deltares.nl"
REST_API_URL = f"{BASE_URL}/httpAuth/app/rest"
PROJECTS_URL = f"{REST_API_URL}/projects/id:%s"
TEST_OCCURRENCES = "./testOccurrences"


class SummaryData(object):
    """A class to store summary data for test results."""

    def __init__(self, name) -> None:
        self.name = name
        self.sum_passed = 0
        self.sum_failed = 0
        self.sum_exception = 0
        self.sum_ignored = 0
        self.sum_muted = 0


class ExecutiveSummary(object):
    """A class to store executive summary data for test results."""

    def __init__(self, name: str, summary: list[SummaryData]) -> None:
        self.name = name
        self.summary = summary


class ConfigurationSummary(object):
    """A class to store data for test results."""

    def __init__(self, passed, failed) -> None:
        self.passed = passed
        self.failed = failed
        self.total = passed + failed
        a = 0.0
        if self.total > 0:
            a = float(self.passed) / float(self.total) * 100.0
        self.percentage = a


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
        return self.passed + self.failed + self.exception + self.ignored + self.muted - self.muted_exception

    def get_not_passed_total(self) -> int:
        return self.failed + self.exception + self.ignored + self.muted


class ConfigurationTestResult(object):
    """A class to store configuration test results info."""

    def __init__(
        self,
        name: str,
        build_nr: int,
        passed: int,
        failed: int,
        ignored: int,
        muted: int,
        exception: int,
        muted_exception: int,
        status_text: str,
        exception_name: str,
    ) -> None:
        self.name = name
        self.build_nr = build_nr
        self.test_result = TestResult(passed, failed, ignored, muted, exception, muted_exception)
        self.passed = passed
        self.failed = failed
        self.ignored = ignored
        self.muted = muted
        self.exception = exception
        self.muted_exception = muted_exception
        self.status_text = status_text
        self.exception_name = exception_name

    def get_total(self) -> int:
        return self.test_result.get_total()

    def get_not_passed_total(self) -> int:
        return self.test_result.get_not_passed_total()


class SubEngineTestResult(object):
    """A class to store configuration test results info."""

    def __init__(self, name: str, engine_results: List[ConfigurationTestResult]) -> None:
        self.name = name
        self.engine_results = engine_results


class EngineTestResult(object):
    """A class to store configuration test results info."""

    def __init__(
        self, name: str, engine_results: List[ConfigurationTestResult], sub_engine_results: List[SubEngineTestResult]
    ) -> None:
        self.name = name
        self.engine_results = engine_results
        self.sub_engine_results = sub_engine_results


class TreeResult(object):
    """A class to store configuration test results info."""

    def __init__(self, name: str, engine_results: List[EngineTestResult]) -> None:
        self.name = name
        self.engine_results = engine_results

    def get_executive_summary(self) -> ExecutiveSummary:
        summary_data = SummaryData("All")
        for engine_results in self.engine_results:
            for engine_result in engine_results.engine_results:
                summary_data.sum_passed += engine_result.passed
                summary_data.sum_failed += engine_result.failed
                summary_data.sum_exception += engine_result.exception
                summary_data.sum_ignored += engine_result.ignored
                summary_data.sum_muted += engine_result.muted

            for sub_engine_results in engine_results.sub_engine_results:
                for engine_result in sub_engine_results.engine_results:
                    summary_data.sum_passed += engine_result.passed
                    summary_data.sum_failed += engine_result.failed
                    summary_data.sum_exception += engine_result.exception
                    summary_data.sum_ignored += engine_result.ignored
                    summary_data.sum_muted += engine_result.muted

        summarydata_array = []
        summarydata_array.append(summary_data)
        return ExecutiveSummary(self.name, summarydata_array)


def get_sum_test_result(test_overview: List[ConfigurationTestResult]) -> TestResult:
    sum_passed = 0
    sum_failed = 0
    sum_exception = 0
    sum_ignored = 0
    sum_muted = 0
    sum_muted_exception = 0
    for test in test_overview:
        sum_passed += test.passed
        sum_failed += test.failed
        sum_ignored += test.ignored
        sum_muted += test.muted
        sum_exception += test.exception
        sum_muted_exception += test.muted_exception
    return TestResult(sum_passed, sum_failed, sum_ignored, sum_muted, sum_exception, sum_muted_exception)


def lprint(*args: str) -> None:
    """
    Write to a log file.

    Args:
        *args: Variable number of arguments to be written to the log file.
    """
    global log_file
    log_file.write(" ".join(map(str, args)) + "\n")


def get_engine_cases_from_url(url, username, password, given_build_config) -> EngineCaseList:
    engine_req = get_request(url, username, password)
    if not text_in_xml_message(engine_req.text):
        return EngineCaseList("", [ConfigurationInfo("", "")])
    xml_engine_root = ET.fromstring(engine_req.text)
    engine_name = xml_engine_root.attrib["name"]
    case_list = get_configuration_info(xml_engine_root, given_build_config)
    return EngineCaseList(engine_name, case_list)


def get_test_result_list(engine_cases: EngineCaseList) -> List[ConfigurationTestResult]:
    test_overview = []

    for case_info in engine_cases.list:
        identifier = case_info.identifier
        url = f"{BASE_URL}/httpAuth/app/rest/builds?locator=buildType:(id:{identifier}),defaultFilter:false,branch:<default>&count=1&fields=count,build(number,statistics,status,statusText,testOccurrences,agent,lastChange,tags(tag),pinned,revisions(revision))"

        case_req = get_request(url, username, password)
        if not text_in_xml_message(case_req.text):
            return 1

        file_name = "TMPdownload_teamcity_retrieve/%s.xml" % identifier
        with open(file_name, "wb") as out_file:
            out_file.write(case_req.content)

        xml_case_root = ET.fromstring(case_req.text)

        for build in xml_case_root.findall("build"):
            status_text = ""
            if build.find(TEST_OCCURRENCES) is None:
                status_text = get_status_text_from_node(build)
            test_overview.append(create_configuration_test_result(build, case_info.name, status_text))

        if len(test_overview) == 0:
            lprint("ERROR: No data available for project %s" % identifier)
            continue

        i = test_overview.__len__() - 1
        if test_overview[i].failed != 0:
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
                                test_overview[i].exception += 1
                                test_overview[i].muted_exception += 1
                                test_overview[i].exception_name = "MUTED: " + xml_test_occ.attrib["name"]
                            else:
                                test_overview[i].failed -= 1
                                test_overview[i].exception += 1
                                test_overview[i].exception_name = xml_test_occ.attrib["name"]
                    except:
                        error_message = f"ERROR retrieving data from last build for {engine_cases.list[i].name} : {xml_test_occ.attrib["name"]}."
                        print(error_message)
                        lprint(error_message)

    return test_overview


def get_status_text_from_node(build: ET.Element) -> str:
    """
    Get status text from xml node.

    Parameters
    ----------
    build : ET.Element
        The XML node representing node.

    Returns
    -------
        str: the status text.
    """
    status = build.find("statusText")
    if status is not None:
        return str(status.text)
    else:
        return "Build failed!"


def get_number_of_tests(build: ET.Element, test_result: str) -> int:
    """
    Get number of tests from xml node.

    Parameters
    ----------
    build : ET.Element
        The XML node representing the build.
    test_result : str
        The test result to match.

    Returns
    -------
        int: number of tests that match the test result.
    """
    if build.find(TEST_OCCURRENCES) is not None:
        if test_result in build.find(TEST_OCCURRENCES).attrib:
            return int(build.find(TEST_OCCURRENCES).attrib[test_result])

    return 0


def create_configuration_test_result(build: ET.Element, name: str, status_text: str) -> ConfigurationTestResult:
    build_nr = build.attrib["number"]
    if build.find(TEST_OCCURRENCES) is not None:
        passed = get_number_of_tests(build, "passed")
        failed = get_number_of_tests(build, "failed")
        ignored = get_number_of_tests(build, "ignored")
        muted = get_number_of_tests(build, "muted")
    else:
        passed = 0
        failed = 0
        ignored = 0
        muted = 0
    return ConfigurationTestResult(name, int(build_nr), passed, failed, ignored, muted, 0, 0, status_text, "")


def get_configuration_info(xml_engine_root, given_build_config) -> List[ConfigurationInfo]:
    """
    Get configuration info from xml tree.

    Returns
    -------
        List[ConfigurationInfo]: List with configurations.
    """
    result = []
    build_types = xml_engine_root.find("buildTypes")
    if build_types is not None:
        for build_type in build_types:
            build_id = build_type.attrib["id"]
            if not given_build_config or build_id in given_build_config:
                result.append(ConfigurationInfo(build_type.attrib["name"], build_id))
    return result


def get_request(url: str, username: str, password: str) -> requests.Response:
    """
    Send an HTTP GET request with authentication.

    Args:
        url (str): The URL to send the request to.
        username (str): The username for authentication.
        password (str): The password for authentication.

    Returns
    -------
        requests.Response: The response object from the request.
    """
    return requests.get(url=url, auth=HTTPBasicAuth(username, password), stream=True, verify=True)


def text_in_xml_message(text: str) -> bool:
    """
    Check if HTTP GET response has text.

    Args:
        text (str): The HTTP GET response to check.

    Returns
    -------
        bool: true or false depending on the text.
    """
    try:
        ET.fromstring(text)
        return True
    except:
        print("TEXT_NOT_IN_XML_MESSAGE" % text)
        return False


def retrieve_engine_test_status(project_ids, given_build_config, username, password, arg_engines) -> TreeResult:
    project_url = PROJECTS_URL % project_ids

    try:
        project_response = get_request(project_url, username, password)
    except:
        print("Given URL does not exist: %s" % project_url)
        return 1

    if not text_in_xml_message(project_response.text):
        return 1
    project_text = ET.fromstring(project_response.text)
    tree_name = project_text.attrib["name"]

    engines = []
    for projects_node in project_text.findall("projects"):
        for project in projects_node:
            engines.append(ConfigurationInfo(project.attrib["name"], project.attrib["id"]))

    engine_results = []
    for engine in engines:
        url = PROJECTS_URL % engine.identifier

        engine_req = get_request(url, username, password)
        if not text_in_xml_message(engine_req.text):
            return 1

        test_results = []
        sub_test_result = []

        engine_cases = get_engine_cases_from_url(url, username, password, given_build_config)
        if engine_cases.has_cases():
            test_results = get_test_result_list(engine_cases)

        xml_engine_root = ET.fromstring(engine_req.text)
        for projects_node in xml_engine_root.findall("projects"):
            for project in projects_node:
                project_info = ConfigurationInfo(project.attrib["name"], project.attrib["id"])

                url_3 = PROJECTS_URL % project_info.identifier
                level_req = get_request(url_3, username, password)
                if not text_in_xml_message(level_req.text):
                    return 1
                sub_engine_cases = get_engine_cases_from_url(url_3, username, password, given_build_config)
                if sub_engine_cases.has_cases():
                    sub_test_result.append(
                        SubEngineTestResult(project_info.name, get_test_result_list(sub_engine_cases))
                    )

        if engine_cases.has_cases() or sub_engine_cases.has_cases():
            engine_results.append(EngineTestResult(engine.name, test_results, sub_test_result))
    return TreeResult(tree_name, engine_results)


def print_executive_summary(summarydata: ExecutiveSummary) -> None:
    lprint("\nTestbench root: %s" % summarydata.name)
    for summary in summarydata.summary:
        total = (
            summary.sum_passed + summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        )
        not_passed = summary.sum_failed + summary.sum_exception + summary.sum_ignored + summary.sum_muted
        a = 0.0
        if total > 0:
            a = float(summary.sum_passed) / float(total) * 100.0

        lprint("\nSummary: %s" % summary.name)
        lprint("Total tests   : %6d" % (total))
        lprint("    Passed    : %6d" % summary.sum_passed)
        lprint("    Not passed: %6d" % not_passed)
        lprint("    Failed    : %6d" % summary.sum_failed)
        lprint("    Exception : %6d" % summary.sum_exception)
        lprint("    Ignored   : %6d" % summary.sum_ignored)
        lprint("    Muted     : %6d" % summary.sum_muted)
        lprint("    Percentage: %6.2f" % float(a))


def print_tree(tree_result: TreeResult) -> None:
    print("")
    print("%s" % tree_result.name)
    lprint("%s" % tree_result.name)

    for engine_result in tree_result.engine_results:
        print("    %s" % engine_result.name)
        lprint("    %s" % engine_result.name)
        if len(engine_result.sub_engine_results) != 0:
            for result in engine_result.sub_engine_results:
                print_engine(result.name, result.engine_results)
        else:
            print_engine(engine_result.name, engine_result.engine_results)


def print_engine(name: str, engines: List[ConfigurationTestResult]):
    print("        %s" % name)
    lprint("        %s" % name)
    lprint(
        "               total   passed   failed   except  ignored    muted        %  --- test case name            (# build)"
    )
    for configuration_line in engines:
        print_coniguration_line(configuration_line)
    sum_test_result = get_sum_test_result(engines)

    configuration_summary = ConfigurationSummary(sum_test_result.passed, sum_test_result.get_not_passed_total())
    lprint("            Total     : %6d" % configuration_summary.total)
    lprint("            Passed    : %6d" % configuration_summary.passed)
    lprint("            Percentage: %6.2f" % configuration_summary.percentage)


def print_coniguration_line(line: ConfigurationTestResult) -> None:
    total = line.get_total()
    if total != 0:
        a = float(line.passed) / float(total) * 100.0
    else:
        a = 0
    if total > 0:
        lprint(
            "            %8d %8d %8d %8d %8d %8d %8.2f  ---  %-24s (#%s)"
            % (
                total,
                line.passed,
                line.failed,
                line.exception,
                line.ignored,
                line.muted,
                a,
                line.name,
                line.build_nr,
            )
        )
    else:
        lprint(
            "                   x        x        x        x        x        x        x  ---  %-24s (#%s)"
            % (line.name, line.build_nr)
        )
        lprint("                                                                            xxx  %s" % line.status_text)

    if line.exception != 0:
        lprint(
            "                                                                            xxx  Exception %s"
            % line.exception_name
        )


def create_argument_parser() -> argparse.ArgumentParser:
    """Create custom argument parser."""
    parser = argparse.ArgumentParser(description="Retrieve status of a testbench running on TeamCity")

    parser.add_argument(
        "-t",
        "--tbroot",
        help="ProjetcId of the testbench root for which the status is needed.",
        dest="tbroot",
        required="true",
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


if __name__ == "__main__":
    start_time = datetime.now()

    if os.path.exists("TMPdownload_teamcity_retrieve"):
        shutil.rmtree("TMPdownload_teamcity_retrieve")
    os.mkdir("TMPdownload_teamcity_retrieve")

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

    print("Start: %s\n" % start_time)
    lprint("Start: %s\n" % start_time)

    print("Listing is written to: %s" % out_put)

    tree_result_overview = retrieve_engine_test_status(tbroot, given_build_config, username, password, engines)
    print_tree(tree_result_overview)
    executive_summary = tree_result_overview.get_executive_summary()
    print_executive_summary(executive_summary)

    if os.path.exists("TMPdownload_teamcity_retrieve"):
        shutil.rmtree("TMPdownload_teamcity_retrieve")

    lprint("\nStart: %s" % start_time)
    lprint("End  : %s" % datetime.now())
    lprint("Ready")
    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now())
    print("Ready")
