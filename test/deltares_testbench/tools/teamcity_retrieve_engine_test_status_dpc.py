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
summarydata_array = []
global log_file
global engine_statistics
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


class Data(object):
    """A class to store data for test results."""

    def __init__(self, name, passed, failed) -> None:
        self.name = name
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


def lprint(*args: str) -> None:
    """
    Write to a log file.

    Args:
        *args: Variable number of arguments to be written to the log file.
    """
    global log_file
    log_file.write(" ".join(map(str, args)) + "\n")


def report_cases(url, given_build_config, username, password, buildname):
    global _enginge_statistics
    global summarydata_array

    engine_req = get_request(url, username, password)
    if not text_in_xml_message(engine_req.text):
        return 1
    xml_engine_root = ET.fromstring(engine_req.text)
    engine_name = xml_engine_root.attrib["name"]
    case_info_list = get_configuration_info(xml_engine_root, given_build_config)

    if len(case_info_list) != 0:
        print("        %s" % engine_name)
        lprint("        %s" % engine_name)
        lprint(
            "               total   passed   failed   except  ignored    muted        %  --- test case name            (# build)"
        )
    print_case_list(case_info_list, buildname, engine_name)


def print_case_list(case_info_list: List[ConfigurationInfo], buildname: str, engine_name: str):
    _sum_passed = 0
    _sum_failed = 0
    _sum_exception = 0
    _sum_ignored = 0
    _sum_muted = 0
    build_nr = []
    passed = []
    failed = []
    exception = []
    ignored = []
    muted = []
    muted_exception = []

    sum_passed_subtotal = 0
    not_passed_subtotal = 0

    for case_info in case_info_list:
        identifier = case_info.identifier
        computation_name = []
        url = f"{BASE_URL}/httpAuth/app/rest/builds?locator=buildType:(id:{identifier}),defaultFilter:false,branch:<default>&count=1&fields=count,build(number,statistics,status,statusText,testOccurrences,agent,lastChange,tags(tag),pinned,revisions(revision))"

        case_req = get_request(url, username, password)
        if not text_in_xml_message(case_req.text):
            return 1

        file_name = "TMPdownload_teamcity_retrieve/%s.xml" % identifier
        with open(file_name, "wb") as out_file:
            out_file.write(case_req.content)

        xml_case_root = ET.fromstring(case_req.text)

        for build in xml_case_root.findall("build"):
            bnr = build.attrib["number"]
            build_nr.append(bnr)
            status_text = ""

            if build.find(TEST_OCCURRENCES) is not None:
                passed.append(get_number_of_tests(build, "passed"))
                failed.append(get_number_of_tests(build, "failed"))
                ignored.append(get_number_of_tests(build, "ignored"))
                muted.append(get_number_of_tests(build, "muted"))
            else:
                passed.append(0)
                failed.append(0)
                ignored.append(0)
                muted.append(0)
                status = build.find("statusText")
                if status is not None:
                    status_text = status.text
                else:
                    status_text = "Build failed!"

        exception.append(0)  # first guess, initially there are no exceptions
        # first guess, initially there are no exceptions for a muted test
        muted_exception.append(0)

        if len(failed) == 0:
            lprint("ERROR: No data available for project %s" % identifier)
            continue

        i = build_nr.__len__() - 1
        a = 0
        if failed[i] != 0:
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
                                exception[i] += 1
                                muted_exception[i] += 1
                                computation_name.append("MUTED: " + xml_test_occ.attrib["name"])
                            else:
                                failed[i] -= 1
                                exception[i] += 1
                                computation_name.append(xml_test_occ.attrib["name"])
                    except:
                        error_message = f"ERROR retrieving data from last build for {case_info_list[i].name} : {xml_test_occ.attrib["name"]}."
                        print(error_message)
                        lprint(error_message)

        total = passed[i] + failed[i] + exception[i] + ignored[i] + muted[i] - muted_exception[i]
        if total != 0:
            a = float(passed[i]) / float(total) * 100.0
        else:
            a = 0

        if total > 0:
            lprint(
                "            %8d %8d %8d %8d %8d %8d %8.2f  ---  %-24s (#%s)"
                % (
                    total,
                    passed[i],
                    failed[i],
                    exception[i],
                    ignored[i],
                    muted[i],
                    a,
                    case_info_list[i].name,
                    build_nr[i],
                )
            )
        else:
            lprint(
                "                   x        x        x        x        x        x        x  ---  %-24s (#%s)"
                % (case_info_list[i].name, build_nr[i])
            )
            lprint(
                "                                                                            xxx  %s" % (status_text)
            )

        if exception[i] != 0:
            for j in range(0, computation_name.__len__()):
                lprint(
                    "                                                                            xxx  Exception %s"
                    % computation_name[j]
                )

        _sum_passed += passed[i]
        _sum_failed += failed[i]
        _sum_exception += exception[i]
        _sum_ignored += ignored[i]
        _sum_muted += muted[i]
        sum_passed_subtotal += passed[i]
        not_passed_subtotal += failed[i] + exception[i] + ignored[i] + muted[i]

    for summary in summarydata_array:
        if (summary.name in buildname) or summary.name == "All":
            summary.sum_passed += _sum_passed
            summary.sum_failed += _sum_failed
            summary.sum_exception += _sum_exception
            summary.sum_ignored += _sum_ignored
            summary.sum_muted += _sum_muted

    if len(case_info_list) != 0:
        engine_statistics.append(Data(engine_name, sum_passed_subtotal, not_passed_subtotal))

        i = len(engine_statistics) - 1
        lprint("            Total     : %6d" % engine_statistics[i].total)
        lprint("            Passed    : %6d" % engine_statistics[i].passed)
        lprint("            Percentage: %6.2f" % engine_statistics[i].percentage)


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


def retrieve_engine_test_status(project_id, given_build_config, username, password, engines):
    global engine_statistics
    global summarydata_array

    summarydata_array.append(SummaryData("All"))
    if engines is not None:
        for engine in engines.split(","):
            summarydata_array.append(SummaryData(engine))

    project_url = PROJECTS_URL % project_id

    try:
        project_response = get_request(project_url, username, password)
    except:
        print("Given URL does not exist: %s" % project_url)
        return 1

    if not text_in_xml_message(project_response.text):
        return 1
    project_text = ET.fromstring(project_response.text)

    print("")
    print("%s" % project_text.attrib["name"])
    lprint("%s" % project_text.attrib["name"])

    engine_name = []
    engine_id = []
    for projects in project_text.findall("projects"):
        for project in projects:
            engine_id.append(project.attrib["id"])
            engine_name.append(project.attrib["name"])

    engine_statistics = []

    for engine in engine_id:
        project_id = []
        project_name = []

        url = PROJECTS_URL % engine

        engine_req = get_request(url, username, password)
        if not text_in_xml_message(engine_req.text):
            return 1
        xml_engine_root = ET.fromstring(engine_req.text)
        print("    %s" % xml_engine_root.attrib["name"])
        lprint("    %s" % xml_engine_root.attrib["name"])

        for projects in xml_engine_root.findall("projects"):
            for project in projects:
                project_id.append(project.attrib["id"])
                project_name.append(project.attrib["name"])

                url_3 = PROJECTS_URL % project.attrib["id"]
                level_req = get_request(url, username, password)
                if not text_in_xml_message(level_req.text):
                    return 1
                report_cases(
                    url_3,
                    given_build_config,
                    username,
                    password,
                    project.attrib["name"],
                )

        report_cases(
            url,
            given_build_config,
            username,
            password,
            engine_name[engine_id.index(engine)],
        )

    lprint("\nTestbench root: %s" % project_text.attrib["name"])
    for summary in summarydata_array:
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

    retrieve_engine_test_status(tbroot, given_build_config, username, password, engines)

    if os.path.exists("TMPdownload_teamcity_retrieve"):
        shutil.rmtree("TMPdownload_teamcity_retrieve")

    lprint("\nStart: %s" % start_time)
    lprint("End  : %s" % datetime.now())
    lprint("Ready")
    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now())
    print("Ready")
