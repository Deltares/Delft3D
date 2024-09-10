from typing import Dict
import os

from settings.general_settings import RELATIVE_PATH_TO_OUTPUT_FOLDER
from settings.email_settings import RELATIVE_PATH_TO_EMAIL_TEMPLATE, LOWER_BOUND_PERCENTAGE_SUCCESSFUL_TESTS
from helpers.TestbankResultParser import TestbankResultParser
from settings.teamcity_settings import KERNELS, TESTCASE_GROUPS


class EmailHelper(object):
    """ Class responsible for preparing the weekly DIMR release email. """

    def __init__(self, dimr_version: str, svn_revision: str, kernel_versions: Dict[str, str],
                 current_parser: TestbankResultParser, previous_parser: TestbankResultParser):
        """
        Creates a new instance of EmailHelper.

        Args:
            dimr_version (str): The latest DIMR version.
            svn_revision (str): The SVN revision number for the latest DIMR set.
            kernel_versions (str): A dictionary mapping kernel names to their version.
            current_parser (TestbankResultParser): A parser for the latest test bench results.
            previous_parser (TestbankResultParser): A parser for the previous test bench results.
        """
        self.__dimr_version = dimr_version
        self.__svn_revision = svn_revision
        self.__kernel_versions = kernel_versions
        self.__current_parser = current_parser
        self.__previous_parser = previous_parser
        self.__template = ""

    def generate_template(self) -> None:
        """ Generate a template email for the latest DIMR release that can be copy/pasted into Outlook. """
        self.__load_template()
        self.__insert_summary_table_header()
        self.__insert_summary_table()
        self.__insert_crashing_testcases_table()
        self.__save_template()

    def __load_template(self) -> None:
        """ Loads the template into memory. """
        current_dir = os.path.dirname(__file__)
        path_to_email_template = os.path.join(current_dir, RELATIVE_PATH_TO_EMAIL_TEMPLATE)

        with open(path_to_email_template, 'r') as file:
            self.__template = file.read()

    def __insert_summary_table_header(self) -> None:
        """ Inserts the summary table header into the template. """
        html = self.__template

        html = html.replace("@@@DIMR_VERSION@@@", self.__dimr_version)
        html = html.replace("@@@SVN_REVISION@@@", self.__svn_revision)
        html = html.replace("@@@LINK_TO_PUBLIC_WIKI@@@", self.__generate_wiki_link())

        self.__template = html

    def __generate_wiki_link(self) -> str:
        link = f"https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+{self.__dimr_version}"
        return f'<a href="{link}">{link}</a>'

    def __insert_summary_table(self) -> None:
        """ Inserts the summary table into the tempalte. """
        html = self.__generate_summary_table_html()
        self.__template = self.__template.replace("@@@SUMMARY_TABLE_BODY@@@", html)

    def __generate_summary_table_html(self) -> str:
        """ Dynamically generates the summary table based on the kernels expected to be present. """
        html = ""

        # Insert the kernel information
        for kernel, revision in self.__kernel_versions.items():
            if kernel == "DIMRset_ver":
                continue

            kernel_name = self.__get_email_friendly_kernel_name(kernel)

            html += "<tr>"
            html += f"<td>{kernel_name}</td>"
            html += f"<td>{revision}</td>"
            html += "<td></td>"
            html += "</tr>"

        # Insert the passing test percentage info
        html += "<tr>"
        html += "<td></td>"
        passing_test_percentage = self.__current_parser.get_percentage_total_passing()
        if float(passing_test_percentage) < LOWER_BOUND_PERCENTAGE_SUCCESSFUL_TESTS:
            html += f'<td><span class="fail">{passing_test_percentage}%</span></td>'
        else:
            html += f'<td><span class="success">{passing_test_percentage}%</span></td>'

        previous_percentage_passing_tests = self.__previous_parser.get_percentage_total_passing()
        html += "<td><table><tr><td><br />"
        if float(previous_percentage_passing_tests) < LOWER_BOUND_PERCENTAGE_SUCCESSFUL_TESTS:
            html += f'Green testbank was (<span class="fail">{previous_percentage_passing_tests}%</span>)'
        else:
            html += f'Green testbank was (<span class="success">{previous_percentage_passing_tests}%</span>)'
        html += "<br />"
        html += f"Total tests: {self.__current_parser.get_total_tests()} was ({self.__previous_parser.get_total_tests()})"
        html += "<br />"
        html += f"Passed&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: {self.__current_parser.get_total_passing()} was ({self.__previous_parser.get_total_passing()})"
        
        html += "</tr></table>"
        html += "</td>"
        html += "</tr>"

        total_exceptions = self.__current_parser.get_total_exceptions()
        previous_total_exceptions = self.__previous_parser.get_total_exceptions()
        html += "<tr>"
        html += "<td></td>"
        if int(total_exceptions) > 0:
            html += f'<td><span class="fail">{total_exceptions}</span></td>'
        else:
            html += f'<td><span class="success">{total_exceptions}</span></td>'
        if int(previous_total_exceptions) > 0:
            html += f'<td>Crashes in testbank (was <span class="fail">{previous_total_exceptions}</span>)</td>'
        else:
            html += f'<td>Crashes in testbank (was <span class="success">{previous_total_exceptions}</span>)</td>'
        html += "</tr>"

        return html

    def __get_email_friendly_kernel_name(self, kernel) -> str:
        """ Gets the email friendly kernel name for a given kernel. """
        kernel_name = ""
        for KERNEL in KERNELS:
            if KERNEL.name_for_extracting_revision == kernel:
                kernel_name = KERNEL.name_for_email
        return kernel_name

    def __save_template(self) -> None:
        """ Saves the email template in the output folder. """
        current_dir = os.path.dirname(__file__)
        path_to_output_folder = os.path.join(current_dir, RELATIVE_PATH_TO_OUTPUT_FOLDER)
        path_to_email_template = os.path.join(path_to_output_folder, f"DIMRset_{self.__dimr_version}.html")
        
        if not os.path.exists(path_to_output_folder):
            os.makedirs(path_to_output_folder)

        with open(path_to_email_template, 'w+') as file:
            file.write(self.__template)

    def __insert_crashing_testcases_table(self):
        """ Inserts the crashing testcases table into the template. """
        html = self.__generate_crashing_testcases_table_html()
        self.__template = self.__template.replace("@@@CRASHING_TESTCASES_TABLE_BODY@@@", html)

    def __generate_crashing_testcases_table_html(self) -> str:
        """ Dynamically generates the crashing testcases table based on the testcases expected to be present. """
        html = ""

        for TESTCASE_GROUP in TESTCASE_GROUPS:
            exceptions = self.__current_parser.get_exceptions_for_testcase_group(TESTCASE_GROUP)
            html += "<tr>"
            html += f"<td>{TESTCASE_GROUP}</td>"
            html += f"<td>{len(exceptions)}</td>"
            html += "<td>"
            for exception in exceptions:
                html += f"{exception}<br />"
            html += "</td>"
            html += "</tr>"

        return html

