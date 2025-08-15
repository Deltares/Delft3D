#!/usr/bin/env python3
"""Prepare a mail template for the release notification."""

import os
from typing import Dict, Optional

from ci_tools.dimrset_delivery.common_utils import (
    ResultTestBankParser,
    get_previous_testbank_result_parser,
    get_testbank_result_parser,
)
from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.settings.teamcity_settings import KERNELS

# Mock data for dry-run mode
MOCK_CURRENT_TEST_RESULTS = """
Summary: All
Total tests   :   2000
    Passed    :   2000
    Not passed:      0
    Failed    :      0
    Exception :      0
    Ignored   :      0
    Muted     :      0
    Percentage: 100.00
"""

MOCK_PREVIOUS_TEST_RESULTS = """
Summary: All
Total tests   :   1900
    Passed    :   1800
    Not passed:      20
    Failed    :      20
    Exception :      20
    Ignored   :      20
    Muted     :      20
    Percentage: 94.74
"""


def prepare_email(context: DimrAutomationContext) -> None:
    """Prepare a mail template for the release notification.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.log("Preparing email template...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(
            f"{context.settings.dry_run_prefix} Preparing email template for DIMR version:",
            dimr_version,
        )
        # Create mock parsers with sensible default values for dry-run
        parser = ResultTestBankParser(MOCK_CURRENT_TEST_RESULTS.strip())
        previous_parser = ResultTestBankParser(MOCK_PREVIOUS_TEST_RESULTS.strip())
    else:
        parser = get_testbank_result_parser(context.settings.path_to_release_test_results_artifact)
        previous_parser = get_previous_testbank_result_parser(context)

    helper = EmailHelper(
        context=context,
        kernel_versions=kernel_versions,
        current_parser=parser,
        previous_parser=previous_parser,
    )
    helper.generate_template()

    print("Email template preparation completed successfully!")


class EmailHelper:
    """Class responsible for preparing the weekly DIMR release email."""

    # Constants
    DIMR_SET_VERSION_KEY = "DIMRset_ver"
    PASS_SPACING = "&nbsp;" * 5

    def __init__(
        self,
        context: DimrAutomationContext,
        kernel_versions: Dict[str, str],
        current_parser: ResultTestBankParser,
        previous_parser: Optional[ResultTestBankParser],
    ) -> None:
        """
        Create a new instance of EmailHelper.

        Args:
            dimr_version (str): The latest DIMR version.
            kernel_versions (Dict[str, str]): A dictionary mapping kernel names to their version.
            current_parser (ResultTestBankParser): A parser for the latest test bench results.
            previous_parser (Optional[ResultTestBankParser]): A parser for the previous test bench results.
        """
        self.__dimr_version = context.get_dimr_version()
        self.__kernel_versions = kernel_versions
        self.__current_parser = current_parser
        self.__previous_parser = previous_parser
        self.__template = ""
        self.__settings = context.settings

    def generate_template(self) -> None:
        """Generate a template email for the latest DIMR release that can be copy/pasted into Outlook."""
        self.__load_template()
        self.__insert_summary_table_header()
        self.__insert_summary_table()
        self.__save_template()

    def __load_template(self) -> None:
        """Load the template into memory."""
        current_dir = os.path.dirname(__file__)
        path_to_email_template = os.path.join(current_dir, self.__settings.relative_path_to_email_template)

        with open(path_to_email_template, "r") as file:
            self.__template = file.read()

    def __insert_summary_table_header(self) -> None:
        """Insert the summary table header into the template."""
        html = self.__template

        html = html.replace("@@@DIMR_VERSION@@@", self.__dimr_version)
        html = html.replace("@@@LINK_TO_PUBLIC_WIKI@@@", self.__generate_wiki_link())

        self.__template = html

    def __generate_wiki_link(self) -> str:
        """Generate a link to the public wiki for the DIMR version."""
        link = f"https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+{self.__dimr_version}"
        return f'<a href="{link}">{link}</a>'

    def __create_status_span(self, value: str, is_percentage: bool = False) -> str:
        """Create a span with success or fail class based on value.

        Parameters
        ----------
        value : str
            The value to display
        is_percentage : bool
            Whether the value is a percentage that should be compared to threshold

        Returns
        -------
        str
            HTML span element with appropriate CSS class
        """
        suffix = "%" if is_percentage else ""

        if is_percentage:
            is_success = float(value) >= int(self.__settings.lower_bound_percentage_successful_tests)
        else:
            # For exceptions, 0 is success, anything else is failure
            is_success = int(value) == 0

        css_class = "success" if is_success else "fail"
        return f'<span class="{css_class}">{value}{suffix}</span>'

    def __insert_summary_table(self) -> None:
        """Insert the summary table into the template."""
        html = self.__generate_summary_table_html()
        self.__template = self.__template.replace("@@@SUMMARY_TABLE_BODY@@@", html)

    def __generate_summary_table_html(self) -> str:
        """Dynamically generates the summary table based on the kernels expected to be present."""
        html_parts = []

        # Add kernel information rows
        html_parts.extend(self.__generate_kernel_rows())

        # Add test results row
        html_parts.append(self.__generate_test_results_row())

        # Add exceptions row
        html_parts.append(self.__generate_exceptions_row())

        return "".join(html_parts)

    def __generate_kernel_rows(self) -> list[str]:
        """Generate HTML rows for kernel information."""
        rows = []
        for kernel, revision in self.__kernel_versions.items():
            if kernel == self.DIMR_SET_VERSION_KEY:
                continue

            kernel_name = self.__get_email_friendly_kernel_name(kernel)
            rows.append(f"<tr><td>{kernel_name}</td><td>{revision}</td><td></td></tr>")

        return rows

    def __generate_test_results_row(self) -> str:
        """Generate HTML row for test results with comparison to previous results."""
        passing_percentage = self.__current_parser.get_percentage_total_passing()
        status_span = self.__create_status_span(passing_percentage, is_percentage=True)

        row_parts = ["<tr>", "<td></td>", f"<td>{status_span}</td>", "<td><table><tr><td><br />"]

        if self.__previous_parser is not None:
            row_parts.extend(self.__generate_previous_test_comparison())
        else:
            row_parts.extend(self.__generate_current_test_info_only(passing_percentage))

        row_parts.extend(["</tr></table></td></tr>"])
        return "".join(row_parts)

    def __generate_previous_test_comparison(self) -> list[str]:
        """Generate HTML for test results comparison with previous results."""
        assert self.__previous_parser is not None, "Previous parser must not be None"

        previous_percentage = self.__previous_parser.get_percentage_total_passing()
        previous_span = self.__create_status_span(previous_percentage, is_percentage=True)

        return [
            f"Green testbank was ({previous_span})<br />",
            f"Total tests: {self.__current_parser.get_total_tests()} ",
            f"was ({self.__previous_parser.get_total_tests()})<br />",
            f"Passed{self.PASS_SPACING}: {self.__current_parser.get_total_passing()} ",
            f"was ({self.__previous_parser.get_total_passing()})",
        ]

    def __generate_current_test_info_only(self, passing_percentage: str) -> list[str]:
        """Generate HTML for current test info when no previous results available."""
        success_span = self.__create_status_span(passing_percentage, is_percentage=True)

        return [
            f"Green testbank: {success_span}<br />",
            f"Total tests: {self.__current_parser.get_total_tests()}<br />",
            f"Passed{self.PASS_SPACING}: {self.__current_parser.get_total_passing()}",
        ]

    def __generate_exceptions_row(self) -> str:
        """Generate HTML row for exceptions/crashes information."""
        total_exceptions = self.__current_parser.get_total_exceptions()
        status_span = self.__create_status_span(total_exceptions, is_percentage=False)

        row_parts = ["<tr>", "<td></td>", f"<td>{status_span}</td>", "<td>"]

        if self.__previous_parser is not None:
            previous_exceptions = self.__previous_parser.get_total_exceptions()
            previous_span = self.__create_status_span(previous_exceptions, is_percentage=False)
            row_parts.append(f"Crashes in testbank (was {previous_span})")
        else:
            row_parts.append("Crashes in testbank (no previous data)")

        row_parts.extend(["</td></tr>"])
        return "".join(row_parts)

    def __get_email_friendly_kernel_name(self, kernel: str) -> str:
        """Get the email friendly kernel name for a given kernel."""
        kernel_name = ""
        for kernel_config in KERNELS:
            if kernel_config.name_for_extracting_revision == kernel:
                kernel_name = kernel_config.name_for_email
        return kernel_name

    def __save_template(self) -> None:
        """Save the email template in the output folder."""
        current_dir = os.path.dirname(__file__)
        path_to_output_folder = os.path.join(current_dir, self.__settings.relative_path_to_output_folder)
        path_to_email_template = os.path.join(path_to_output_folder, f"DIMRset_{self.__dimr_version}.html")

        print(f"Saved email html to {path_to_email_template}")
        if not os.path.exists(path_to_output_folder):
            os.makedirs(path_to_output_folder)

        with open(path_to_email_template, "w+") as file:
            file.write(self.__template)


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False, require_ssh=False)

    print("Starting email template preparation...")
    prepare_email(context)
    print("Finished")
