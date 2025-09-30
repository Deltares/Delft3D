#!/usr/bin/env python3
"""Prepare a mail template for the release notification."""

import os
import sys

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.common_utils import (
    SummaryResults,
    get_previous_testbank_result_parser,
    get_testbank_result_parser,
)
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import KERNELS
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class EmailHelper(StepExecutorInterface):
    """
    Prepares the weekly DIMR release email.

    This class generates an HTML email template for DIMRset releases, including kernel versions,
    test results, and exception summaries. Use `execute_step` to perform the preparation.
    """

    # Constants
    DIMR_SET_VERSION_KEY = "DIMRset_ver"
    PASS_SPACING = "&nbsp;" * 5

    def __init__(
        self,
        context: DimrAutomationContext,
        services: Services,
    ) -> None:
        """
        Initialize EmailHelper.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing configuration and clients.
        services : Services
            Service manager for external dependencies.
        """
        self.__context = context
        self.__settings = context.settings
        self.__dimr_version = context.dimr_version
        self.__kernel_versions = context.kernel_versions
        self.__current_parser = get_testbank_result_parser(context)
        self.__previous_parser = get_previous_testbank_result_parser(context, services)
        self.__template = ""

    def execute_step(self) -> bool:
        """
        Prepare the mail template for the release notification.

        Returns
        -------
        bool
            True if the template was prepared successfully.
        """
        self.__context.log("Preparing email template...")

        success = self.__generate_template()
        if success:
            self.__context.log("Email template preparation completed successfully!")
            return True
        else:
            self.__context.log("Email template preparation failed.")
            return False

    def __generate_template(self) -> bool:
        """
        Generate the email template for the latest DIMR release.

        Returns True if all steps succeed, False otherwise.
        """
        number_of_errors: int = 0
        if not self.__load_template():
            number_of_errors += 1

        self.__insert_summary_table_header()
        self.__insert_summary_table()

        if not self.__save_template():
            number_of_errors += 1

        return number_of_errors == 0

    def __load_template(self) -> bool:
        """Load the email template into memory. Returns True if successful, False otherwise."""
        current_dir = os.path.dirname(__file__)
        path_to_email_template = os.path.join(current_dir, self.__settings.relative_path_to_email_template)
        try:
            with open(path_to_email_template, "r") as file:
                self.__template = file.read()
            return True
        except Exception as e:
            self.__context.log(f"Failed to load email template: {e}", severity=LogLevel.ERROR)
            return False

    def __insert_summary_table_header(self) -> None:
        """Insert the summary table header into the template."""
        html = self.__template

        html = html.replace("@@@DIMR_VERSION@@@", self.__dimr_version)

        self.__template = html

    def __create_status_span(self, value: str, is_percentage: bool = False) -> str:
        """
        Create a span with success or fail class based on value.

        Parameters
        ----------
        value : str
            The value to display.
        is_percentage : bool
            Whether the value is a percentage to compare to threshold.

        Returns
        -------
        str
            HTML span element with appropriate CSS class.
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
        """
        Generate the summary table HTML for the email.

        Returns
        -------
        str
            HTML string for the summary table.
        """
        html_parts = []

        # Add kernel information rows
        html_parts.extend(self.__generate_kernel_rows())

        # Add test results row
        html_parts.append(self.__generate_test_results_row())

        # Add exceptions row
        html_parts.append(self.__generate_exceptions_row())

        return "".join(html_parts)

    def __generate_kernel_rows(self) -> list[str]:
        """
        Generate HTML rows for kernel information.

        Returns
        -------
        list of str
            List of HTML table row strings for kernels.
        """
        rows = []
        for kernel, revision in self.__kernel_versions.items():
            if kernel == self.DIMR_SET_VERSION_KEY:
                continue

            kernel_name = self.__get_email_friendly_kernel_name(kernel)
            rows.append(f"<tr><td>{kernel_name}</td><td>{revision}</td><td></td></tr>")

        return rows

    def __generate_test_results_row(self) -> str:
        """
        Generate HTML row for test results with comparison to previous results.

        Returns
        -------
        str
            HTML table row for test results.
        """
        passing_percentage = self.__current_parser.get_value(SummaryResults.PERCENTAGE)
        status_span = self.__create_status_span(passing_percentage, is_percentage=True)

        row_parts = ["<tr>", "<td></td>", f"<td>{status_span}</td>", "<td><table><tr><td><br />"]

        if self.__previous_parser is not None:
            row_parts.extend(self.__generate_previous_test_comparison())
        else:
            row_parts.extend(self.__generate_current_test_info_only(passing_percentage))

        row_parts.extend(["</tr></table></td></tr>"])
        return "".join(row_parts)

    def __generate_previous_test_comparison(self) -> list[str]:
        """
        Generate HTML for test results comparison with previous results.

        Returns
        -------
        list of str
            List of HTML strings for previous test comparison.
        """
        assert self.__previous_parser is not None, "Previous parser must not be None"

        previous_percentage = self.__previous_parser.get_value(SummaryResults.PERCENTAGE)
        previous_span = self.__create_status_span(previous_percentage, is_percentage=True)

        return [
            f"Green testbank was ({previous_span})<br />",
            f"Total tests: {self.__current_parser.get_value(SummaryResults.TOTAL_TESTS)} ",
            f"was ({self.__previous_parser.get_value(SummaryResults.TOTAL_TESTS)})<br />",
            f"Passed{self.PASS_SPACING}: {self.__current_parser.get_value(SummaryResults.PASSED)} ",
            f"was ({self.__previous_parser.get_value(SummaryResults.PASSED)})",
        ]

    def __generate_current_test_info_only(self, passing_percentage: str) -> list[str]:
        """
        Generate HTML for current test info when no previous results are available.

        Parameters
        ----------
        passing_percentage : str
            Percentage of passing tests.

        Returns
        -------
        list of str
            List of HTML strings for current test info.
        """
        success_span = self.__create_status_span(passing_percentage, is_percentage=True)

        return [
            f"Green testbank: {success_span}<br />",
            f"Total tests: {self.__current_parser.get_value(SummaryResults.TOTAL_TESTS)}<br />",
            f"Passed{self.PASS_SPACING}: {self.__current_parser.get_value(SummaryResults.PASSED)}",
        ]

    def __generate_exceptions_row(self) -> str:
        """
        Generate HTML row for exceptions/crashes information.

        Returns
        -------
        str
            HTML table row for exceptions/crashes.
        """
        total_exceptions = self.__current_parser.get_value(SummaryResults.EXCEPTION)
        status_span = self.__create_status_span(total_exceptions, is_percentage=False)

        row_parts = ["<tr>", "<td></td>", f"<td>{status_span}</td>", "<td>"]

        if self.__previous_parser is not None:
            previous_exceptions = self.__previous_parser.get_value(SummaryResults.EXCEPTION)
            previous_span = self.__create_status_span(previous_exceptions, is_percentage=False)
            row_parts.append(f"Crashes in testbank (was {previous_span})")
        else:
            row_parts.append("Crashes in testbank (no previous data)")

        row_parts.extend(["</td></tr>"])
        return "".join(row_parts)

    def __get_email_friendly_kernel_name(self, kernel: str) -> str:
        """
        Get the email-friendly kernel name for a given kernel.

        Parameters
        ----------
        kernel : str
            Kernel identifier.

        Returns
        -------
        str
            Kernel name formatted for email.
        """
        kernel_name = ""
        for kernel_config in KERNELS:
            if kernel_config.name_for_extracting_revision == kernel:
                kernel_name = kernel_config.name_for_email
        return kernel_name

    def __save_template(self) -> bool:
        """Save the email template in the output folder. Returns True if successful, False otherwise."""
        current_dir = os.path.dirname(__file__)
        path_to_output_folder = os.path.join(current_dir, self.__settings.relative_path_to_output_folder)
        path_to_email_template = os.path.join(path_to_output_folder, f"DIMRset_{self.__dimr_version}.html")
        try:
            if not os.path.exists(path_to_output_folder):
                os.makedirs(path_to_output_folder)
            with open(path_to_email_template, "w+") as file:
                file.write(self.__template)
            self.__context.log(f"Saved email html to {path_to_email_template}")
            return True
        except Exception as e:
            self.__context.log(f"Failed to save email template: {e}", severity=LogLevel.ERROR)
            return False


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_git=False, require_ssh=False)
        services = Services(context)

        context.log("Starting email template preparation...")
        if EmailHelper(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed email template preparation!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nEmail template preparation interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Email template preparation failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
