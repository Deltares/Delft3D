#!/usr/bin/env python3
"""Update the Excel sheet with this week's release information."""

import sys
from datetime import datetime, timezone
from typing import List

from openpyxl import load_workbook
from openpyxl.worksheet.worksheet import Worksheet

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.common_utils import SummaryResults, get_testbank_result_parser
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.ssh_client import Direction
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class ExcelHelper(StepExecutorInterface):
    """
    Object responsible for updating the Excel sheet.

    This class updates the Excel sheet with release information for DIMR.
    Usage: Instantiate with context and services, then call execute_step().
    """

    def __init__(
        self,
        context: DimrAutomationContext,
        services: Services,
    ) -> None:
        """
        Initialize ExcelHelper.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing configuration and clients.
        services : Services
            Service objects for SSH, TeamCity, etc.
        """
        self.__context = context
        self.__services = services
        self.__filepath = context.settings.versions_excel_filename
        self.__dimr_version = context.dimr_version
        self.__kernel_versions = context.kernel_versions
        self.__parser = get_testbank_result_parser(context)
        self.__sheet_name = context.settings.sheet_name
        self.__name_column = context.settings.name_column

    def execute_step(self) -> bool:
        """
        Update the Excel sheet with this week's release information.

        Returns
        -------
        bool
            True if the update was successful, False otherwise.
        """
        self.__context.log("Updating Excel sheet...")

        if self.__context.dry_run:
            self.__context.log(f"Would update Excel sheet with DIMR version: {self.__context.dimr_version}")
            self.__context.log("Would download Excel from network drive")
            self.__context.log("Would append new row with release information")
            self.__context.log("Would upload updated Excel back to network drive")
            return True

        path_to_excel_file = f"/p/d-hydro/dimrset/{self.__context.settings.versions_excel_filename}"

        if self.__services.ssh is None:
            self.__context.log("SSH client is required but not initialized", severity=LogLevel.ERROR)
            return False
        if self.__services.teamcity is None:
            self.__context.log("TeamCity client is required but not initialized")
            return False

        self.__services.ssh.secure_copy(
            self.__context.settings.versions_excel_filename,
            path_to_excel_file,
            Direction.FROM,
        )

        self.__append_row()
        self.__services.ssh.secure_copy(
            self.__context.settings.versions_excel_filename,
            path_to_excel_file,
            Direction.TO,
        )

        self.__context.log("Excel sheet update completed successfully!")
        return True

    def __append_row(self) -> None:
        """Append a new row to the Excel sheet with this week's DIMR information."""
        row = self.__prepare_row_to_insert()
        print(row)

        workbook = None
        try:
            workbook = load_workbook(filename=self.__filepath)
            worksheet = workbook[self.__sheet_name]

            if self.__worksheet_already_contains_row(worksheet):
                print("The Excel sheet already contains a row for this DIMRset value and revision number.")
                return

            worksheet.append(row)
            workbook.save(filename=self.__filepath)
        except Exception as e:
            print("Could not update the excel: \n")
            print(e)
        finally:
            if workbook is not None:
                workbook.close()

    def __prepare_row_to_insert(self) -> List[str]:
        """
        Prepare a row to be inserted in the Excel sheet.

        Returns
        -------
        List[str]
            The row data to insert into the Excel sheet.
        """
        row = []

        row.append("")  # Column A (empty column)
        row.append(str(datetime.now(tz=timezone.utc).date()))  # Column B (Date)
        row.append(f"DIMRset {self.__dimr_version}")  # Column C (DIMR version)
        row.append("")  # Column D (Revision)
        row.append("FLOW1D2D now in GitHub")  # Column E (Flow1D)
        row.append("OSS")  # Column F (FlowFM)
        row.append(self.__kernel_versions["build.vcs.number"])  # Column G (OSS)
        row.append("DRR now in GitHub")  # Column H (RR)
        row.append("FBC now in GitHub")  # Column I (FBC)
        row.append(self.__parser.get_value(SummaryResults.PERCENTAGE))  # Column J (Percentage passing)
        row.append(self.__parser.get_value(SummaryResults.TOTAL_TESTS))  # Column K (Total Number of          cases)
        row.append(self.__parser.get_value(SummaryResults.PASSED))  # Column L (      Number of green    cases)
        row.append(self.__parser.get_value(SummaryResults.NOT_PASSED))  # Column M (      Number of red      cases)
        row.append(self.__parser.get_value(SummaryResults.EXCEPTION))  # Column N (      Number of crashing cases)
        row.append("")  # Column O (Docker hub)
        row.append("Flow1D and RR: only Windows")  # Column P (Remarks)

        return row

    def __worksheet_already_contains_row(self, worksheet: Worksheet) -> bool:
        """
        Check if the Excel sheet already contains a row for the given DIMRset.

        Parameters
        ----------
        worksheet : Worksheet
            The worksheet to check for existing rows.

        Returns
        -------
        bool
            True if the row exists, False otherwise.
        """
        name_already_exists = False

        name_column = worksheet[self.__name_column]

        for cell in name_column:
            if cell.value == f"DIMRset {self.__dimr_version}":
                name_already_exists = True

        return name_already_exists


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_git=False, require_jira=False)
        services = Services(context)

        context.log("Starting Excel sheet update...")
        if ExcelHelper(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed Excel sheet update!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nExcel sheet update interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Excel sheet update failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
