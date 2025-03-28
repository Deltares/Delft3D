from datetime import date
from typing import Dict, List
from openpyxl import load_workbook, worksheet

from helpers.TestbankResultParser import TestbankResultParser
from lib.TeamCity import TeamCity
from settings.general_settings import SHEET_NAME, NAME_COLUMN


class ExcelHelper(object):
    """ Object responsible for updating the Excel sheet. """

    def __init__(self, teamcity: TeamCity, filepath: str, dimr_version: str,
                 kernel_versions: Dict[str, str], parser: TestbankResultParser):
        """
        Creates a new instance of ExcelHelper.

        Args:
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            filepath (str): Path to the Excel file.
            dimr_version (str): The DIMR version to upate the Excel for.
            kernel_versions (Dict[str, str]): A dictionary mapping kernel names to their version.
            parser (TestbankResultParser): A parser for the latest test bench results.
        """
        self.__teamcity = teamcity
        self.__filepath = filepath
        self.__dimr_version = dimr_version
        self.__kernel_versions = kernel_versions
        self.__parser = parser

    def append_row(self) -> None:
        """ Appends a new row to the Excel sheet with this week's DIMR information. """
        row = self.__prepare_row_to_insert()
        print(row)

        try:
            workbook = load_workbook(filename=self.__filepath)
            worksheet = workbook[SHEET_NAME]

            if self.__worksheet_already_contains_row(worksheet):
                print("The Excel sheet already contains a row for this DIMRset value and revision number.")
                return

            worksheet.append(row)
            workbook.save(filename=self.__filepath)
        except Exception as e:
            print("Could not update the excel: \n")
            print(e)
        finally:
            workbook.close()

    def __prepare_row_to_insert(self) -> List[str]:
        """ Prepares a row to be inserted in the Excel sheet. """
        row = []

        row.append("")                                              # Column A (empty column)
        row.append(date.today())                                    # Column B (Date)
        row.append(f"DIMRset {self.__dimr_version}")                # Column C (DIMR version)
        row.append("")                                              # Column D (Revision)
        row.append("FLOW1D2D now in GitLab")                        # Column E (Flow1D)
        row.append("OSS")                                           # Column F (FlowFM)
        row.append(self.__kernel_versions["OSS_ver"])               # Column G (OSS)
        row.append("DRR now in GitLab")                             # Column H (RR)
        row.append("FBC now in GitLab")                             # Column I (FBC)
        row.append(self.__parser.get_percentage_total_passing())    # Column J (Percentage passing)
        row.append(self.__parser.get_total_tests())                 # Column K (Total Number of          cases)
        row.append(self.__parser.get_total_passing())               # Column L (      Number of green    cases)
        row.append(self.__parser.get_total_failing())               # Column M (      Number of red      cases)
        row.append(self.__parser.get_total_exceptions())            # Column N (      Number of crashing cases)
        row.append("")                                              # Column O (Docker hub)
        row.append("Flow1D and RR: only Windows")                   # Column P (Remarks)

        return row

    def __worksheet_already_contains_row(self, worksheet: worksheet):
        """
        Checks if the Excel sheet already contains a row for the given DIMRset.
        Return True if the Excel sheet already contains such a row.
        """
        name_already_exists = False

        name_column = worksheet[NAME_COLUMN]

        for cell in name_column:
            if cell.value == f"DIMRset {self.__dimr_version}":
                name_already_exists = True

        return name_already_exists



