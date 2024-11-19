import os
from datetime import datetime
from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.config.types.presence_type import PresenceType
from src.suite.run_data import RunData
from src.suite.test_case_result import TestCaseResult
from src.suite.test_set_runner import TestSetRunner
from src.utils.common import get_default_logging_folder_path, log_header, log_separator, log_table
from src.utils.comparers.comparer_factory import ComparerFactory
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.composite_logger import CompositeLogger
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.i_logger import ILogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType
from src.utils.paths import Paths


class ComparisonRunner(TestSetRunner):
    """Test runner that compares files."""

    def post_process(
        self,
        test_case_config: TestCaseConfig,
        logger: ITestLogger,
        run_data: RunData,
    ) -> TestCaseResult:
        test_result = TestCaseResult(test_case_config, run_data)
        skip_report = self.__skip_test_case(test_case_config)
        if skip_report:
            logger.warning("No checks performed for this testcase (ignored)")
            run_data.end_time = datetime.now()
            return test_result

        logger.info("Comparing results with reference")

        # Step 1: check if all files in the reference also exist in the compare run.
        all_reference_files = Paths().findAllSubFiles(test_case_config.absolute_test_case_reference_path)
        all_result_files = Paths().findAllSubFiles(test_case_config.absolute_test_case_path)

        self.__warn_for_missing_files(test_case_config, all_reference_files, all_result_files, logger)

        # Step 2: content comparison
        for file_check in test_case_config.checks:
            comparer = ComparerFactory.select_comparer(file_check, self.programs, logger)

            if file_check.ignore:
                continue

            self.__raise_error_when_missing(all_result_files, file_check)
            results = self.__compare_files(test_case_config, file_check, comparer, logger)

            if results == [] or results is None:
                continue

            # add test case results for file check
            test_result.results += results

        if len(test_result.results) == 0:
            logger.warning("No results to display")
            logger.test_Result(TestResultType.Empty)
            run_data.end_time = datetime.now()
            return test_result

        # Step 3: Write the results to a .txt file in the test case directory.
        log_file = os.path.join(get_default_logging_folder_path(), test_case_config.name, "result.txt")
        logger.info(f"Detailed comparison results will be written to: {log_file}")

        logger_list = [FileLogger(self.settings.log_level, test_case_config.name, log_file)]
        if self.settings.teamcity:
            logger_list.append(logger)

        composite_logger = CompositeLogger(logger_list)

        composite_logger.debug("RESULTS of the comparison run")

        table = {
            "Test case name": [],
            "Filename": [],
            "Parameter": [],
            "Location": [],
            "Result": [],
            "MaxAbsDiff": [],
            "MaxRelDiff": [],
        }

        failed = False
        error = False

        error_variables = []
        for testcase_name, file_check, parameter, compare_result in test_result.results:
            table["Test case name"].append(testcase_name)
            table["Filename"].append(file_check.name)
            table["Parameter"].append(str(parameter.name))
            table["Location"].append(str(parameter.location))

            if compare_result.result != "ERROR":
                table["Result"].append(compare_result.result)
                table["MaxAbsDiff"].append(compare_result.max_abs_diff)
                table["MaxRelDiff"].append(compare_result.max_rel_diff)
            else:
                table["Result"].append("<error>")
                table["MaxAbsDiff"].append("<error>")
                table["MaxRelDiff"].append("<error>")
                error = True
                error_variables.append(str(parameter.name))

            if compare_result.result == "NOK":
                failed = True

        log_table(table, composite_logger)

        if self.settings.teamcity and error:
            logger.test_Result(TestResultType.Error, ", ".join(error_variables))

        elif self.settings.teamcity and failed:
            logger.test_Result(TestResultType.Differences)

        run_data.end_time = datetime.now()
        return test_result

    def __compare_files(
        self,
        test_case_config: TestCaseConfig,
        file_check: FileCheck,
        comparer: IComparer,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        logger.debug(f"checking file {file_check.name}")

        absolute_file_name = os.path.join(test_case_config.absolute_test_case_path, file_check.name)

        file_exists = os.path.exists(absolute_file_name)

        if file_check.presence == PresenceType.NONE:
            return comparer.compare(
                test_case_config.absolute_test_case_reference_path,
                test_case_config.absolute_test_case_path,
                file_check,
                test_case_config.name,
                logger,
            )

        parameter = Parameter()
        parameter.name = file_check.name
        result = ComparisonResult()

        if file_check.presence == PresenceType.ABSENT:
            result.result = "NOK" if file_exists else "OK"

        if file_check.presence == PresenceType.PRESENT:
            result.result = "OK" if file_exists else "NOK"

        return [(test_case_config.name, file_check, parameter, result)]

    def __raise_error_when_missing(self, all_result_files: list[str], file_check: FileCheck) -> None:
        in_results = Paths().rebuildToLocalPath(file_check.name) in all_result_files
        if (
            not in_results
            and os.path.splitext(file_check.name)[1] != ".log"
            and file_check.presence == PresenceType.NONE
        ):
            raise Exception("Could not check %s, file not found in result data" % file_check.name)

    def __warn_for_missing_files(
        self,
        test_case_config: TestCaseConfig,
        all_reference_files: List[str],
        all_result_files: List[str],
        logger: ILogger,
    ) -> None:
        if len(all_reference_files) == 0:
            raise OSError(
                -1,
                "Could not locate reference data",
                test_case_config.absolute_test_case_reference_path,
            )

        for file_name in all_reference_files:
            # ignore files with these extensions
            if file_name not in all_result_files and os.path.splitext(file_name)[1] not in (
                ".log",
                ".tmp",
            ):
                ignore = False
                fllocal = Paths().rebuildToLocalPath(file_name)
                # To check whether a missing file can be ignored: use "config.getChecks()" instead of "comparers.keys()"
                # Otherwise files without checks (but with the "ignore" sign) won't be used
                for fc in test_case_config.checks:
                    fclocal = Paths().rebuildToLocalPath(fc.name)
                    if fclocal == fllocal:
                        ignore = fc.ignore
                        break
                if ignore:
                    logger.debug(f"File {file_name} is missing but marked as 'ignore'")
                else:
                    logger.warning(
                        f"File {os.path.basename(file_name)} part of reference result but not part of case result",
                    )

    def __skip_test_case(self, test_case_config: TestCaseConfig):
        return test_case_config.ignore or all([fc.ignore for fc in test_case_config.checks])

    def show_summary(self, results: List[TestCaseResult], logger: ILogger) -> None:
        """Write a summary of the result to the logging module.

        Typically redirects to standard error.
        """
        log_header("SUMMARY of the compare run", logger)

        table = {
            "Test case name": [],
            "Runtime": [],
            "Ratio": [],
            "Result": [],
            "MaxAbsDiff": [],
            "MaxRelDiff": [],
            "File name": [],
            "Parameter name": [],
            "Information": [],
        }

        for result in results:
            test_case_config = result.config
            tc_results = result.results

            # For this test case, find the 'worst' result. This has to take into
            # account that a lower maxAbsDiff for a NOK is worse than a higher
            # maxAbsDiff for an OK.
            # That is: the result always has priority over the maxAbsDiff.
            # We make use here of the fact that 'ERROR' < 'NOK' < 'OK',
            #  lexicographically.

            maxAbsDiff_worst = 0.0
            result_worst = "OK"
            i_worst = -1
            for i, (_, _, _, comparison_result) in enumerate(tc_results):
                if comparison_result.result < result_worst or (
                    comparison_result.result == result_worst and comparison_result.max_abs_diff > maxAbsDiff_worst
                ):
                    maxAbsDiff_worst = comparison_result.max_abs_diff
                    result_worst = comparison_result.result
                    i_worst = i

            if i_worst == -1 and tc_results and len(tc_results) > 0:
                i_worst = len(tc_results) - 1

            # Local variables now contain the 'worst' scores for that test case. This one will be written in the summary
            worst_result = tc_results[i_worst]
            _, w_filename, w_parameter, w_cr = worst_result

            table["Test case name"].append(test_case_config.name)
            table["Result"].append(w_cr.result)
            table["File name"].append(w_filename.name)
            table["Parameter name"].append(w_parameter.name)

            is_ignored = w_cr.result == "IGNORED"
            table["Runtime"].append(test_case_config.run_time if not is_ignored else "---")
            table["Ratio"].append(
                test_case_config.run_time / test_case_config.ref_run_time if not is_ignored else "---"
            )
            table["MaxAbsDiff"].append(w_cr.max_abs_diff if not is_ignored else "---")
            table["MaxRelDiff"].append(w_cr.max_rel_diff if not is_ignored else "---")
            table["Information"].append(
                "at coordinates" + str(w_cr.max_abs_diff_coordinates) if not is_ignored else "---"
            )

        log_table(table, logger)
        log_separator(logger)

    def create_error_result(self, test_case_config: TestCaseConfig, run_data: RunData) -> TestCaseResult:
        comparison_result = ComparisonResult()
        comparison_result.max_abs_diff = 0.0
        comparison_result.max_rel_diff = 0.0
        comparison_result.passed = False
        comparison_result.error = True
        comparison_result.result = "ERROR"
        result = TestCaseResult(test_case_config, run_data)
        result.results.append((test_case_config.name, FileCheck(), Parameter(), comparison_result))

        return result
