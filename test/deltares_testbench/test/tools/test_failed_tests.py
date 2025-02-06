from inspect import Parameter

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.suite.comparison_runner import ComparisonRunner
from src.suite.run_data import RunData
from src.suite.test_bench_settings import TestBenchSettings
from src.suite.test_case_result import TestCaseResult
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.end_result import EndResult
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.log_level import LogLevel
from tools.failed_tests import get_failed_tests


class TestFailedTests:
    def test_get_failed_tests(self, tmpdir) -> None:
        # Arrange
        expected_result = ["Test 1", "Test 3"]
        test1 = self.__create_test_case_config("Test 1")
        test2 = self.__create_test_case_config("Test 2")
        test3 = self.__create_test_case_config("Test 3")
        rd = RunData(1, 1)
        case_result1 = self.__create_error_result(test1, rd, EndResult.NOK)
        case_result2 = self.__create_succesfuls_result(test2, rd, EndResult.OK)
        case_result3 = self.__create_error_result(test3, rd, EndResult.ERROR)
        test_log = tmpdir.join("test_log.log")
        logger = FileLogger(LogLevel.INFO, "file_logger", test_log)
        settings = TestBenchSettings()
        runner = ComparisonRunner(settings, logger)
        runner.show_summary([case_result1, case_result2, case_result3], logger)

        # Act
        result = get_failed_tests(str(test_log))

        # Assert
        assert result == expected_result

    @staticmethod
    def __create_test_case_config(name: str):
        tcc = TestCaseConfig()
        tcc.name = name
        return tcc

    @staticmethod
    def __create_error_result(test_case_config: TestCaseConfig, run_data: RunData, result: EndResult) -> TestCaseResult:
        comparison_result = ComparisonResult()
        comparison_result.max_abs_diff = 0.0
        comparison_result.max_rel_diff = 0.0
        comparison_result.passed = False
        comparison_result.error = True
        comparison_result.result = result
        result = TestCaseResult(test_case_config, run_data)
        result.results.append((test_case_config.name, FileCheck(), Parameter(), comparison_result))

        return result

    @staticmethod
    def __create_succesfuls_result(
        test_case_config: TestCaseConfig, run_data: RunData, result: EndResult
    ) -> TestCaseResult:
        comparison_result = ComparisonResult()
        comparison_result.max_abs_diff = 100.0
        comparison_result.max_rel_diff = 100.0
        comparison_result.passed = True
        comparison_result.error = False
        comparison_result.result = result
        result = TestCaseResult(test_case_config, run_data)
        result.results.append((test_case_config.name, FileCheck(), Parameter(), comparison_result))

        return result
