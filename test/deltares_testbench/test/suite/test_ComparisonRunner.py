import glob
import os
import pathlib as pl
from unittest.mock import MagicMock, call

import pytest
from pytest_mock import MockerFixture

from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.path_type import PathType
from src.suite.comparison_runner import ComparisonRunner
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.common import get_default_logging_folder_path
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.paths import Paths
from src.utils.xml_config_parser import XmlConfigParser


class TestComparisonRunner:
    @pytest.mark.usefixtures("fs")  # Use fake filesystem.
    def test_run_tests_and_debug_log_downloaded_file(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.skip_run = True
        config = TestComparisonRunner.create_test_case_config("Name_1", False)
        config.path = TestCasePath("abc/prefix", "v1")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        download_mock = mocker.patch("src.suite.test_set_runner.HandlerFactory.download")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        path = Paths().rebuildToLocalPath(Paths().mergeFullPath("references", "Name_1", "Name_1"))
        expected_log_message = f"Downloading reference result, {path} from https://deltares.nl/Name_1/abc/prefix"
        assert call(expected_log_message) in testcase_logger.debug.call_args_list
        assert download_mock.call_count == 2  # Downloads case AND reference data.

    def test_log_and_skip_with_argument_skip_run(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.skip_run = True
        config = TestComparisonRunner.create_test_case_config("Name_1", False, PathType.INPUT)
        config.path = TestCasePath("abc/prefix", "v1")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        prepare_mock = mocker.patch("src.suite.test_set_runner.TestSetRunner._TestSetRunner__prepare_test_case")
        run_mock = mocker.patch("src.suite.test_case.TestCase.run")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        expected_log_message = "Skipping execution of testcase (postprocess only)...\n"
        assert call(expected_log_message) in testcase_logger.info.call_args_list
        prepare_mock.assert_called()
        run_mock.assert_not_called()

    def test_skip_download_for_parameter_no_download(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.skip_download = [PathType.INPUT]
        config = TestComparisonRunner.create_test_case_config("Name_1", False, PathType.INPUT)
        config.path = TestCasePath("abc/prefix", "v1")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        download_mock = mocker.patch("src.suite.test_set_runner.HandlerFactory.download")
        run_mock = mocker.patch("src.suite.test_case.TestCase.run")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        expected_log_message = "Skipping input of case download (skip download argument)"
        assert call(expected_log_message) in testcase_logger.info.call_args_list
        download_mock.assert_not_called()
        run_mock.assert_called()

    def test_run_tests_in_parallel_with_empty_settings_raises_value_error(self) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.configs_to_run = []
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act & Assert
        with pytest.raises(ValueError):
            runner.run_tests_in_parallel()

    def test_run_tests_in_parallel_with_ignore_check_if_log_file_exist(self) -> None:
        # Arrange
        log_folder_path = get_default_logging_folder_path()
        log_file_1 = os.path.join(log_folder_path, "Name_1", "Name_1.log")
        log_file_2 = os.path.join(log_folder_path, "Name_2", "Name_2.log")
        TestComparisonRunner.clean_empty_logs(log_file_1)
        TestComparisonRunner.clean_empty_logs(log_file_2)
        settings = TestBenchSettings()
        config1 = TestComparisonRunner.create_test_case_config("Name_1", True)
        config2 = TestComparisonRunner.create_test_case_config("Name_2", False)
        settings.configs_to_run = [config1, config2]
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_in_parallel()

        # Assert
        TestComparisonRunner.assertIsFile(log_file_1)
        TestComparisonRunner.assertIsFile(log_file_2)

    def test_run_without_test_cases_logs_no_results(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.config_file = "some.xml"
        settings.local_paths = LocalPaths()
        settings.parallel = False
        # settings.filter = "testcase=e02_f102_c02e_1d-precipitation123"
        logger = MagicMock(spec=ConsoleLogger)

        runner = ComparisonRunner(settings, logger)

        # Act
        with pytest.raises(ValueError):
            runner.run()

        # Assert
        assert (
            call(f"There are no test cases in '{settings.config_file}' with applied filter '{settings.filter}'.")
            in logger.error.call_args_list
        )

    def test_run_without_test_cases_due_to_filter_logs_no_results_with_filter_suggestion(
        self, mocker: MockerFixture
    ) -> None:
        # Arrange
        settings = TestBenchSettings()
        config1 = TestComparisonRunner.create_test_case_config("Banana_1", True)
        config2 = TestComparisonRunner.create_test_case_config("Banana_2", False)
        settings.config_file = "some.xml"
        settings.configs_from_xml = [config1, config2]
        settings.local_paths = LocalPaths()
        settings.parallel = False
        settings.filter = "testcase=Apple"
        logger = MagicMock(spec=ConsoleLogger)

        runner = ComparisonRunner(settings, logger)

        # Act
        if settings.filter != "":
            settings.configs_to_run = XmlConfigParser.filter_configs(settings.configs_from_xml, settings.filter, logger)

        with pytest.raises(ValueError):
            runner.run()

        # Assert
        assert (
            call(f"There are no test cases in '{settings.config_file}' with applied filter '{settings.filter}'.")
            in logger.error.call_args_list
        )

    @staticmethod
    def create_test_case_config(name: str, ignore: bool, type=PathType.REFERENCE) -> TestCaseConfig:
        config = TestCaseConfig()
        config.name = name
        config.ignore = ignore

        location1 = TestComparisonRunner.create_location(name, type)
        location2 = TestComparisonRunner.create_location(name, type)

        config.locations = [location1, location2]
        return config

    @staticmethod
    def create_location(name: str, type: PathType) -> Location:
        location1 = Location()
        location1.root = "https://deltares.nl/"
        location1.from_path = name.replace(" ", "")
        location1.type = type
        return location1

    @staticmethod
    def clean_empty_logs(filenames: str) -> None:
        try:
            for filename in glob.glob(filenames.split(".")[0]):
                os.remove(filename)
        except OSError:
            pass

    @staticmethod
    def assertIsFile(path: str) -> None:
        assert pl.Path(path).resolve().is_file(), f"File does not exist: {str(path)}"
