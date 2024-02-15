import pytest
import os
import pathlib as pl
import glob

from src.suite.comparison_runner import ComparisonRunner
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.common import get_default_logging_folder_path
from src.utils.logging.log_level import LogLevel
from src.utils.logging.console_logger import ConsoleLogger
from src.config.test_case_config import TestCaseConfig
from src.config.location import Location


class TestComparisonRunner:
    @staticmethod
    def test_run_tests_in_parallel_with_empty_settings_raises_value_error():
        # Arrange
        settings = TestBenchSettings()
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act & Assert
        with pytest.raises(ValueError):
            runner.run_tests_in_parallel()

    @staticmethod
    def test_run_tests_in_parallel_with_ignore_check_if_log_file_exist():
        # Arrange
        log_folder_path = get_default_logging_folder_path()
        log_file_1 = os.path.join(log_folder_path, "Name_1.log")
        log_file_2 = os.path.join(log_folder_path, "Name_2.log")
        TestComparisonRunner.clean_empty_logs(log_file_1)
        TestComparisonRunner.clean_empty_logs(log_file_2)
        settings = TestBenchSettings()
        config1 = TestComparisonRunner.create_test_case_config("Name_1", True)
        config2 = TestComparisonRunner.create_test_case_config("Name_2", False)
        settings.configs = [config1, config2]
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_in_parallel()

        # Assert
        TestComparisonRunner.assertIsFile(log_file_1)
        TestComparisonRunner.assertIsFile(log_file_2)

    @staticmethod
    def create_test_case_config(name: str, ignore: bool) -> TestCaseConfig:
        config = TestCaseConfig()
        config.name = name
        config.ignore = ignore

        location1 = TestComparisonRunner.create_location(name)
        location2 = TestComparisonRunner.create_location(name)

        config.locations = [location1, location2]
        return config

    @staticmethod
    def create_location(name):
        location1 = Location()
        location1.root = "https://deltares.nl/"
        location1.from_path = name.replace(' ', '')
        return location1

    @staticmethod
    def clean_empty_logs(filenames):
        try:
            for filename in glob.glob(filenames.split(".")[0]):
                os.remove(filename)
        except OSError:
            pass

    @staticmethod
    def assertIsFile(path):
        assert pl.Path(path).resolve().is_file(), f"File does not exist: {str(path)}"
