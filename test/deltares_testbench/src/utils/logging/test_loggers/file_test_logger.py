"""Test case logger for logging to file.

Copyright (C)  Stichting Deltares, 2024
"""

import os
from typing import Optional

from src.utils.common import get_default_logging_folder_path
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType


class FileTestLogger(FileLogger, ITestLogger):
    def __init__(self, log_level: LogLevel, test_case_id: str) -> None:
        path = os.path.join(get_default_logging_folder_path(), test_case_id, test_case_id + ".log")
        super().__init__(log_level, test_case_id, path)
        self.__test_case_id = test_case_id

    def test_started(self) -> None:
        self.info(f"Started test {self.__test_case_id}")

    def test_ignored(self) -> None:
        self.info(f"Ignored test {self.__test_case_id}")

    def test_finished(self) -> None:
        self.info(f"Finished test {self.__test_case_id}")

    def test_Result(
        self,
        result_type: TestResultType,
        error_message: Optional[str] = None,
    ) -> None:
        if result_type == TestResultType.Passed:
            self.info("Test passed = Comparison passed")
            return

        message = ""
        if result_type == TestResultType.Empty:
            message = "Test failed : Comparison: empty result"
        elif result_type == TestResultType.Error:
            message = f"Test failed : Comparison: Error occurred while comparing {error_message}"
        elif result_type == TestResultType.Differences:
            message = "Test failed : Comparison: differences above tolerance"
        elif result_type == TestResultType.Exception:
            message = f"Test failed : Exception occurred' details='{error_message}"

        self.error(message)
