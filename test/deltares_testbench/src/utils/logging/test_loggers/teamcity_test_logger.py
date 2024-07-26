"""Test case logger for teamcity.

Copyright (C)  Stichting Deltares, 2024
"""

import datetime
import sys
import traceback
from typing import List, Optional

from src.utils.common import escape_teamcity
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType


class TeamcityTestLogger(ITestLogger):
    """Logs test case information for teamcity."""

    def __init__(self, test_name: str, log_level: LogLevel) -> None:
        self.__test_name = test_name
        self.__flow_id = test_name
        self.__log_level = log_level

    def error(self, message: str, exc_info=False) -> None:
        self.log(message, LogLevel.ERROR, exc_info=exc_info)

    def exception(self, message: str) -> None:
        self.log(message, LogLevel.ERROR, exc_info=True)

    def warning(self, message: str) -> None:
        self.log(message, LogLevel.WARNING)

    def info(self, message: str) -> None:
        self.log(message, LogLevel.INFO)

    def debug(self, message: str) -> None:
        self.log(message, LogLevel.DEBUG)

    def log(self, message: str, log_level: LogLevel, exc_info: bool = False) -> None:
        if self.__log_level > log_level:
            return

        status = self.__get_status(log_level)

        extra_tags = [f"status='{status}'"]
        if exc_info:
            stack_trace = traceback.format_exc()
            extra_tags.append(f"errorDetails='{escape_teamcity(stack_trace)}'")
        self.write_tc_message(
            "message",
            f"{str(log_level)} : {message}",
            extra_tags=extra_tags,
        )

    def test_started(self) -> None:
        self.write_tc_message("testStarted")

    def test_ignored(self) -> None:
        self.write_tc_message("testIgnored")

    def test_finished(self) -> None:
        self.write_tc_message("testFinished")

    def test_Result(
        self,
        result_type: TestResultType,
        error_message: Optional[str] = None,
    ) -> None:
        if result_type == TestResultType.Empty:
            self.write_tc_message("testFailed", "Comparison: empty result")
        elif result_type == TestResultType.Error:
            self.write_tc_message(
                "testFailed",
                f"Comparison: Error occurred while comparing {error_message}",
            )
        elif result_type == TestResultType.Differences:
            self.write_tc_message("testFailed", "Comparison: differences above tolerance")
        elif result_type == TestResultType.Exception:
            escaped_message = escape_teamcity(error_message or "")
            self.write_tc_message(
                "testFailed",
                "Exception occurred",
                extra_tags=[f"details='{escaped_message}'"],
            )
        elif result_type == TestResultType.Passed:
            self.write_tc_message("testFinished", "Comparison passed")

    def write_tc_message(
        self,
        command: str,
        message: Optional[str] = None,
        extra_tags: Optional[List[str]] = None,
    ) -> None:
        time_str = datetime.datetime.now().isoformat(timespec="milliseconds")
        tc_message = (
            f"##teamcity[{command} "
            + f"name='{self.__test_name}' "
            + f"flowId='{self.__flow_id}' "
            + f"timestamp='{time_str}' "
        )

        if extra_tags:
            tc_message += " ".join(extra_tags) + " "

        if message:
            escaped_message = escape_teamcity(message)
            tc_message += f"text='{escaped_message}' "

        if command == "testFailed" and message:
            escaped_message = escape_teamcity(message)
            tc_message += f"message='{escaped_message}' "

        tc_message += "]"

        sys.stdout.write(tc_message)

    def __get_status(self, level: LogLevel) -> str:
        if level == LogLevel.ERROR or level == LogLevel.CRITICAL:
            return "ERROR"
        if level == LogLevel.FATAL:
            return "FAILURE"
        if level == LogLevel.WARNING:
            return "WARNING"

        return "NORMAL"
