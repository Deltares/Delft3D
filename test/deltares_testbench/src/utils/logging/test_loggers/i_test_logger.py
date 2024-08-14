"""Interface for test case logger.

Copyright (C)  Stichting Deltares, 2024
"""

from abc import abstractmethod
from typing import Optional

from src.utils.logging.i_logger import ILogger
from src.utils.logging.test_loggers.test_result_type import TestResultType


class ITestLogger(ILogger):
    """interface for test case logger."""

    @abstractmethod
    def test_started(self):
        """Logs that a test has started."""

    @abstractmethod
    def test_ignored(self):
        """Logs that a test is ignored."""

    @abstractmethod
    def test_finished(self):
        """Logs that a test has finished."""

    @abstractmethod
    def test_Result(
        self,
        result_type: TestResultType,
        error_message: Optional[str] = None,
    ):
        """Logs the result of a test."""
