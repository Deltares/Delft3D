"""
Description: interface extending ILogger interface with the option to create test case loggers
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from abc import abstractmethod

from src.utils.logging.i_logger import ILogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger


class IMainLogger(ILogger):
    @abstractmethod
    def create_test_case_logger(self, test_case_id: str) -> ITestLogger:
        """Creates an new instance of a logger for a specific test case
        Args:
            test_case_id (str): id of the test (usually the name)
        Returns:
            ITestLogger: created test logger
        """
