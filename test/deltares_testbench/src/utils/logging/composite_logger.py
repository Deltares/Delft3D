"""
Description: logger for combining multiple loggers
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from typing import Iterable, List

from src.utils.logging.i_logger import ILogger
from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.i_test_logger import ITestLogger


class CompositeLogger(IMainLogger):
    """Logger for combining multiple loggers"""

    def __init__(self, loggers: Iterable[ILogger]) -> None:
        self.__loggers = list(loggers)

    @property
    def loggers(self) -> List[ILogger]:
        """Loggers of this composite logger"""
        return self.__loggers

    def error(self, message: str, exc_info: bool = False):
        self.log(message, LogLevel.ERROR, exc_info=exc_info)

    def exception(self, message: str):
        self.log(message, LogLevel.ERROR, exc_info=True)

    def warning(self, message: str):
        self.log(message, LogLevel.WARNING)

    def info(self, message: str):
        self.log(message, LogLevel.INFO)

    def debug(self, message: str):
        self.log(message, LogLevel.DEBUG)

    def log(self, message: str, log_level: LogLevel, exc_info: bool = False):
        for logger in self.__loggers:
            logger.log(message, log_level, exc_info=exc_info)

    def create_test_case_logger(self, test_case_id: str) -> ITestLogger:
        for logger in self.loggers:
            if isinstance(logger, IMainLogger):
                return logger.create_test_case_logger(test_case_id)
