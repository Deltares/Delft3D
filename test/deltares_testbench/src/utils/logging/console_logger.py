"""Logger for console.

Copyright (C)  Stichting Deltares, 2024
"""

import logging
import sys
import traceback

from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.file_test_logger import FileTestLogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger


class ConsoleLogger(IMainLogger):
    def __init__(self, log_level: LogLevel) -> None:
        self.__log_level = log_level
        self.__logger = self.__create_logger(log_level)

    @property
    def log_level(self) -> LogLevel:
        return self.__log_level

    def error(self, message: str, exc_info: bool = False) -> None:
        self.__base_log_message(message, logging.ERROR, exc_info=exc_info)
        sys.stderr.write(message + "\n")
        if exc_info:
            sys.stderr.write(traceback.format_exc())

    def exception(self, message: str) -> None:
        self.__base_log_message(message, logging.ERROR, exc_info=True)
        sys.stderr.write(message + "\n")
        sys.stderr.write(traceback.format_exc())

    def warning(self, message: str) -> None:
        self.__base_log_message(message, logging.WARNING)

    def info(self, message: str) -> None:
        self.__base_log_message(message, logging.INFO)

    def debug(self, message: str) -> None:
        self.__base_log_message(message, logging.DEBUG)

    def log(self, message: str, log_level: LogLevel, exc_info: bool = False) -> None:
        self.__base_log_message(message, log_level.value, exc_info=exc_info)

    def create_test_case_logger(self, test_case_id: str) -> ITestLogger:
        return FileTestLogger(self.log_level, test_case_id)

    def __base_log_message(self, message: str, log_level: int, exc_info: bool = False) -> None:
        self.__logger.log(log_level, message, exc_info=exc_info)

    def __create_logger(self, log_level: LogLevel) -> logging.Logger:
        logger = logging.getLogger(__name__)
        logger.setLevel(log_level.value)
        logger.isEnabledFor(log_level.value)

        logger.handlers.clear()
        logger.handlers.append(self.__create_console_handler(log_level.value))
        return logger

    def __create_console_handler(self, log_level: int) -> logging.Handler:
        if log_level <= logging.DEBUG:
            format_str = "%(asctime)s [%(levelname)-7s] : %(message)s (%(module)s.%(funcName)s)"
        else:
            format_str = "%(asctime)s [%(levelname)-7s] %(message)s"

        handler = logging.StreamHandler()
        handler.setLevel(log_level)
        handler.setFormatter(logging.Formatter(format_str))

        return handler
