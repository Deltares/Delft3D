"""Logger for logging to file.

Copyright (C)  Stichting Deltares, 2024
"""

import logging
import os
import sys
import traceback
from logging import handlers

from src.utils.logging.i_logger import ILogger
from src.utils.logging.log_level import LogLevel


class FileLogger(ILogger):
    """Logger for logging messages to file."""

    def __init__(self, log_level: LogLevel, name: str, path: str) -> None:
        self.__path = path
        level = log_level.value

        logger = logging.getLogger(name)
        logger.propagate = False
        logger.setLevel(level)
        logger.isEnabledFor(level)

        logger.handlers.clear()
        logger.handlers.append(self.__create_file_logger(level))
        self.__logger = logger

    def error(self, message: str, exc_info: bool = False) -> None:
        self.__logger.error(message, exc_info=exc_info)
        sys.stderr.write(message + "\n")
        if exc_info:
            sys.stderr.write(traceback.format_exc())

    def exception(self, message: str) -> None:
        self.__logger.exception(message)
        sys.stderr.write(message + "\n")
        sys.stderr.write(traceback.format_exc())

    def warning(self, message: str) -> None:
        self.__logger.warning(message)

    def info(self, message: str) -> None:
        self.__logger.info(message)

    def debug(self, message: str) -> None:
        self.__logger.debug(message)

    def log(self, message: str, log_level: LogLevel, exc_info: bool = False) -> None:
        self.__logger.log(log_level.value, message, exc_info=exc_info)

    def close(self) -> None:
        """Close all handlers of the logger."""
        for handler in self.__logger.handlers:
            handler.close()
            self.__logger.removeHandler(handler)

    def __create_file_logger(self, log_level: int):
        log_folder = os.path.dirname(self.__path)
        os.makedirs(log_folder, exist_ok=True)

        if os.path.isfile(self.__path):
            handler = handlers.RotatingFileHandler(self.__path, backupCount=10)
            handler.doRollover()
        else:
            handler = handlers.RotatingFileHandler(self.__path, backupCount=10)

        handler.setLevel(log_level)
        format_str = "%(asctime)s [%(levelname)-7s] : %(message)s"
        handler.setFormatter(logging.Formatter(format_str))
        return handler
