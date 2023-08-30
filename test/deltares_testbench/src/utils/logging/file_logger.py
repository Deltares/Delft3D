"""
Description: logger for logging to file
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import logging
import os
import sys
from logging import handlers

from src.utils.logging.i_logger import ILogger
from src.utils.logging.log_level import LogLevel


class FileLogger(ILogger):
    """Logger for logging messages to file

    Args:
        ILogger (ILogger): main logger interface
    """

    def __init__(self, log_level: LogLevel, name: str, path: str) -> None:
        self.__path = path
        level = self.__get_internal_log_level(log_level)

        logger = logging.getLogger(name)
        logger.propagate = False
        logger.setLevel(level)
        logger.isEnabledFor(level)

        logger.handlers.clear()
        logger.handlers.append(self.__create_file_logger(level))
        self.__logger = logger

    def error(self, message: str):
        self.__logger.error(message)
        sys.stderr.write(message)

    def warning(self, message: str):
        self.__logger.warning(message)

    def info(self, message: str):
        self.__logger.info(message)

    def debug(self, message: str):
        self.__logger.debug(message)

    def log(self, message: str, log_level: LogLevel):
        self.__logger.log(self.__get_internal_log_level(log_level), message)

    def __get_internal_log_level(self, log_level: LogLevel) -> int:
        return log_level

    def __create_file_logger(self, log_level: int):
        log_folder = os.path.dirname(self.__path)

        if not os.path.exists(log_folder):
            os.mkdir(log_folder)

        handler = handlers.RotatingFileHandler(self.__path, backupCount=10)
        handler.doRollover()
        handler.setLevel(log_level)

        format_str = "%(asctime)s [%(levelname)-7s] : %(message)s"

        handler.setFormatter(logging.Formatter(format_str))
        return handler
