"""
Description: logger interface
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from abc import ABC, abstractmethod

from src.utils.logging.log_level import LogLevel


class ILogger(ABC):
    """Interface for a logger"""

    @abstractmethod
    def error(self, message: str, exc_info: bool = False):
        """Logs a error message

        Args:
            message (str): message to log
        """

    @abstractmethod
    def warning(self, message: str):
        """Logs a warning message

        Args:
            message (str): message to log
        """

    @abstractmethod
    def info(self, message: str):
        """Logs a info message

        Args:
            message (str): message to log
        """

    @abstractmethod
    def debug(self, message: str):
        """Logs a debug message

        Args:
            message (str): message to log
        """

    @abstractmethod
    def log(self, message: str, log_level: LogLevel):
        """Logs a message with the provided log level

        Args:
            message (str): message to log
            log_level (LogLevel): log level.
        """
