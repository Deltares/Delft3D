"""Logger interface.

Copyright (C)  Stichting Deltares, 2024
"""

from abc import ABC, abstractmethod

from src.utils.logging.log_level import LogLevel


class ILogger(ABC):
    """Interface for a logger."""

    @abstractmethod
    def error(self, message: str, exc_info: bool = False) -> None:
        """Log a error message.

        Parameters
        ----------
        message : str
            Message to log.
        exc_info : bool
            Log stacktrace if available (`False` by default).
        """

    @abstractmethod
    def exception(self, message: str) -> None:
        """Log an error message, with stacktrace if available.

        Parameters
        ----------
        message : str
            Message to log.
        """

    @abstractmethod
    def warning(self, message: str) -> None:
        """Log a warning message.

        Parameters
        ----------
        message : str
            Message to log.
        """

    @abstractmethod
    def info(self, message: str) -> None:
        """Log a info message.

        Parameters
        ----------
        message : str
            Message to log.
        """

    @abstractmethod
    def debug(self, message: str) -> None:
        """Log a debug message.

        Parameters
        ----------
        message : str
            Message to log.
        """

    @abstractmethod
    def log(self, message: str, log_level: LogLevel, exc_info: bool = False) -> None:
        """Log a message with the provided log level.

        Parameters
        ----------
        message : str
            Message to log.
        log_level : LogLevel
            Log level.
        exc_info : bool
            Log stacktrace if available (`False` by default).
        """
