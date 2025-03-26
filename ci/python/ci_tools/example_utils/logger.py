from enum import Enum


class LogLevel(Enum):
    """Level of logging."""

    NORMAL = 1
    WARNING = 2
    FAILURE = 3
    ERROR = 4


class Logger:
    """Logger class to handle logging with optional TeamCity integration."""

    def __init__(self, run_on_teamcity: bool = False) -> None:
        self.run_on_teamcity = run_on_teamcity

    def log(self, message: str, severity: LogLevel = LogLevel.NORMAL) -> None:
        """Log a message with a severity level."""
        if self.run_on_teamcity:
            print(f"##teamcity[message text='{message}' status='{severity.name}']")
        else:
            if severity == LogLevel.NORMAL:
                print(message)
            else:
                print(f"{severity.name}: {message}")
