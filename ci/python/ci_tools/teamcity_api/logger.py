from datetime import datetime, timezone
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

    def _escape_teamcity(self, text: str) -> str:
        """Escape special characters for TeamCity service messages."""
        return (
            text.replace("|", "||")
            .replace("'", "|'")
            .replace("\n", "|n")
            .replace("\r", "|r")
            .replace("[", "|[")
            .replace("]", "|]")
        )

    def log(self, message: str, severity: LogLevel = LogLevel.NORMAL) -> None:
        """Log a message with a severity level."""
        date_time_string = datetime.now(tz=timezone.utc).strftime("%m/%d/%Y:%H:%M:%S")
        if self.run_on_teamcity:
            escaped_message = self._escape_teamcity(message)
            print(f"##teamcity[message text='{escaped_message}' status='{severity.name}']")
        else:
            print(f"[{date_time_string}] {severity.name:<7} {message}")
