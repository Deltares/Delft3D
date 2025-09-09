from enum import StrEnum
from typing import Any


class ErrorCode(StrEnum):
    """Error code used in the `MinioToolError` base exception class."""

    AUTH = "auth"
    CLI_ARGS = "cli_args"
    UNKNOWN = "unknown"


class MinioToolError(Exception):
    """Base class for errors in the `MinioTool`."""

    def __init__(self, message: str, code: ErrorCode = ErrorCode.UNKNOWN) -> None:
        super().__init__(message)
        self.code = code
        self.message = message


class MinioAuthError(MinioToolError):
    """Authentication or authorization error."""

    def __init__(self, message: str) -> None:
        super().__init__(message, ErrorCode.AUTH)


class MinioCliArgsError(MinioToolError):
    """Error in command line arguments."""

    def __init__(self, message: str, cli_args: dict[str, Any]) -> None:
        super().__init__(message, ErrorCode.CLI_ARGS)
        self.cli_args = cli_args
