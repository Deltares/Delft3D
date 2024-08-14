"""Logger level.

Copyright (C)  Stichting Deltares, 2024
"""

from enum import IntEnum


class LogLevel(IntEnum):
    """Level of logging."""

    CRITICAL = 50
    FATAL = CRITICAL
    ERROR = 40
    WARNING = 30
    INFO = 20
    DEBUG = 10
    NOTSET = 0
