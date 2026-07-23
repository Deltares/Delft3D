"""Validation issue value types reported by the MDU document."""

import ctypes
from dataclasses import dataclass
from enum import IntEnum


class Severity(IntEnum):
    """Severity of a validation issue."""

    INFO = 0
    WARNING = 1
    ERROR = 2


class MduIssue(ctypes.Structure):
    """ctypes mirror of the C ``mdu_issue_t`` struct (the wire form of an :class:`Issue`)."""

    _fields_ = [
        ("line_number", ctypes.c_int32),
        ("severity", ctypes.c_int32),
        ("message", ctypes.c_char_p),
    ]


@dataclass
class Issue:
    """A single validation finding: a message with a severity and (optional) source line."""

    line_number: int
    severity: Severity
    message: str
