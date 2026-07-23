"""Validation issue value types reported by the MDU document."""

from dataclasses import dataclass
from enum import IntEnum

# The ctypes wire form of an issue is the generated struct, so it is the exact type the generated
# argtypes expect (re-exported here under the wrapper's naming).
from dflowfm_io.base.bindings import mdu_issue_t as MduIssue  # noqa: F401


class Severity(IntEnum):
    """Severity of a validation issue."""

    INFO = 0
    WARNING = 1
    ERROR = 2


@dataclass
class Issue:
    """A single validation finding: a message with a severity and (optional) source line."""

    line_number: int
    severity: Severity
    message: str
