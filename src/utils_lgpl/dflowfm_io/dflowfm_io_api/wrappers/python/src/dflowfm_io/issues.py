"""Validation issue value types reported by the MDU document."""

from dataclasses import dataclass
from enum import IntEnum

from dflowfm_io.base import bindings

# The ctypes wire form of an issue is the generated struct, so it is the exact type the generated
# argtypes expect (re-exported here under the wrapper's naming).
MduIssue = bindings.mdu_issue_t


class Severity(IntEnum):
    """Severity of a validation issue."""

    INFO = bindings.MDU_SEVERITY_INFO
    WARNING = bindings.MDU_SEVERITY_WARNING
    ERROR = bindings.MDU_SEVERITY_ERROR


@dataclass
class Issue:
    """A single validation finding: a message with a severity and (optional) source line."""

    line_number: int
    severity: Severity
    message: str
