"""Reusable infrastructure for the dflowfm_io bindings.

Native-library loading, C return-code handling, and native handle management — the pieces any module
wrapping the shared library needs, not just the MDU one.
"""

from dflowfm_io.base.errors import (
    DFLOWFM_IO_RESULT_ERROR,
    DFLOWFM_IO_RESULT_SUCCESS,
    check_result,
)
from dflowfm_io.base.handle import HandleRef
from dflowfm_io.base.library import DLLFinder, dll_finder, lib

# Importing bindings applies the generated argtypes/restype to `lib` (generated from the C header).
from dflowfm_io.base import bindings  # noqa: E402,F401

__all__ = [
    "lib",
    "DLLFinder",
    "dll_finder",
    "check_result",
    "DFLOWFM_IO_RESULT_SUCCESS",
    "DFLOWFM_IO_RESULT_ERROR",
    "HandleRef",
]
