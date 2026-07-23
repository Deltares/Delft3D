"""Python bindings for the shared D-Flow FM IO library (``dflowfm_io_api``).

Importing this package loads the native library and exposes the public API:
:class:`MduDocument` (with its :class:`MduModel` and :class:`MduReport`) and the
:class:`Issue` / :class:`Severity` value types.
"""

from .document import MduDocument
from .issues import Issue, Severity
from .mdu import MduModel, MduReport

__all__ = ["MduDocument", "MduModel", "MduReport", "Issue", "Severity"]
