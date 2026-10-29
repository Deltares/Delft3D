"""Python bindings for the shared D-Flow FM IO library (``dflowfm_io_api``).

Importing this package loads the native library and exposes the public API:
:class:`MduDocument` (with its :class:`MduModel`, :class:`MduReport`, and typed
:class:`MduSchema`) and the :class:`Issue` / :class:`Severity` value types.
"""

from dflowfm_io.issues import Issue, Severity
from dflowfm_io.mdu import MduDocument, MduModel, MduReport, MduSchema

__all__ = ["MduDocument", "MduModel", "MduReport", "MduSchema", "Issue", "Severity"]
