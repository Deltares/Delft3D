"""The MDU document (:class:`MduDocument`) and its validation issues (:class:`MduReport`).

The typed property access (:class:`MduModel`) is generated from the C header (see
:mod:`dflowfm_io.mdu.model`) and re-exported here. The document lifecycle and the report stay
hand-written: they encode Python design (handle ownership, ``__del__``, convenience methods) with
no 1:1 C-function counterpart.
"""

import ctypes

from dflowfm_io.base import HandleRef, check_result, lib
from dflowfm_io.issues import Issue, MduIssue, Severity
from dflowfm_io.mdu.model import MduModel
from dflowfm_io.mdu.schema import MduSchema

__all__ = ["MduModel", "MduReport", "MduDocument", "MduSchema"]


class MduReport:
    """The validation issues (info/warning/error) collected while loading an MDU document."""

    def __init__(self, ref: HandleRef):
        self._ref = ref

    def get_issues(self) -> list[Issue]:
        array_out = ctypes.POINTER(MduIssue)()
        size_out = ctypes.c_uint64()
        check_result(lib.mdu_get_issue_list(self._ref.handle, ctypes.byref(array_out), ctypes.byref(size_out)))
        issues = []
        for i in range(size_out.value):
            raw = array_out[i]
            message = raw.message.decode("utf-8") if raw.message else ""
            issues.append(Issue(raw.line_number, Severity(raw.severity), message))
        return issues

    def has_errors(self) -> bool:
        return any(issue.severity == Severity.ERROR for issue in self.get_issues())

    def print_overview(self) -> None:
        for issue in self.get_issues():
            location = f"line {issue.line_number}" if issue.line_number >= 0 else "no line"
            print(f"[{issue.severity.name}] ({location}) {issue.message}")


class MduDocument:
    """An MDU document — create/load/save, with typed access via :attr:`model` and :attr:`report`."""

    def __init__(self):
        handle = ctypes.c_void_p()
        check_result(lib.mdu_create(ctypes.byref(handle)))
        self._ref = HandleRef(handle, self)
        self._model = MduModel(self._ref)
        self._report = MduReport(self._ref)
        self._schema = MduSchema(self._model)

    def __del__(self):
        if hasattr(self, "_ref") and self._ref.handle:
            lib.mdu_destroy(ctypes.byref(self._ref.handle))
            self._ref.handle = None

    @property
    def model(self) -> MduModel:
        return self._model

    @property
    def report(self) -> MduReport:
        return self._report

    @property
    def schema(self) -> MduSchema:
        """Typed, per-keyword access to the MDU sections (Layer 2), e.g. ``doc.schema.geometry.netFile``."""
        return self._schema

    def load_from_file(self, filename: str) -> None:
        check_result(lib.mdu_load_from_file(self._ref.handle, filename.encode("utf-8")))

    def load_from_lines(self, data: list[str]) -> None:
        encoded = "\n".join(data).encode("utf-8")
        check_result(lib.mdu_load_from_string(self._ref.handle, encoded, ctypes.c_uint64(len(encoded))))

    def save_to_file(self, filename: str) -> None:
        check_result(lib.mdu_save_to_file(self._ref.handle, filename.encode("utf-8")))

    def save_to_lines(self) -> list[str]:
        string_out = ctypes.c_char_p()
        check_result(lib.mdu_save_to_string(self._ref.handle, ctypes.byref(string_out)))
        return string_out.value.decode("utf-8").splitlines()
