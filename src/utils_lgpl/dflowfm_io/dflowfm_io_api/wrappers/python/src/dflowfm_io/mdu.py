"""The MDU document (:class:`MduDocument`) and its typed data (:class:`MduModel`) and validation
issues (:class:`MduReport`)."""

import ctypes
from datetime import datetime, timezone
from pathlib import Path

from dflowfm_io.base import HandleRef, check_result, lib
from dflowfm_io.issues import Issue, MduIssue, Severity


class MduModel:
    """Typed get/set access to MDU properties, keyed by dotted ``section.property`` names."""

    def __init__(self, ref: HandleRef):
        self._ref = ref

    def get_int(self, key: str) -> int:
        value = ctypes.c_int32()
        check_result(lib.mdu_get_int(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_bool(self, key: str) -> bool:
        value = ctypes.c_int32()
        check_result(lib.mdu_get_bool(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value != 0

    def get_double(self, key: str) -> float:
        value = ctypes.c_double()
        check_result(lib.mdu_get_double(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_string(self, key: str) -> str:
        string_out = ctypes.c_char_p()
        check_result(lib.mdu_get_string(self._ref.handle, key.encode("utf-8"), ctypes.byref(string_out)))
        return string_out.value.decode("utf-8")

    def get_path(self, key: str) -> Path:
        path_out = ctypes.c_char_p()
        check_result(lib.mdu_get_path(self._ref.handle, key.encode("utf-8"), ctypes.byref(path_out)))
        return Path(path_out.value.decode("utf-8"))

    def get_datetime(self, key: str) -> datetime:
        epoch_out = ctypes.c_int64()
        check_result(lib.mdu_get_datetime(self._ref.handle, key.encode("utf-8"), ctypes.byref(epoch_out)))
        return datetime.fromtimestamp(epoch_out.value, tz=timezone.utc)

    def get_enum(self, key: str) -> int:
        value = ctypes.c_int32()
        check_result(lib.mdu_get_enum(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_string_list(self, key: str) -> list[str]:
        array_out = ctypes.POINTER(ctypes.c_char_p)()
        size_out = ctypes.c_uint64()
        check_result(lib.mdu_get_string_list(self._ref.handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [array_out[i].decode("utf-8") for i in range(size_out.value)]

    def get_path_list(self, key: str) -> list[Path]:
        array_out = ctypes.POINTER(ctypes.c_char_p)()
        size_out = ctypes.c_uint64()
        check_result(lib.mdu_get_path_list(self._ref.handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [Path(array_out[i].decode("utf-8")) for i in range(size_out.value)]

    def get_double_list(self, key: str) -> list[float]:
        array_out = ctypes.POINTER(ctypes.c_double)()
        size_out = ctypes.c_uint64()
        check_result(lib.mdu_get_double_list(self._ref.handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [array_out[i] for i in range(size_out.value)]

    def set_int(self, key: str, value: int) -> None:
        check_result(lib.mdu_set_int(self._ref.handle, key.encode("utf-8"), ctypes.c_int32(value)))

    def set_bool(self, key: str, value: bool) -> None:
        check_result(lib.mdu_set_bool(self._ref.handle, key.encode("utf-8"), ctypes.c_int32(1 if value else 0)))

    def set_double(self, key: str, value: float) -> None:
        check_result(lib.mdu_set_double(self._ref.handle, key.encode("utf-8"), ctypes.c_double(value)))

    def set_string(self, key: str, value: str) -> None:
        check_result(lib.mdu_set_string(self._ref.handle, key.encode("utf-8"), value.encode("utf-8")))

    def set_path(self, key: str, value: Path | str) -> None:
        check_result(lib.mdu_set_path(self._ref.handle, key.encode("utf-8"), str(value).encode("utf-8")))

    def set_datetime(self, key: str, value: datetime) -> None:
        epoch = int(value.timestamp())
        check_result(lib.mdu_set_datetime(self._ref.handle, key.encode("utf-8"), ctypes.c_int64(epoch)))

    def set_enum(self, key: str, value: int) -> None:
        check_result(lib.mdu_set_enum(self._ref.handle, key.encode("utf-8"), ctypes.c_int32(value)))

    def set_string_list(self, key: str, values: list[str]) -> None:
        encoded = [v.encode("utf-8") for v in values]
        arr = (ctypes.c_char_p * len(encoded))(*encoded)
        check_result(lib.mdu_set_string_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(encoded))))

    def set_path_list(self, key: str, values: list[Path | str]) -> None:
        encoded = [str(v).encode("utf-8") for v in values]
        arr = (ctypes.c_char_p * len(encoded))(*encoded)
        check_result(lib.mdu_set_path_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(encoded))))

    def set_double_list(self, key: str, values: list[float]) -> None:
        arr = (ctypes.c_double * len(values))(*values)
        check_result(lib.mdu_set_double_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(values))))


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
