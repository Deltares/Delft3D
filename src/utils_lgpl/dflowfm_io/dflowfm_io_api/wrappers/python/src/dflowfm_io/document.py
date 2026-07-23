"""The top-level MDU document: load, save, and access its model and report."""

import ctypes

from .errors import check_result
from .handle import HandleRef
from .library import lib
from .mdu import MduModel, MduReport


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
