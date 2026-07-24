"""GENERATED from dflowfm_io_api.h by codegen/generate_bindings.py - do not edit.

The typed MduModel: one get/set method per mdu_get_*/mdu_set_* C function, with the per-type
value marshalling. Regenerated from the C header, so the accessors cannot drift from the ABI.
"""

import ctypes
from datetime import datetime, timezone
from pathlib import Path

from dflowfm_io.base import HandleRef, check_result, lib


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
        value = ctypes.c_char_p()
        check_result(lib.mdu_get_string(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value.decode("utf-8")

    def get_path(self, key: str) -> Path:
        value = ctypes.c_char_p()
        check_result(lib.mdu_get_path(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return Path(value.value.decode("utf-8"))

    def get_datetime(self, key: str) -> datetime:
        value = ctypes.c_int64()
        check_result(lib.mdu_get_datetime(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return datetime.fromtimestamp(value.value, tz=timezone.utc)

    def get_enum(self, key: str) -> int:
        value = ctypes.c_int32()
        check_result(lib.mdu_get_enum(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_enum_name(self, key: str) -> str:
        value = ctypes.c_char_p()
        check_result(lib.mdu_get_enum_name(self._ref.handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value.decode("utf-8")

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
        check_result(lib.mdu_set_datetime(self._ref.handle, key.encode("utf-8"), ctypes.c_int64(int((value if value.tzinfo else value.replace(tzinfo=timezone.utc)).timestamp()))))

    def set_enum(self, key: str, value: int) -> None:
        check_result(lib.mdu_set_enum(self._ref.handle, key.encode("utf-8"), ctypes.c_int32(value)))

    def set_enum_name(self, key: str, value: str) -> None:
        check_result(lib.mdu_set_enum_name(self._ref.handle, key.encode("utf-8"), value.encode("utf-8")))

    def set_string_list(self, key: str, values: list[str]) -> None:
        encoded = [v.encode("utf-8") for v in values]
        arr = (ctypes.c_char_p * len(encoded))(*encoded)
        check_result(lib.mdu_set_string_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(encoded))))

    def set_path_list(self, key: str, values: list[Path | str]) -> None:
        encoded = [str(v).encode("utf-8") for v in values]
        arr = (ctypes.c_char_p * len(encoded))(*encoded)
        check_result(lib.mdu_set_path_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(encoded))))

    def set_double_list(self, key: str, values: list[float]) -> None:
        encoded = list(values)
        arr = (ctypes.c_double * len(encoded))(*encoded)
        check_result(lib.mdu_set_double_list(self._ref.handle, key.encode("utf-8"), arr, ctypes.c_uint64(len(encoded))))
