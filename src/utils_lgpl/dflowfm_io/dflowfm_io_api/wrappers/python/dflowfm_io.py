import ctypes
import os
import platform
from pathlib import Path


def _find_project_root():
    """Walk up from this file to find the top-level project root (contains CMakeLists.txt with 'project(')."""
    d = os.path.dirname(os.path.abspath(__file__))
    result = None
    while True:
        cmake_file = os.path.join(d, "CMakeLists.txt")
        if os.path.isfile(cmake_file):
            result = d
        parent = os.path.dirname(d)
        if parent == d:
            break
        d = parent
    return result


def _load_library():
    if platform.system() == "Windows":
        lib_name = "dflowfm_io_api.dll"
    else:
        lib_name = "libdflowfm_io_api.so"

    # Explicit override via environment variable
    lib_dir = os.environ.get("DFLOWFM_IO_LIB_DIR", "")
    if lib_dir:
        return ctypes.CDLL(os.path.join(lib_dir, lib_name))

    # Search common CMake build output directories
    project_root = _find_project_root()
    if project_root:
        search_dirs = [
            os.path.join(project_root, "build"),
            os.path.join(project_root, "build", "Debug"),
            os.path.join(project_root, "build", "Release"),
            os.path.join(project_root, "build", "RelWithDebInfo"),
            os.path.join(project_root, "build", "MinSizeRel"),
        ]
        for d in search_dirs:
            candidate = os.path.join(d, lib_name)
            if os.path.isfile(candidate):
                return ctypes.CDLL(candidate)

    return ctypes.CDLL(lib_name)


_lib = _load_library()

DFLOWFM_IO_RESULT_SUCCESS = 0
DFLOWFM_IO_RESULT_ERROR = 1


def _check_result(result):
    if result != DFLOWFM_IO_RESULT_SUCCESS:
        _lib.dflowfm_io_get_last_error.restype = ctypes.c_char_p    
        error_message = _lib.dflowfm_io_get_last_error()
        raise RuntimeError(error_message.decode('utf-8'))

class MduModel:
    def __init__(self):
        handle = ctypes.c_void_p()
        _check_result(_lib.mdu_model_create(ctypes.byref(handle)))
        self._handle = handle

    def __del__(self):
        if hasattr(self, "_handle") and self._handle:
            _lib.mdu_model_destroy(ctypes.byref(self._handle))
            self._handle = None

    def get_dummy_value(self) -> int:
        value = ctypes.c_int()
        _check_result(_lib.mdu_model_get_dummy_value(self._handle, ctypes.byref(value)))
        return value.value

    def load_from_file(self, filename: str) -> None:
        _check_result(_lib.mdu_model_load_from_file(self._handle, filename.encode("utf-8")))

    def load_from_lines(self, data: list[str]) -> None:
        encoded = "\n".join(data).encode("utf-8")
        _check_result(_lib.mdu_model_load_from_string(self._handle, encoded, len(encoded)))

    def save_to_file(self, filename: str) -> None:
        _check_result(_lib.mdu_model_save_to_file(self._handle, filename.encode("utf-8")))

    def save_to_lines(self) -> list[str]:
        string_out = ctypes.c_char_p()
        _check_result(_lib.mdu_model_save_to_string(self._handle, ctypes.byref(string_out)))
        return string_out.value.decode("utf-8").splitlines()

    def get_int(self, key: str) -> int:
        value = ctypes.c_int()
        _check_result(_lib.mdu_model_get_int(self._handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_double(self, key: str) -> float:
        value = ctypes.c_double()
        _check_result(_lib.mdu_model_get_double(self._handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value

    def get_string(self, key: str) -> str:
        string_out = ctypes.c_char_p()
        _check_result(_lib.mdu_model_get_string(self._handle, key.encode("utf-8"), ctypes.byref(string_out)))
        return string_out.value.decode("utf-8")

    def get_string_list(self, key: str) -> list[str]:
        array_out = ctypes.POINTER(ctypes.c_char_p)()
        size_out = ctypes.c_size_t()
        _check_result(_lib.mdu_model_get_string_list(self._handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [array_out[i].decode("utf-8") for i in range(size_out.value)]

    def get_bool(self, key: str) -> bool:
        value = ctypes.c_int()
        _check_result(_lib.mdu_model_get_bool(self._handle, key.encode("utf-8"), ctypes.byref(value)))
        return value.value != 0

    def get_path(self, key: str) -> Path:
        path_out = ctypes.c_char_p()
        _check_result(_lib.mdu_model_get_path(self._handle, key.encode("utf-8"), ctypes.byref(path_out)))
        return Path(path_out.value.decode("utf-8"))

    def get_path_list(self, key: str) -> list[Path]:
        array_out = ctypes.POINTER(ctypes.c_char_p)()
        size_out = ctypes.c_size_t()
        _check_result(_lib.mdu_model_get_path_list(self._handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [Path(array_out[i].decode("utf-8")) for i in range(size_out.value)]

    def set_int(self, key: str, value: int) -> None:
        _check_result(_lib.mdu_model_set_int(self._handle, key.encode("utf-8"), ctypes.c_int(value)))

    def set_bool(self, key: str, value: bool) -> None:
        _check_result(_lib.mdu_model_set_bool(self._handle, key.encode("utf-8"), ctypes.c_int(1 if value else 0)))

    def set_double(self, key: str, value: float) -> None:
        _check_result(_lib.mdu_model_set_double(self._handle, key.encode("utf-8"), ctypes.c_double(value)))

    def set_string(self, key: str, value: str) -> None:
        _check_result(_lib.mdu_model_set_string(self._handle, key.encode("utf-8"), value.encode("utf-8")))

    def set_path(self, key: str, value: Path | str) -> None:
        _check_result(_lib.mdu_model_set_path(self._handle, key.encode("utf-8"), str(value).encode("utf-8")))
