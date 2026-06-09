import ctypes
import os
import platform


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

_lib.mdu_model_create.restype = ctypes.c_int
_lib.mdu_model_create.argtypes = [ctypes.POINTER(ctypes.c_void_p)]

_lib.mdu_model_destroy.restype = ctypes.c_int
_lib.mdu_model_destroy.argtypes = [ctypes.POINTER(ctypes.c_void_p)]

_lib.mdu_model_get_dummy_value.restype = ctypes.c_int
_lib.mdu_model_get_dummy_value.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_int)]

_lib.mdu_model_get_string.restype = ctypes.c_int
_lib.mdu_model_get_string.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]

_lib.mdu_model_get_string_list.restype = ctypes.c_int
_lib.mdu_model_get_string_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_char_p)), ctypes.POINTER(ctypes.c_size_t)]


def _check_result(result):
    if result != DFLOWFM_IO_RESULT_SUCCESS:
        raise RuntimeError("dflowfm_io call failed")


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

    def get_string_value(self, key: str) -> str:
        string_out = ctypes.c_char_p()
        _check_result(_lib.mdu_model_get_string(self._handle, key.encode("utf-8"), ctypes.byref(string_out)))
        return string_out.value.decode("utf-8")

    def get_string_list(self, key: str) -> list[str]:
        array_out = ctypes.POINTER(ctypes.c_char_p)()
        size_out = ctypes.c_size_t()
        _check_result(_lib.mdu_model_get_string_list(self._handle, key.encode("utf-8"), ctypes.byref(array_out), ctypes.byref(size_out)))
        return [array_out[i].decode("utf-8") for i in range(size_out.value)]
