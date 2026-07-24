"""GENERATED from dflowfm_io_api.h by codegen/generate_bindings.py - do not edit.

The ctypes ABI mirror of the dflowfm_io_api C header: enum constants, the mdu_issue_t struct,
and argtypes/restype for every exported function, applied to the loaded library.
"""

import ctypes

from dflowfm_io.base.library import lib

# --- enum mdu_severity_t ---
MDU_SEVERITY_INFO = 0
MDU_SEVERITY_WARNING = 1
MDU_SEVERITY_ERROR = 2


# --- struct mdu_issue_t ---
class mdu_issue_t(ctypes.Structure):
    _fields_ = [
        ("line_number", ctypes.c_int32),
        ("severity", ctypes.c_int32),
        ("message", ctypes.c_char_p),
    ]


# --- function signatures ---
lib.dflowfm_io_get_last_error.argtypes = []
lib.dflowfm_io_get_last_error.restype = ctypes.c_char_p
lib.mdu_create.argtypes = [ctypes.POINTER(ctypes.c_void_p)]
lib.mdu_create.restype = ctypes.c_int32
lib.mdu_destroy.argtypes = [ctypes.POINTER(ctypes.c_void_p)]
lib.mdu_destroy.restype = ctypes.c_int32
lib.mdu_load_from_file.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
lib.mdu_load_from_file.restype = ctypes.c_int32
lib.mdu_load_from_string.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_uint64]
lib.mdu_load_from_string.restype = ctypes.c_int32
lib.mdu_save_to_file.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
lib.mdu_save_to_file.restype = ctypes.c_int32
lib.mdu_save_to_string.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p)]
lib.mdu_save_to_string.restype = ctypes.c_int32
lib.mdu_get_int.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)]
lib.mdu_get_int.restype = ctypes.c_int32
lib.mdu_get_bool.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)]
lib.mdu_get_bool.restype = ctypes.c_int32
lib.mdu_get_double.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_double)]
lib.mdu_get_double.restype = ctypes.c_int32
lib.mdu_get_string.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
lib.mdu_get_string.restype = ctypes.c_int32
lib.mdu_get_path.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
lib.mdu_get_path.restype = ctypes.c_int32
lib.mdu_get_datetime.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int64)]
lib.mdu_get_datetime.restype = ctypes.c_int32
lib.mdu_get_enum.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)]
lib.mdu_get_enum.restype = ctypes.c_int32
lib.mdu_get_enum_name.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
lib.mdu_get_enum_name.restype = ctypes.c_int32
lib.mdu_get_string_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_char_p)), ctypes.POINTER(ctypes.c_uint64)]
lib.mdu_get_string_list.restype = ctypes.c_int32
lib.mdu_get_path_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_char_p)), ctypes.POINTER(ctypes.c_uint64)]
lib.mdu_get_path_list.restype = ctypes.c_int32
lib.mdu_get_double_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_double)), ctypes.POINTER(ctypes.c_uint64)]
lib.mdu_get_double_list.restype = ctypes.c_int32
lib.mdu_set_int.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int32]
lib.mdu_set_int.restype = ctypes.c_int32
lib.mdu_set_bool.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int32]
lib.mdu_set_bool.restype = ctypes.c_int32
lib.mdu_set_double.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_double]
lib.mdu_set_double.restype = ctypes.c_int32
lib.mdu_set_string.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_char_p]
lib.mdu_set_string.restype = ctypes.c_int32
lib.mdu_set_path.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_char_p]
lib.mdu_set_path.restype = ctypes.c_int32
lib.mdu_set_datetime.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int64]
lib.mdu_set_datetime.restype = ctypes.c_int32
lib.mdu_set_enum.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int32]
lib.mdu_set_enum.restype = ctypes.c_int32
lib.mdu_set_enum_name.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_char_p]
lib.mdu_set_enum_name.restype = ctypes.c_int32
lib.mdu_set_string_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
lib.mdu_set_string_list.restype = ctypes.c_int32
lib.mdu_set_path_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
lib.mdu_set_path_list.restype = ctypes.c_int32
lib.mdu_set_double_list.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_double), ctypes.c_uint64]
lib.mdu_set_double_list.restype = ctypes.c_int32
lib.mdu_get_issue_list.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.POINTER(mdu_issue_t)), ctypes.POINTER(ctypes.c_uint64)]
lib.mdu_get_issue_list.restype = ctypes.c_int32
