"""Translating dflowfm_io_api C return codes into Python exceptions."""

import ctypes

from dflowfm_io.base.library import lib

DFLOWFM_IO_RESULT_SUCCESS = 0
DFLOWFM_IO_RESULT_ERROR = 1


def check_result(result):
    """Raise ``RuntimeError`` with the backend's last error message on a non-success result."""
    if result != DFLOWFM_IO_RESULT_SUCCESS:
        lib.dflowfm_io_get_last_error.restype = ctypes.c_char_p
        error_message = lib.dflowfm_io_get_last_error()
        raise RuntimeError(error_message.decode("utf-8"))
