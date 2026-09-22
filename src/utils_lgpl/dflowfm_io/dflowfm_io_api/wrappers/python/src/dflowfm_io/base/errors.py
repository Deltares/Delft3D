"""Translating dflowfm_io_api C return codes into Python exceptions."""

from dflowfm_io.base.library import lib

DFLOWFM_IO_RESULT_SUCCESS = 0
DFLOWFM_IO_RESULT_ERROR = 1


def check_result(result):
    """Raise ``RuntimeError`` with the backend's last error message on a non-success result."""
    if result != DFLOWFM_IO_RESULT_SUCCESS:
        # restype for dflowfm_io_get_last_error is applied at import by base.bindings.
        error_message = lib.dflowfm_io_get_last_error()
        # The C side may return NULL when no message is set; don't mask the failure with an
        # AttributeError from decoding None.
        raise RuntimeError((error_message or b"unknown dflowfm_io error").decode("utf-8"))
