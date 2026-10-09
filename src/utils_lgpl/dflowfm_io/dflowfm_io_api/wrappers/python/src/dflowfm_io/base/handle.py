"""Opaque handle to a native MDU document."""

import ctypes


class HandleRef:
    """Holds the native document handle and keeps a reference to its owner alive."""

    def __init__(self, handle: ctypes.c_void_p, owner: object):
        self.handle = handle
        self._owner = owner
