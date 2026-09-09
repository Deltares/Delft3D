"""Locating and loading the native dflowfm_io_api library.

Importing this module loads the shared library once and exposes it as :data:`lib` — a single
process-wide :class:`Lib` handle that every other module (``bindings``, ``errors``, ``mdu``)
shares. See :class:`LibLoader` for where it is loaded from.
"""

import ctypes
import os
import platform

from dflowfm_io._lib.library_path import DFLOWFM_LIBRARY_PATH

class LibLoader:
    def __init__(self):
        self._loaded: ctypes.CDLL | None = None

    def load(self) -> ctypes.CDLL:
        """Load the native library (but only once!) and return the shared :class:`ctypes.CDLL` handle."""
        if self._loaded is None:
            # Make sibling runtime DLLs next to the library discoverable (Windows).
            if platform.system() == "Windows" and hasattr(os, "add_dll_directory"):
                os.add_dll_directory(str(DFLOWFM_LIBRARY_PATH.parent))
            self._loaded = ctypes.CDLL(str(DFLOWFM_LIBRARY_PATH))
        return self._loaded


class Lib:
    """The loaded native dflowfm_io_api library.

    A thin wrapper over the underlying :class:`ctypes.CDLL`: attribute access forwards to it, so
    ``lib.mdu_get_int(...)`` calls the C function and ``lib.mdu_get_int.argtypes = [...]`` configures
    it — exactly as on a raw ``CDLL``. Instances *are* the library, hence ``lib = Lib()``.
    """

    def __init__(self, loader: LibLoader | None = None):
        self.loader = loader or LibLoader()
        self._cdll = self.loader.load()

    @property
    def cdll(self) -> ctypes.CDLL:
        """The underlying ctypes handle."""
        return self._cdll

    def __getattr__(self, name: str):
        # Reached only for names not found on the wrapper itself (the C symbols); forward to the CDLL.
        # Guard `_cdll` to avoid infinite recursion before it is assigned in __init__.
        if name == "_cdll":
            raise AttributeError(name)
        return getattr(self._cdll, name)


lib = Lib()
