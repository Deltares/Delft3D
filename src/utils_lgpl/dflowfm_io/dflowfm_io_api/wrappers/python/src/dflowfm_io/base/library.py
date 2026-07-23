"""Locating and loading the native dflowfm_io_api library.

Importing this module loads the shared library once and exposes it as :data:`lib` — a single
process-wide :class:`Lib` handle that every other module (``bindings``, ``errors``, ``mdu``)
shares. See :class:`LibLoader` for where it is loaded from.
"""

import ctypes
import os
import platform
from pathlib import Path


def _platform_library_name() -> str:
    """Return the native library filename for the current platform."""
    names = {
        "Windows": "dflowfm_io_api.dll",
        "Darwin": "libdflowfm_io_api.dylib",
    }
    return names.get(platform.system(), "libdflowfm_io_api.so")


class LibLoader:
    """Locate and load the native dflowfm_io_api library.

    The library lives next to this package in ``_lib/``, where the ``dflowfm_io_api`` build stages it
    after every compile — the single, fixed location for both the installed wheel and the developer
    tree.
    """

    def __init__(self):
        self.dll_name = _platform_library_name()
        self._loaded: ctypes.CDLL | None = None

    def find(self) -> Path:
        """Return the path to the bundled native library, or raise if it has not been staged."""
        # Bundled at the package root (dflowfm_io/_lib), one level up from this base subpackage.
        path = Path(__file__).resolve().parents[1] / "_lib" / self.dll_name
        if not path.is_file():
            raise RuntimeError(
                f"Could not find {path}. Build the dflowfm_io_api target so the library is staged into _lib."
            )
        return path

    def load(self) -> ctypes.CDLL:
        """Load the native library (once) and return the shared :class:`ctypes.CDLL` handle."""
        if self._loaded is None:
            path = self.find()
            # Make sibling runtime DLLs next to the library discoverable (Windows).
            if platform.system() == "Windows" and hasattr(os, "add_dll_directory"):
                os.add_dll_directory(str(path.parent))
            self._loaded = ctypes.CDLL(str(path))
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
