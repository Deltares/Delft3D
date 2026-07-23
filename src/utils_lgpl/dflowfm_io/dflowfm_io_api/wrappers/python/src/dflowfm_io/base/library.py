"""Locating and loading the native dflowfm_io_api library.

Importing this module loads the shared library once and exposes it as :data:`lib`.
"""

import ctypes
import os
import platform
from pathlib import Path


def _find_project_root():
    """Walk up from this file to find the top-level project root (contains CMakeLists.txt)."""
    d = os.path.dirname(os.path.abspath(__file__))
    result = None
    while True:
        if os.path.isfile(os.path.join(d, "CMakeLists.txt")):
            result = d
        parent = os.path.dirname(d)
        if parent == d:
            break
        d = parent
    return result


class DLLFinder:
    """Locate and load the native dflowfm_io_api library.

    Search order:
      1. bundled next to this package (``_lib/`` — the installed wheel case),
      2. the ``DFLOWFM_IO_LIB_DIR`` environment variable (explicit override),
      3. common CMake build output directories under the project root (developer tree).
    """

    def __init__(self, dll_dir: str | None = None):
        self.dll_name = (
            "dflowfm_io_api.dll"
            if platform.system() == "Windows"
            else "libdflowfm_io_api.so"
        )
        self.dll_dir = dll_dir

    def _bundled_lib(self) -> Path | None:
        # The native library is bundled at the package root (dflowfm_io/_lib), one level up from
        # this base subpackage.
        candidate = Path(__file__).parents[1] / "_lib" / self.dll_name
        return candidate if candidate.is_file() else None

    def _env_lib(self) -> Path | None:
        directory = self.dll_dir or os.environ.get("DFLOWFM_IO_LIB_DIR", "")
        if not directory:
            return None
        candidate = Path(directory) / self.dll_name
        return candidate if candidate.is_file() else None

    def _cmake_lib(self) -> Path | None:
        root = _find_project_root()
        if not root:
            return None
        for sub in ("build", "build/Debug", "build/Release", "build/RelWithDebInfo", "build/MinSizeRel"):
            candidate = Path(root) / sub / self.dll_name
            if candidate.is_file():
                return candidate
        return None

    def find(self) -> Path:
        for finder in (self._bundled_lib, self._env_lib, self._cmake_lib):
            path = finder()
            if path is not None:
                return path
        raise RuntimeError(
            f"Could not find {self.dll_name}. Searched: bundled package _lib/, "
            f"DFLOWFM_IO_LIB_DIR, and CMake build directories."
        )

    def load(self) -> ctypes.CDLL:
        path = self.find()
        # Ensure any sibling runtime DLLs next to the library are discoverable (Windows).
        if platform.system() == "Windows" and hasattr(os, "add_dll_directory"):
            os.add_dll_directory(str(path.parent))
        return ctypes.CDLL(str(path))


dll_finder = DLLFinder()
lib = dll_finder.load()
