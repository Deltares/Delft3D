"""Locating and loading the native dflowfm_io_api library.

Importing this module loads the shared library once and exposes it as :data:`lib` — a single
process-wide :class:`Lib` handle that every other module (``bindings``, ``errors``, ``mdu``)
shares. See :class:`LibLoader` for the search order.
"""

import ctypes
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

    Search order (first match wins):
      1. an explicit override — the ``dll_dir`` argument, else the ``DFLOWFM_IO_LIB_DIR``
         environment variable — so a developer can always point at a specific build;
      2. the copy bundled next to this package (``_lib/`` — the installed-wheel case);
      3. common CMake build-output directories under the project root (the developer tree).

    The explicit override deliberately outranks the bundled copy: if you set it, you mean it.
    """

    def __init__(self, dll_dir: str | None = None):
        self.dll_name = _platform_library_name()
        self.dll_dir = dll_dir
        self._loaded: ctypes.CDLL | None = None

    @staticmethod
    def _project_root() -> Path | None:
        """Walk up from this file to the top-most directory that contains a ``CMakeLists.txt``."""
        directory = Path(__file__).resolve().parent
        root: Path | None = None
        for candidate in (directory, *directory.parents):
            if (candidate / "CMakeLists.txt").is_file():
                root = candidate
        return root

    def _override_dir(self) -> str | None:
        """The explicit library directory, if any: constructor argument or environment variable."""
        import os

        return self.dll_dir or os.environ.get("DFLOWFM_IO_LIB_DIR") or None

    def _candidate_dirs(self) -> list[Path]:
        """The directories to search, in precedence order."""
        dirs: list[Path] = []

        override = self._override_dir()
        if override:
            dirs.append(Path(override))

        # Bundled at the package root (dflowfm_io/_lib), one level up from this base subpackage.
        dirs.append(Path(__file__).resolve().parents[1] / "_lib")

        root = self._project_root()
        if root:
            for sub in ("build", "build/Debug", "build/Release", "build/RelWithDebInfo", "build/MinSizeRel"):
                dirs.append(root / sub)

        return dirs

    def find(self) -> Path:
        """Return the path to the native library, or raise if it cannot be found."""
        searched = [directory / self.dll_name for directory in self._candidate_dirs()]
        found = next((candidate for candidate in searched if candidate.is_file()), None)
        if found is None:
            locations = "\n  ".join(str(path) for path in searched)
            raise RuntimeError(f"Could not find {self.dll_name}. Searched:\n  {locations}")
        return found

    def load(self) -> ctypes.CDLL:
        """Load the native library (once) and return the shared :class:`ctypes.CDLL` handle."""
        if self._loaded is None:
            path = self.find()
            # Make sibling runtime DLLs next to the library discoverable (Windows).
            if platform.system() == "Windows":
                import os

                if hasattr(os, "add_dll_directory"):
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
