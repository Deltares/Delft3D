"""Build shim: force a platform-specific, Python-version-agnostic wheel tag.

The package is pure-Python (ctypes) but ships a compiled native library, so the wheel must NOT be
tagged `py3-none-any`. It must carry the platform tag (e.g. `win_amd64`) while staying agnostic to
the Python version (`py3-none-<platform>`) — one wheel per platform covers all supported Python
versions.
"""

from pathlib import Path

from setuptools import setup

LIB_DIR = Path(__file__).parent / "src" / "dflowfm_io" / "_lib"
LIB_PATTERNS = ("*.dll", "*.so", "*.dylib")


def find_lib():
    """Return the native library files currently staged in ``LIB_DIR``.

    Looks for the platform library names (``*.dll`` / ``*.so`` / ``*.dylib``). An empty list means
    the library has not been built and staged yet.
    """
    return [lib for pattern in LIB_PATTERNS for lib in LIB_DIR.glob(pattern)]


# The bdist_wheel command moved into setuptools in 70.1; fall back to the standalone wheel package
# on older versions. If neither is importable the ImportError propagates and the build fails — which
# is correct: without this tag override the wheel would be mis-tagged py3-none-any.
try:
    from setuptools.command.bdist_wheel import bdist_wheel
except ImportError:
    from wheel.bdist_wheel import bdist_wheel


class BundledWheel(bdist_wheel):
    """A wheel that bundles a compiled native library.

    Overrides the stock ``bdist_wheel`` command to (1) tag the wheel per platform instead of
    ``py3-none-any``, and (2) refuse to build if the native library has not been staged.
    """

    # --- setuptools bdist_wheel hooks (names fixed by the framework) ---

    def finalize_options(self):
        """setuptools hook: finalize command options, then mark the wheel platform-specific."""
        super().finalize_options()
        self.mark_as_platform_specific()

    def run(self):
        """setuptools hook: verify the native library is staged, then build the wheel."""
        self.require_bundled_library()
        super().run()

    def get_tag(self):
        """setuptools hook: return the wheel's (python, abi, platform) tag tuple."""
        return self.platform_wheel_tag()

    # --- named helpers doing the actual work ---

    def mark_as_platform_specific(self):
        """Flag the wheel as non-pure so it gets a platform tag (it carries a binary)."""
        self.root_is_pure = False

    def require_bundled_library(self):
        """Abort the build if no native library has been staged (avoids a broken wheel)."""
        if not find_lib():
            raise SystemExit(
                f"dflowfm_io: no native library found in {LIB_DIR}.\n"
                f"The wheel would be broken (importing dflowfm_io would fail at runtime).\n"
                f"Build the native library first, e.g.:\n"
                f"    cmake --build <build-dir> --target dflowfm_io_api"
            )

    def platform_wheel_tag(self):
        """Keep the platform part of the tag but make it Python-version-agnostic (py3-none-<plat>)."""
        _python, _abi, plat = super().get_tag()
        return "py3", "none", plat


setup(cmdclass={"bdist_wheel": BundledWheel})
