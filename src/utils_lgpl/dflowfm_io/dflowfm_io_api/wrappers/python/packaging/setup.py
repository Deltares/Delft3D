"""Build shim: force a platform-specific, Python-version-agnostic wheel tag.

The package is pure-Python (ctypes) but ships a compiled native library, so the wheel must NOT be
tagged `py3-none-any`. It must carry the platform tag (e.g. `win_amd64`) while staying agnostic to
the Python version (`py3-none-<platform>`) — one wheel per platform covers all supported Python
versions.
"""

from setuptools import setup


# The bdist_wheel command moved into setuptools in 70.1; fall back to the standalone wheel package
# on older versions. If neither is importable the ImportError propagates and the build fails — which
# is correct: without this tag override the wheel would be mis-tagged py3-none-any.
try:
    from setuptools.command.bdist_wheel import bdist_wheel as _bdist_wheel
except ImportError:
    from wheel.bdist_wheel import bdist_wheel as _bdist_wheel


class bdist_wheel(_bdist_wheel):

    def finalize_options(self):
        super().finalize_options()
        self.root_is_pure = False  # contains a platform binary -> not pure

    def get_tag(self):
        _python, _abi, plat = super().get_tag()
        return "py3", "none", plat


setup(cmdclass={"bdist_wheel": bdist_wheel})
