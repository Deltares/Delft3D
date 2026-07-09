import os

from conan import ConanFile
from conan.errors import ConanInvalidConfiguration
from conan.tools.files import download, rmdir, rm, save

required_conan_version = ">=2"

class CygwinConan(ConanFile):
    name = "cygwin"
    package_type = "application"
    description = (
        "Cygwin is a large collection of GNU and Open Source tools "
        "which provide functionality similar to a Linux distribution on Windows."
    )
    topics = ("cygwin", "bash", "posix", "installer", "build")
    homepage = "https://cygwin.com"
    license = "GPL-3.0-or-later"
    settings = "os", "arch"

    options = {
        # Comma separated list of extra Cygwin packages to install on top of the
        # (always installed) "Base" category. Set to None for a Base-only install.
        "packages": [None, "ANY"],
        # Cygwin ships its own link.exe which can shadow the MSVC/Intel linker.
        # Rename it so the native toolchain linker is picked up instead. See
        # https://petsc.org/main/install/windows/#native-microsoft-intel-windows-compilers
        "rename_link": [True, False],
        # Comma separated list of Cygwin mirror URLs to download packages from.
        # Leave as None to use the default community mirrors from conandata.yml.
        "mirrors": [None, "ANY"],
    }
    default_options = {
        "packages": "python3,make",
        "rename_link": False,
        "mirrors": None,
    }

    @property
    def _mirrors(self):
        override = self.options.get_safe("mirrors")
        if override:
            return [m.strip() for m in str(override).split(",") if m.strip()]
        return self.conan_data["sources"][self.version]["mirrors"]

    @property
    def _cygwin_root(self):
        return os.path.join(self.package_folder, "cygwin64")

    def validate(self):
        if self.settings.os != "Windows":
            raise ConanInvalidConfiguration("The cygwin recipe only supports Windows.")
        if self.settings.arch != "x86_64":
            raise ConanInvalidConfiguration("Cygwin is only packaged for the x86_64 architecture.")

    def build(self):
        download(
            self,
            url=self.conan_data["sources"][self.version]["setup"]["url"],
            filename="cygwin-setup-x86_64.exe",
            sha256=self.conan_data["sources"][self.version]["setup"]["sha256"],
        )

    def package(self):
        setup_exe = os.path.join(self.build_folder, "cygwin-setup-x86_64.exe")
        # setup.exe uses very long, URL-encoded directory names for its download
        # cache (e.g. https%3a%2f%2fmirrors.kernel.org%2f...). Keep it in a short
        # scratch folder and delete it afterwards so it never ends up packaged.
        package_cache = os.path.join(self.build_folder, "cygwin-packages")

        args = [
            "--quiet-mode",
            "--no-shortcuts",
            "--no-startmenu",
            "--no-desktop",
            "--no-admin",
            "--upgrade-also",
            "--root", self._cygwin_root,
            "--local-package-dir", package_cache,
        ]
        for mirror in self._mirrors:
            args += ["--site", mirror]
        args += ["--only-site"]
        packages = self.options.get_safe("packages")
        if packages:
            args += ["--packages", str(packages)]

        self.run(f'"{setup_exe}" {" ".join(args)}')

        rmdir(self, package_cache)

        if self.options.rename_link:
            link_exe = os.path.join(self._cygwin_root, "bin", "link.exe")
            if os.path.exists(link_exe):
                os.replace(link_exe, os.path.join(self._cygwin_root, "bin", "link-cygwin.exe"))

        save(
            self,
            os.path.join(self.package_folder, "licenses", "LICENSE"),
            "Cygwin is a collection of Open Source packages, each covered by its "
            "own license (mostly GPL-3.0-or-later). The per-package license and "
            "copyright files are installed under cygwin64/usr/share/doc. See "
            "https://cygwin.com/licensing.html for details.\n",
        )

        # Reduce package size / avoid long-path issues from the offline docs.
        rm(self, "*.pdb", os.path.join(self._cygwin_root, "bin"), recursive=True)

    def package_info(self):
        self.cpp_info.includedirs = []
        self.cpp_info.libdirs = []

        bindir = os.path.join(self._cygwin_root, "bin")
        self.cpp_info.bindirs = [bindir]

        self.buildenv_info.define_path("CYGWIN_ROOT", self._cygwin_root)

        # Let consumers that set win_bash = True (e.g. petsc) automatically pick
        # up this Cygwin bash, without any profile configuration.
        self.conf_info.define("tools.microsoft.bash:subsystem", "cygwin")
        self.conf_info.define("tools.microsoft.bash:path", os.path.join(bindir, "bash.exe"))
