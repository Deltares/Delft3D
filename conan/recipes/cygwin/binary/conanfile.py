import os
import textwrap

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

    def _dereference_symlinks(self):
        # Cygwin's default "symlinks" are Windows files that start with the
        # magic header "!<symlink>" and carry the FILE_ATTRIBUTE_SYSTEM flag,
        # which are followed by cygwin1.dll. Conan copies/(un)packs the files
        # without preserving Windows-specific attributes, so replace every
        # symlink (file or directory) with a real copy of its final target.
        bash_exe = os.path.join(self._cygwin_root, "bin", "bash.exe")
        if not os.path.exists(bash_exe):
            return

        script = textwrap.dedent(
            """\
            #!/bin/bash
            set -o errexit

            find / -xdev -type l -print0 |
            while IFS= read -r -d '' link; do
                # Leave any symlink that resolves into /proc, such as /dev/stdin, untouched.
                first_hop=$(readlink -- "$link") || continue
                case "$first_hop" in
                    /proc/*)
                        continue
                        ;;
                esac

                target=$(readlink --canonicalize -- "$link") || continue

                # Temporarily grant owner write access to the parent directory and restore afterwards,
                # because owners do not have write access on some owned directories
                parent=$(dirname -- "$link")
                parent_mode=$(stat --format='%a' -- "$parent") || continue
                chmod u+w -- "$parent"

                if [ -d "$target" ]; then
                    rm --force -- "$link"
                    cp --recursive --preserve=mode,ownership,timestamps -- "$target" "$link"
                elif [ -f "$target" ]; then
                    rm --force -- "$link"
                    cp --preserve=mode,ownership,timestamps -- "$target" "$link"
                fi

                chmod "$parent_mode" -- "$parent"
            done
            """
        )
        script_path = os.path.join(self.build_folder, "dereference_symlinks.sh")
        save(self, script_path, script)
        # --login sources /etc/profile so PATH picks up Cygwin's own coreutils
        self.run(f'"{bash_exe}" --login "{script_path}"')

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

        self._dereference_symlinks()

        if self.options.rename_link:
            link_exe = os.path.join(self._cygwin_root, "bin", "link.exe")
            if os.path.exists(link_exe):
                os.replace(link_exe, os.path.join(self._cygwin_root, "bin", "link-cygwin.exe"))

        download(
            self,
            url=self.conan_data["sources"][self.version]["license"]["url"],
            filename=os.path.join(self.package_folder, "licenses", "COPYING.LIB"),
            sha256=self.conan_data["sources"][self.version]["license"]["sha256"],
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
