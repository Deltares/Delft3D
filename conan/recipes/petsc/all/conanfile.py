import os

from conan import ConanFile
from conan.errors import ConanInvalidConfiguration
from conan.tools.files import copy, get, rmdir
from conan.tools.intel import IntelCC
from conan.tools.microsoft import unix_path

class PetscConan(ConanFile):
    name = "petsc"
    package_type = "library"
    license = "BSD-2-Clause"
    homepage = "https://petsc.org"
    description = (
        "PETSc/TAO is a tool for writing, analyzing, and optimizing "
        "large-scale numerical simulations."
    )
    topics = ("petsc", "tao", "scientific", "linear-algebra", "mpi", "hpc")

    settings = "os", "compiler", "build_type", "arch", "fortran_compiler"
    options = {
        "shared": [True, False],
        "scalar_type": ["real", "complex"],
        "precision": ["single", "double", "__fp16", "__float128"],
        "with_64_bit_indices": [True, False],
    }
    default_options = {
        "shared": True,
        "scalar_type": "real",
        "precision": "double",
        "with_64_bit_indices": False,
    }

    def _wrappers(self):
        """Return the (cc, cxx, fc) PETSc configure compiler wrappers.

        The C/C++ wrapper is chosen from the ``compiler`` setting and the Fortran
        wrapper from the custom ``fortran_compiler`` setting.
        Raises ConanInvalidConfiguration for unsupported combos.
        """
        compiler = str(self.settings.compiler)
        fortran = self.settings.get_safe("fortran_compiler")

        if self.settings.os == "Windows":
            c_map = {
                "msvc": ("win32fe_cl", "win32fe_cl"),
                "intel-cc": ("win32fe_icx", "win32fe_icx"),
            }
            f_map = {"ifx": "win32fe_ifx", "ifort": "win32fe_ifort"}
        else:
            c_map = {"intel-cc": ("mpiicx", "mpiicpx")}
            f_map = {"ifx": "mpiifx", "ifort": "mpiifort"}

        if compiler not in c_map:
            raise ConanInvalidConfiguration(
                f"petsc: unsupported compiler '{compiler}' on {self.settings.os}. "
                f"Supported C/C++ compilers: {sorted(c_map)}."
            )
        if fortran not in f_map:
            raise ConanInvalidConfiguration(
                f"petsc: unsupported or undefined 'fortran_compiler' setting "
                f"'{fortran}' on {self.settings.os}. Supported: {sorted(f_map)}. "
                "Define it in the profile, e.g. 'fortran_compiler=ifx'."
            )
        cc, cxx = c_map[compiler]
        return cc, cxx, f_map[fortran]

    def validate(self):
        if self.settings.os not in ("Linux", "Windows"):
            raise ConanInvalidConfiguration(
                "The petsc recipe currently supports Linux and Windows only."
            )
        # Validate the compiler / fortran_compiler combination
        _ = self._wrappers()

    def _validate_windows_bash(self):
        # On Windows PETSc is configured and built from a Cygwin bash shell using
        # the win32fe wrapper around the native compilers. The Cygwin bash must
        # be pointed to from the profile with:
        #   tools.microsoft.bash:subsystem=cygwin
        #   tools.microsoft.bash:path=<...>/cygwin64/bin/bash.exe
        # Cygwin's own link.exe is renamed in the container so it does not shadow
        # the MSVC/Intel linker
        # (see https://petsc.org/main/install/windows/#native-microsoft-intel-windows-compilers).
        subsystem = self.conf.get("tools.microsoft.bash:subsystem")
        bash_path = self.conf.get("tools.microsoft.bash:path")
        if subsystem != "cygwin" or not bash_path:
            raise ConanInvalidConfiguration(
                "petsc on Windows requires a Cygwin bash. Set "
                "tools.microsoft.bash:subsystem=cygwin and "
                "tools.microsoft.bash:path=<...>/cygwin64/bin/bash.exe in the "
                "profile."
            )

    def configure(self):
        if self.settings.os == "Windows":
            self.win_bash = True

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    def generate(self):
        # On Linux we register the oneAPI setvars.sh into the "conanbuild"
        # environment so that self.run() has the Intel MPI wrappers
        # (mpiicx/mpiicpx/mpiifx) and MKL on PATH. On Windows the compiler
        # environment is provided by the "Intel oneAPI command prompt" that
        # `conan create` is launched from and is inherited by the Cygwin shell,
        # so no generator is required.
        if self.settings.os == "Linux":
            IntelCC(self).generate()

    def build(self):
        if self.settings.os == "Windows":
            self._validate_windows_bash()
            self._build_windows()
        else:
            self._build_unix()

    def _build_unix(self):
        debug = self.settings.build_type == "Debug"
        opt_flags = "-g -O0" if debug else "-O3"
        cc, cxx, fc = self._wrappers()

        args = [
            f"--prefix={self.package_folder}",
            f"--with-cc={cc}",
            f"--with-cxx={cxx}",
            f"--with-fc={fc}",
            f"--with-debugging={1 if debug else 0}",
            f'--COPTFLAGS="{opt_flags}"',
            f'--CXXOPTFLAGS="{opt_flags}"',
            f'--FOPTFLAGS="{opt_flags}"',
            f"--with-shared-libraries={1 if self.options.shared else 0}",
            f"--with-scalar-type={self.options.scalar_type}",
            f"--with-precision={self.options.precision}",
            f"--with-64-bit-indices={1 if self.options.with_64_bit_indices else 0}",
        ]

        self.run(f"./configure {' '.join(args)}", cwd=self.source_folder)
        self.run("make", cwd=self.source_folder)

    _petsc_arch = "arch-mswin-c-opt"

    def _build_windows(self):
        debug = self.settings.build_type == "Debug"
        prefix = unix_path(self, self.package_folder)
        cc, cxx, fc = self._wrappers()

        args = " ".join(
            [
                f'--prefix="{prefix}"',
                f"--with-cc={cc}",
                f"--with-cxx={cxx}",
                f"--with-fc={fc}",
                '--with-mpi-include="[$MPI/include,$MPI/include/mpi]"',
                '--with-mpi-lib="[$MPI/lib/impi.lib,$MPI/lib/impicxx.lib]"',
                '--with-mpiexec="$MPI/bin/mpiexec.exe"',
                '--with-blaslapack-dir="$MKL/lib"',
                f"--with-debugging={1 if debug else 0}",
                # PETSc only officially supports a shared build with ifx via
                # --with-shared-libraries=0.
                f"--with-shared-libraries={1 if self.options.shared else 0}",
                f"--with-scalar-type={self.options.scalar_type}",
                f"--with-precision={self.options.precision}",
                f"--with-64-bit-indices={1 if self.options.with_64_bit_indices else 0}",
            ]
        )

        src_dir = unix_path(self, self.source_folder)
        self.run(
            f'cd "{src_dir}" && {self._oneapi_env()} '
            f"&& ./configure {args} && {self._windows_make('all')}",
            cwd=self.source_folder,
        )

    @staticmethod
    def _oneapi_env():
        # Derive the oneAPI MPI/MKL roots from ONEAPI_ROOT.
        # DOS 8.3 short paths avoid the spaces in "Program Files (x86)" that would
        # otherwise break PETSc configure. The environment is inherited from the
        # Intel oneAPI command prompt that `conan create` must be launched from.
        return (
            ': "${ONEAPI_ROOT:?is not set - launch conan from an Intel oneAPI '
            'command prompt}" && '
            'MPI="$(cygpath -u "$(cygpath -ms "$ONEAPI_ROOT\\mpi\\latest")")" && '
            'MKL="$(cygpath -u "$(cygpath -ms "$ONEAPI_ROOT\\mkl\\latest")")"'
        )

    def _windows_make(self, target):
        src_dir = unix_path(self, self.source_folder)
        return f'make PETSC_DIR="{src_dir}" PETSC_ARCH="{self._petsc_arch}" {target}'

    def package(self):
        copy(
            self,
            "LICENSE",
            src=self.source_folder,
            dst=os.path.join(self.package_folder, "licenses"),
        )
        if self.settings.os == "Windows":
            src_dir = unix_path(self, self.source_folder)
            self.run(
                f'cd "{src_dir}" && {self._windows_make("install")}',
                cwd=self.source_folder,
            )
        else:
            self.run("make install", cwd=self.source_folder)

        # PETSc's `make install` deploys a full research install. Drop the tutorial/test
        # payload while keeping the small, generally-useful support files.
        share = os.path.join(self.package_folder, "share", "petsc")
        rmdir(self, os.path.join(share, "examples"))
        rmdir(self, os.path.join(share, "datafiles"))

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "PETSc")
        self.cpp_info.set_property("cmake_target_name", "PETSc::PETSc")
        self.cpp_info.set_property("pkg_config_name", "PETSc")
        self.cpp_info.libs = ["libpetsc"] if self.settings.os == "Windows" else ["petsc"]
        self.cpp_info.includedirs = ["include"]
        self.cpp_info.libdirs = ["lib"]
        if self.settings.os == "Windows":
            self.cpp_info.bindirs = ["lib"]

        if self.settings.os in ["Linux", "FreeBSD"]:
            self.cpp_info.system_libs = ["m", "dl"]
            if not self.options.shared:
                self.cpp_info.system_libs += ["pthread", "stdc++"]
