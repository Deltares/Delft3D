import os

from conan import ConanFile
from conan.errors import ConanInvalidConfiguration
from conan.tools.files import copy, get, rmdir
from conan.tools.intel import IntelCC

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

    settings = "os", "compiler", "build_type", "arch"
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

    def validate(self):
        # Windows support will be added later through Cygwin.
        if self.settings.os != "Linux":
            raise ConanInvalidConfiguration(
                "The petsc recipe currently supports Linux only."
            )
        if self.settings.compiler != "intel-cc":
            raise ConanInvalidConfiguration(
                "The petsc recipe requires the Intel oneAPI compiler (intel-cc), "
                "which provides the MPI wrappers (mpiicx/mpiicpx/mpiifx) and MKL "
                "used by the PETSc build."
            )

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    def generate(self):
        # Registers the oneAPI setvars.sh into the "conanbuild" environment so
        # that self.run() has the Intel MPI wrappers (mpiicx/mpiicpx/mpiifx) and
        # MKL on PATH.
        IntelCC(self).generate()

    def build(self):
        debug = self.settings.build_type == "Debug"
        opt_flags = "-g -O0" if debug else "-O3"

        args = [
            f"--prefix={self.package_folder}",
            "--with-cc=mpiicx",
            "--with-cxx=mpiicpx",
            "--with-fc=mpiifx",
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

    def package(self):
        copy(
            self,
            "LICENSE",
            src=self.source_folder,
            dst=os.path.join(self.package_folder, "licenses"),
        )
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
        self.cpp_info.libs = ["petsc"]
        self.cpp_info.includedirs = ["include"]
        self.cpp_info.libdirs = ["lib"]

        if self.settings.os in ["Linux", "FreeBSD"]:
            self.cpp_info.system_libs = ["m", "dl"]
            if not self.options.shared:
                self.cpp_info.system_libs += ["pthread", "stdc++"]
