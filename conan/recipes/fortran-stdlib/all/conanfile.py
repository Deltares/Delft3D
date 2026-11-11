import os

from conan import ConanFile
from conan.tools.cmake import CMake, CMakeToolchain, cmake_layout
from conan.tools.files import copy, get, rmdir

required_conan_version = ">=2"


class FortranStdlibConan(ConanFile):
    name = "fortran-stdlib"
    package_type = "static-library"
    license = "MIT"
    homepage = "https://github.com/fortran-lang/stdlib"
    url = "https://github.com/fortran-lang/stdlib"
    description = "Community-driven standard library for Fortran"
    topics = ("fortran", "stdlib")

    settings = "os", "arch", "compiler", "build_type", "fortran_compiler"

    def layout(self):
        cmake_layout(self, src_folder="src")

    def build_requirements(self):
        self.tool_requires("cmake/[>=3.14 <5]")
        self.tool_requires("fypp/3.2.0")

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    def generate(self):
        tc = CMakeToolchain(self)
        tc.cache_variables["BUILD_TESTING"] = False
        tc.cache_variables["FIND_BLAS"] = False
        tc.cache_variables["CMAKE_INSTALL_LIBDIR"] = "lib"
        tc.cache_variables["CMAKE_INSTALL_BINDIR"] = "bin"
        tc.cache_variables["CMAKE_INSTALL_INCLUDEDIR"] = "include"
        tc.cache_variables["CMAKE_INSTALL_MODULEDIR"] = "include"
        tc.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def package(self):
        cmake = CMake(self)
        cmake.install()
        copy(
            self,
            "LICENSE",
            src=self.source_folder,
            dst=os.path.join(self.package_folder, "licenses"),
        )
        rmdir(self, os.path.join(self.package_folder, "lib", "cmake"))
        rmdir(self, os.path.join(self.package_folder, "lib", "pkgconfig"))

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "fortran_stdlib")
        self.cpp_info.set_property(
            "cmake_target_name", "fortran_stdlib::fortran_stdlib"
        )
        # Match stdlib 0.8.1's target_link_libraries graph, not archive file order.
        dependencies = {
            "ansi": ["core", "strings"],
            "array": [],
            "bitsets": ["core"],
            "blas": ["core", "linalg_core"],
            "constants": ["core"],
            "core": [],
            "hash": ["core"],
            "hashmaps": ["hash"],
            "intrinsics": ["linalg_core", "blas", "constants"],
            "io": ["core", "strings"],
            "lapack": ["core", "linalg_core", "blas", "strings"],
            "lapack_extended": ["core", "linalg_core"],
            "linalg_core": ["core"],
            "linalg_iterative": ["linalg", "sparse"],
            "linalg": [
                "core", "linalg_core", "constants", "lapack",
                "lapack_extended", "sorting", "intrinsics",
            ],
            "logger": ["core"],
            "math": ["bitsets", "core", "strings"],
            "quadrature": ["core", "specialfunctions"],
            "selection": ["core"],
            "sorting": ["bitsets", "core", "strings"],
            "specialfunctions": ["intrinsics"],
            "specialmatrices": [
                "constants", "linalg_core", "linalg", "lapack", "lapack_extended",
            ],
            "stringlist": ["math", "strings"],
            "strings": ["core"],
            "system": ["core", "linalg_core", "strings"],
            "stats": ["core", "linalg_core", "linalg", "selection", "strings"],
            "sparse": ["constants", "sorting"],
        }
        for name, requires in dependencies.items():
            component = self.cpp_info.components[name]
            component.set_property("cmake_target_name", f"fortran_stdlib::fortran_stdlib_{name}")
            component.libs = [f"fortran_stdlib_{name}"]
            component.requires = requires

        self.cpp_info.components["stdlib"].libs = ["fortran_stdlib"]
        self.cpp_info.components["stdlib"].requires = list(dependencies)
