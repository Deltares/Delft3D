import os
from pathlib import Path

from conan.tools.cmake import CMake, CMakeDeps, CMakeToolchain, cmake_layout
from conan.tools.files import get

from conan import ConanFile


class swanRecipe(ConanFile):
    name = "swan"
    package_type = "library"
    implements = ["auto_shared_fpic"]

    # Optional metadata
    license = ("swan", "GNU General Public License v3.0")
    author = "Deltares"
    url = "https://github.com/Deltares/SWAN"
    description = "SWAN (Simulating WAves Nearshore) is a numerical wave model."
    topics = ("swan", "waves", "numerical", "model")

    # Binary configuration
    settings = "os", "compiler", "build_type", "arch", "fortran_compiler"
    options = {"shared": [True, False], "fPIC": [True, False]}
    default_options = {"shared": False, "fPIC": True}

    def layout(self):
        cmake_layout(self)

    def requirements(self):
        self.requires("netcdf/4.9.2")
        self.requires("netcdf-fortran/4.6.2")
        self.requires("hdf5/1.14.2")

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    @property
    def _swan_source_folder(self):
        # adjust this path to the folder inside the extracted archive that contains CMakeLists.txt
        return Path(self.source_folder) / "src" / "cmake"

    def generate(self):
        deps = CMakeDeps(self)
        deps.generate()
        tc = CMakeToolchain(self)
        # Work around bug in conan relating to CheckLibraryExists, see https://github.com/conan-io/conan/issues/12180
        tc.cache_variables["CMAKE_TRY_COMPILE_CONFIGURATION"] = str(
            self.settings.build_type
        )
        tc.cache_variables["CMAKE_INSTALL_LIBDIR"] = "lib"
        tc.cache_variables["CMAKE_INSTALL_BINDIR"] = "bin"
        # Do not build tests or examples
        tc.variables["ENABLE_TESTS"] = False
        tc.variables["BUILD_EXAMPLES"] = False
        tc.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure(
            build_script_folder=os.path.join(self.source_folder, "src", "cmake")
        )
        cmake.build()

    def package(self):
        cmake = CMake(self)
        cmake.install()

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "SWAN")
        self.cpp_info.set_property("cmake_target_name", "SWAN::SWAN")
        self.cpp_info.includedirs = ["include"]
        self.cpp_info.libs = ["swan"]
        self.cpp_info.requires = ["netcdf::netcdf", "netCDF::netcdff", "hdf5::hdf5"]
