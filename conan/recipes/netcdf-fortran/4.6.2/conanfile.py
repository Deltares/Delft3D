from conan import ConanFile
from conan.tools.cmake import CMakeToolchain, CMake, cmake_layout, CMakeDeps
from conan.tools.files import get


class netcdf_fortranRecipe(ConanFile):
    name = "netcdf-fortran"
    package_type = "library"
    implements = ["auto_shared_fpic"]

    # Optional metadata
    license = ("NetCDF", "Apache-2.0")
    author = "Unidata"
    url = "https://github.com/Unidata/netcdf-fortran"
    description = "NetCDF Fortran library for scientific data storage."
    topics = ("netcdf", "fortran", "scientific", "data")

    # Binary configuration
    settings = "os", "compiler", "build_type", "arch"
    options = {"shared": [True, False], "fPIC": [True, False]}
    default_options = {"shared": False, "fPIC": True}

    def layout(self):
        cmake_layout(self)

    def requirements(self):
        self.requires("netcdf/4.9.2")
        self.requires("hdf5/1.14.6")

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    def generate(self):
        deps = CMakeDeps(self)
        deps.generate()
        tc = CMakeToolchain(self)
        # Work around bug in conan relating to CheckLibraryExists, see https://github.com/conan-io/conan/issues/12180
        tc.cache_variables["CMAKE_TRY_COMPILE_CONFIGURATION"] = str(
            self.settings.build_type
        )
        # netcdf-fortran's CMakeLists.txt uses CHECK_LIBRARY_EXISTS to verify
        # nc_def_var_szip exists in libnetcdf. This fails because Conan's
        # CMakeDeps sets NETCDF_C_LIBRARY to the target name "netCDF::netcdf"
        # rather than a library path, which CHECK_LIBRARY_EXISTS cannot use.
        # We know netcdf 4.9.2 has this symbol, so skip the check.
        tc.cache_variables["HAVE_DEF_VAR_SZIP"] = True
        # Do not build tests or examples
        tc.variables["ENABLE_TESTS"] = False
        tc.variables["BUILD_EXAMPLES"] = False
        tc.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def package(self):
        cmake = CMake(self)
        cmake.install()

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "netCDF-Fortran")
        self.cpp_info.set_property("cmake_target_name", "netCDF::netcdff")
        self.cpp_info.includedirs = ["include", f"include/{self.settings.build_type}"]
        self.cpp_info.libs = ["netcdff"]
        self.cpp_info.requires = ["netcdf::netcdf", "hdf5::hdf5"]
