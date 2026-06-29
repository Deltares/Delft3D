from conan import ConanFile
from conan.tools.cmake import CMakeToolchain, CMake, cmake_layout, CMakeDeps
from conan.tools.files import get, rmdir, rm, rename
from pathlib import Path


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
        self.requires("hdf5/1.14.2")

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

        # The upstream install puts .mod files into include/<build_type>/
        # for multi-config generators. Move them up to include/.
        include_dir = Path(self.package_folder) / "include"
        mod_subdir = include_dir / str(self.settings.build_type)
        if mod_subdir.is_dir():
            for f in mod_subdir.glob("*.mod"):
                rename(self, str(f), str(include_dir / f.name))
            rmdir(self, str(mod_subdir))

        # Remove CMake build-tree directories that leak into include/
        # when using multi-config generators (Visual Studio).
        rmdir(self, str(include_dir / "CMakeFiles"))
        for d in include_dir.glob("*.dir"):
            rmdir(self, str(d))

        # Remove nf-config (not needed; consumers use CMake targets)
        rm(self, "nf-config", str(Path(self.package_folder) / "bin"))

        # Remove upstream CMake config files and pkgconfig (Conan generates its own)
        lib_dir = Path(self.package_folder) / "lib"
        rmdir(self, str(lib_dir / "cmake"))
        rmdir(self, str(lib_dir / "pkgconfig"))

        # Remove object files leaked by the upstream install(TARGETS ... OBJECTS)
        for d in lib_dir.glob("objects-*"):
            rmdir(self, str(d))

        # Remove libnetcdff.settings (not needed at consume time)
        rm(self, "libnetcdff.settings", str(lib_dir))

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "netCDF-Fortran")
        self.cpp_info.set_property("cmake_target_name", "netCDF::netcdff")
        self.cpp_info.includedirs = ["include"]
        self.cpp_info.libs = ["netcdff"]
        self.cpp_info.requires = ["netcdf::netcdf", "hdf5::hdf5"]
