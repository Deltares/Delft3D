from conan import ConanFile
from conan.tools.cmake import CMakeToolchain, CMake, cmake_layout, CMakeDeps
from conan.tools.scm import Git

class netcdf_fortranRecipe(ConanFile):
    name = "netcdf-fortran"
    version = "4.5.4"
    package_type = "library"

    # Optional metadata
    license = "<Put the package license here>"
    author = "<Put your name here> <And your email here>"
    url = "<Package recipe repository url here, for issues about the package>"
    description = "<Description of netcdf-fortran package here>"
    topics = ("<Put some tag here>", "<here>", "<and here>")

    # Binary configuration
    settings = "os", "compiler", "build_type", "arch"
    options = {"shared": [True, False], "fPIC": [True, False]}
    default_options = {"shared": False, "fPIC": True}

    def config_options(self):
        if self.settings.os == "Windows":
            self.options.rm_safe("fPIC")

    def configure(self):
        if self.options.shared:
            self.options.rm_safe("fPIC")

    def layout(self):
        cmake_layout(self)

    def requirements(self):
        self.requires("netcdf/4.8.1")

    def source(self):
        git = Git(self)
        git.clone(url="https://github.com/Unidata/netcdf-fortran.git", target=".", args=["--branch", "v4.5.4", "--depth", "1"])

    def generate(self):
        deps = CMakeDeps(self)
        deps.set_property("netcdf", "cmake_target_aliases", "[libnetcdf]")
        deps.generate()
        tc = CMakeToolchain(self)
        # Work around bug in conan relating to CheckLibraryExists, see https://github.com/conan-io/conan/issues/12180
        tc.cache_variables["CMAKE_TRY_COMPILE_CONFIGURATION"] = str(self.settings.build_type)
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
        self.cpp_info.libs = ["netcdff"]
