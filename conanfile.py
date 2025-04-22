from conan import ConanFile
from conan.tools.cmake import cmake_layout, CMakeToolchain

class Delft3DRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"

    def requirements(self):
        self.requires("netcdf-fortran/4.5.4")

    def layout(self):
        cmake_layout(self)

    def generate(self):
        tc = CMakeToolchain(self)
        tc.user_presets_path = 'src/CMake/CMakePresets.json'
        tc.generate()
