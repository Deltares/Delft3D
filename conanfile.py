from conan import ConanFile
from conan.tools.cmake import cmake_layout, CMakeToolchain


class Delft3DRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"

    def requirements(self):
        self.requires("shapelib/1.6.1", libs=True)
        self.requires("gdal/3.8.3", libs=True)
        self.requires("netcdf-fortran/4.5.4")

    def layout(self):
        cmake_layout(self)

    def configure(self):
        self.options["gdal"].with_proj = True
        self.options["gdal"].with_shapelib = False
        self.options["gdal"].with_geos = False

        self.options["gdal"].with_arrow = False
        self.options["gdal"].with_opencl = False
        self.options["gdal"].with_openjpeg = False
        self.options["gdal"].with_png = False
        self.options["gdal"].with_giflib = False
        self.options["gdal"].with_qhull = False
        self.options["gdal"].with_libiconv = False
        self.options["gdal"].with_openssl = False
        self.options["gdal"].with_lerc = False

        self.options["gdal"].with_curl = False
        self.options["proj"].with_curl = False

    def generate(self):
        tc = CMakeToolchain(self)
        tc.user_presets_path = 'src/cmake/CMakePresets.json'
        tc.generate()
