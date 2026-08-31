from conan import ConanFile
from conan.tools.files import save

class Delft3DRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch", "fortran_compiler"
    generators = "CMakeDeps"

    def requirements(self):
        self.requires("zlib/[>=1.2.11 <2]")
        self.requires("hdf5/1.14.2")
        self.requires("netcdf/4.9.2")
        self.requires("netcdf-fortran/4.6.2")
        self.requires("libtiff/4.7.1")
        self.requires("proj/9.3.1")
        self.requires("gdal/3.12.1")
        self.requires("expat/2.8.0")
        self.requires("petsc/3.25.3")
        self.requires("cgal/6.2")
        self.requires("boost/1.90.0")
        self.requires("eigen/5.0.1")
        self.requires("libxml2/2.15.3")
        self.requires("precice/3.4.1")

    def generate(self):
        save(self, "conan.stamp", "Timestamp of this file is used by CMake to detect if conan.lock has changed since last conan install.")

    def layout(self):
        self.folders.generators = "generators"

    def configure(self):
        self.options["zlib"].shared = True
        self.options["hdf5"].shared = True
        self.options["netcdf"].shared = True
        self.options["netcdf-fortran"].shared = True
        self.options["proj"].shared = True
        self.options["gdal"].shared = True
        self.options["libtiff"].shared = True
        # disable DAP and byterange support, this requires dependencies like libcurl are not needed
        self.options["netcdf"].dap = False
        self.options["netcdf"].byterange = False
        # Disable unneeded gdal features. Delft3D only uses gdal through the
        # fortrangis Fortran binding, and the only call site reads GeoTIFF
        # raster files (see read_samples_from_geotiff.F90). The GTiff driver
        # is built unconditionally from gdal's required dependencies
        # (libtiff, libgeotiff, proj, zlib, json-c), so all optional gdal
        # back-ends and drivers can be turned off.
        self.options["gdal"].with_arrow = False
        self.options["gdal"].with_curl = False
        self.options["gdal"].with_expat = False
        self.options["gdal"].with_geos = False
        self.options["gdal"].with_gif = False
        self.options["gdal"].with_jpeg = False
        self.options["gdal"].with_lerc = False
        self.options["gdal"].with_libcsf = False
        self.options["gdal"].with_libdeflate = False
        self.options["gdal"].with_libiconv = False
        self.options["gdal"].with_opencl = False
        self.options["gdal"].with_png = False
        self.options["gdal"].with_qhull = False
        self.options["gdal"].with_shapelib = False
        self.options["gdal"].with_sqlite3 = False
        self.options["gdal"].gdal_optional_drivers = False
        self.options["gdal"].ogr_optional_drivers = False
        # Trim transitive deps that gdal/proj would otherwise pull in.
        # libtiff: keep zlib (DEFLATE-compressed GeoTIFFs are common for
        # elevation/bathymetry rasters); drop xz (lzma) and libjpeg.
        self.options["libtiff"].lzma = False
        self.options["libtiff"].jpeg = False
        # proj: we do not download grid-shift files at runtime and do not
        # ship the proj/cs2cs/... CLIs. Keep with_tiff for accurate datum
        # transforms via TIFF grid files.
        self.options["proj"].with_curl = False
        self.options["proj"].build_executables = False
        self.options["expat"].shared = True
        self.options["hdf5"].enable_cxx = False
        self.options["libxml2"].iconv = False
        # preCICE is compiled with BOOST_ALL_DYN_LINK and the Delft3D CMake
        # code expects Boost DLLs (Boost_USE_STATIC_LIBS OFF), so build Boost
        # shared. Set here as well, because the boost node is expanded from
        # this recipe and would otherwise keep the recipe default (static).
        self.options["boost"].shared = True
        # Disable unused boost options
        self.options["boost"].without_charconv = True
        self.options["boost"].without_cobalt = True
        self.options["boost"].without_context = True
        self.options["boost"].without_contract = True
        self.options["boost"].without_coroutine = True
        self.options["boost"].without_fiber = True
        self.options["boost"].without_graph = True
        self.options["boost"].without_iostreams = True
        self.options["boost"].without_json = True
        self.options["boost"].without_locale = True
        self.options["boost"].without_math = True
        self.options["boost"].without_nowide = True
        self.options["boost"].without_process = True
        self.options["boost"].without_random = True
        # NOTE: serialization cannot be disabled: Boost.Log links against
        # Boost.PropertyTree (log_setup settings parser) and PropertyTree
        # declares boost_serialization as a b2 dependency, so b2 builds and
        # installs libboost_serialization/libboost_wserialization anyway.
        self.options["boost"].without_stacktrace = True
        self.options["boost"].without_timer = True
        self.options["boost"].without_type_erasure = True
        self.options["boost"].without_url = True
        self.options["boost"].without_wave = True
