from conan import ConanFile
from conan.tools.cmake import cmake_layout


class Delft3DRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"

    def requirements(self):
        self.requires("zlib/[>=1.2.11 <2]")
        self.requires("hdf5/1.14.6")
        self.requires("netcdf/4.9.2")
        self.requires("netcdf-fortran/4.6.2")
        self.requires("proj/9.3.1")
        self.requires("gdal/3.12.1")

    def layout(self):
        cmake_layout(self)

    def configure(self):
        self.options["zlib"].shared = True
        self.options["hdf5"].shared = True
        self.options["netcdf"].shared = True
        self.options["netcdf-fortran"].shared = True
        self.options["proj"].shared = True
        self.options["gdal"].shared = True
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
