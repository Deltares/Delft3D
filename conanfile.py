from conan import ConanFile
from conan.tools.files import get, copy, mkdir
from conan.tools.cmake import CMake, CMakeToolchain
import os

class Delft3DDepsConan(ConanFile):
    name = "delft3d-deps"
    version = "1.0.0"
    license = "See individual components"
    url = "https://github.com/your-org/delft3d-deps"
    description = "Dependencies for Delft3D build environment"
    settings = "os", "arch", "compiler", "build_type"
    options = {"shared": [True, False], "fPIC": [True, False], "debug": [True, False]}
    default_options = {"shared": True, "fPIC": True, "debug": False}
    generators = "cmake", "cmake_find_package", "cmake_find_package_multi", "pkg_config"

    _sources = {
        "zlib": "https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz",
        "libaec": "https://swprojects.dkrz.de/redmine/attachments/download/453/libaec-0.3.2.tar.gz",
        "zstd": "https://github.com/facebook/zstd/archive/refs/tags/v1.5.6.tar.gz",
        "uuid": "https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v2.40/util-linux-2.40.2.tar.gz",
        "metis": "https://github.com/KarypisLab/METIS/archive/refs/tags/v5.2.1.tar.gz",
        "gklib": "https://github.com/KarypisLab/GKlib/archive/8bd6bad750b2b0d90800c632cf18e8ee93ad72d7.tar.gz",
        "expat": "https://github.com/libexpat/libexpat/archive/refs/tags/R_2_6_2.tar.gz",
        "xerces-c": "https://github.com/apache/xerces-c/archive/refs/tags/v3.2.5.tar.gz",
        "petsc": "https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-3.19.0.tar.gz",
        "sqlite3": "https://www.sqlite.org/2024/sqlite-autoconf-3460100.tar.gz",
        "tiff": "https://download.osgeo.org/libtiff/tiff-4.6.0.tar.gz",
        "hdf5": "https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz",
        "netcdf-c": "https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.9.2.tar.gz",
        "netcdf-fortran": "https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz",
        "proj": "https://download.osgeo.org/proj/proj-9.2.0.tar.gz",
        "gdal": "https://github.com/OSGeo/gdal/releases/download/v3.9.2/gdal-3.9.2.tar.gz",
        "esmf": "https://github.com/esmf-org/esmf/archive/refs/tags/v8.8.0.tar.gz",
    }

    def requirements(self):
      self.requires("cmake/3.31.9")
      # Use existing Conan packages where possible
      self.requires("zlib/1.3.1")
      self.requires("zstd/1.5.6")
      self.requires("sqlite3/3.46.1")
      self.requires("libtiff/4.6.0")
      #sub of tiff
      self.requires("xz_utils/5.4.5")
      self.requires("libjpeg/9e")
     
      self.requires("b2/5.3.3")
      self.requires("boost/1.85.0")
      self.requires("gtest/1.13.0")
      
      # Everything else is built from source
      self.requires("libaec/1.1.4")
      self.requires("util-linux-libuuid/2.39.2")#HOPE ITS THE SAME 
      self.requires("metis/5.2.1")
      self.requires("expat/2.6.4")
      self.requires("xerces-c/3.2.5")
     
     
 #  NOT AVIALBABLE   self.requires("petsc/3.19.0")
     
      self.requires("hdf5/1.14.3")
      self.requires("ninja/1.13.1")
      self.requires("meson/1.9.1")
      self.requires("pkgconf/2.2.0")
      self.requires("netcdf/4.8.1")
      # NETCDF subs

      self.requires("libcurl/8.16.0")
      
      self.requires("openssl/3.6.0")
      
      
      ##  NOT AVIAL self.requires("netcdf-fortran/4.6.1")
      self.requires("proj/9.3.1")
      self.requires("gdal/3.10.3")
      
      ## arrow sub 
      self.requires("arrow/19.0.1")
      self.requires('mimalloc/2.2.4')
      self.requires('libdeflate/1.22')
      self.requires('libpng/1.6.50')
     ## not AVAIL self.requires("esmf/8.8.0")

    def build(self):
      debug_flags = "-g -O0" if self.options.debug else "-O3 -DNDEBUG"
      for name, url in self._sources.items():
        src_dir = os.path.join(self.build_folder, name)
        get(self, url, strip_root=True, destination=src_dir)

        with self.chdir(src_dir):
          if name in ["zlib", "libaec", "zstd", "sqlite3", "uuid"]:
            # Autotools style
            self.run(f"./configure --prefix={self.package_folder} CFLAGS='{debug_flags}' CXXFLAGS='{debug_flags}'")
            self.run(f"make -j{os.cpu_count()}")
            self.run("make install")
          elif name in ["tiff", "xerces-c", "gdal", "proj", "hdf5", "netcdf-c", "netcdf-fortran", "petsc", "esmf"]:
            # CMake style
            mkdir(self, "build")
            cmake = CMake(self)
            cmake.configure(source_folder=src_dir, build_folder="build",
                            defs={
                              "CMAKE_INSTALL_PREFIX": self.package_folder,
                              "CMAKE_BUILD_TYPE": "Debug" if self.options.debug else "Release",
                              "CMAKE_C_FLAGS": debug_flags,
                              "CMAKE_CXX_FLAGS": debug_flags
                            })
            cmake.build()
            cmake.install()
          elif name == "metis" or name == "gklib":
            # METIS/GKlib uses custom Makefile
            self.run(f"make -j{os.cpu_count()}")
            self.run(f"make PREFIX={self.package_folder} install")

    def package(self):
      # Copy built headers and libs
      copy(self, "*", src=os.path.join(self.build_folder), dst=self.package_folder)

    def package_info(self):
      self.cpp_info.includedirs = [os.path.join(self.package_folder, "include")]
      self.cpp_info.libdirs = [os.path.join(self.package_folder, "lib")]
      # Set common libraries, can extend with all installed libraries
      self.cpp_info.libs = ["z", "zstd", "sqlite3", "tiff", "boost_system", "boost_filesystem", "gtest"]
      self.env_info.LD_LIBRARY_PATH.append(os.path.join(self.package_folder, "lib"))
      
