# Conan recipes for Delft3D

This folder contains custom Conan 2 recipes for third-party libraries that are
not available (or not in the required version) from conan-center-index.

## Clean cache (optional)

If you want to start with a clean conan cache, use the following commands:

```bash
conan remove "*" --confirm
conan cache clean
```

## Profile on Windows

```
[settings]
arch=x86_64
build_type=Debug
compiler=msvc
compiler.cppstd=20
compiler.runtime=dynamic
compiler.version=194
os=Windows

[conf]
tools.cmake.cmaketoolchain:extra_variables={'CMAKE_GENERATOR_TOOLSET': {'value': 'fortran=ifx', 'cache': True, 'type': 'STRING', 'force': True}}

```

## Building packages

Build the packages in order (each depends on the previous):

```bash
conan create recipes/cmake/binary --version=3.31.12 --profile=default

conan create recipes/zlib/all --version=1.3.2 --profile=default --options="zlib/*:shared=True"

conan create recipes/hdf5/all --version=1.14.6 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True"

conan create recipes/netcdf/all --version=4.9.2 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True" --options="netcdf/*:shared=True" --options="netcdf/*:dap=False" --options="netcdf/*:byterange=False"

conan create recipes/netcdf-fortran/4.6.2 --version=4.6.2 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True" --options="netcdf/*:shared=True" --options="netcdf/*:dap=False" --options="netcdf/*:byterange=False" --options="netcdf-fortran/*:shared=True"

conan create recipes/json-c/all --version=0.17 --profile=default

conan create recipes/libtiff/all --version=4.7.1 --profile=default --options="libtiff/*:lzma=False" --options="libtiff/*:jpeg=False" --options="zlib/*:shared=True"

conan create recipes/sqlite3/all --version=3.53.0 --profile=default

conan create recipes/nlohmann_json/all --version=3.11.3 --profile=default

conan create recipes/proj/all --version=9.3.1 --profile=default --options="proj/*:shared=True" --options="proj/*:with_curl=False" --options="proj/*:build_executables=False" --options="libtiff/*:lzma=False" --options="libtiff/*:jpeg=False" --options="zlib/*:shared=True"

conan create recipes/libgeotiff/all --version=1.7.1 --profile=default --options="proj/*:shared=True" --options="proj/*:with_curl=False" --options="proj/*:build_executables=False" --options="libtiff/*:lzma=False" --options="libtiff/*:jpeg=False" --options="zlib/*:shared=True"

conan create recipes/gdal/post_3.5.0 --version=3.12.1 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True" --options="netcdf/*:shared=True" --options="netcdf/*:dap=False" --options="netcdf/*:byterange=False" --options="netcdf-fortran/*:shared=True" --options="gdal/*:shared=True" --options="gdal/*:with_arrow=False" --options="gdal/*:with_curl=False" --options="gdal/*:with_expat=False" --options="gdal/*:with_geos=False" --options="gdal/*:with_gif=False" --options="gdal/*:with_jpeg=False" --options="gdal/*:with_lerc=False" --options="gdal/*:with_libcsf=False" --options="gdal/*:with_libdeflate=False" --options="gdal/*:with_libiconv=False" --options="gdal/*:with_opencl=False" --options="gdal/*:with_png=False" --options="gdal/*:with_qhull=False" --options="gdal/*:with_shapelib=False" --options="gdal/*:with_sqlite3=False" --options="gdal/*:gdal_optional_drivers=False" --options="gdal/*:ogr_optional_drivers=False" --options="libtiff/*:lzma=False" --options="libtiff/*:jpeg=False" --options="proj/*:shared=True" --options="proj/*:with_curl=False" --options="proj/*:build_executables=False"
```

## Exporting recipes without building

If you only want to register the recipes in the local cache (and build later
with `conan install --build=missing`):

```bash
conan export recipes/cmake/binary --version=3.31.12
conan export recipes/zlib/all --version=1.3.2
conan export recipes/hdf5/all --version=1.14.6
conan export recipes/netcdf/all --version=4.9.2
conan export recipes/netcdf-fortran/4.6.2 --version=4.6.2
conan export recipes/json-c/all --version=0.17
conan export recipes/libtiff/all --version=4.7.1
conan export recipes/sqlite3/all --version=3.53.0
conan export recipes/nlohmann_json/all --version=3.11.3
conan export recipes/proj/all --version=9.3.1
conan export recipes/libgeotiff/all --version=1.7.1
conan export recipes/gdal/post_3.5.0 --version=3.12.1
```
