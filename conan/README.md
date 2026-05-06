# Conan recipes for Delft3D

This folder contains custom Conan 2 recipes for third-party libraries that are
not available (or not in the required version) from conan-center-index.

## Clean cache (optional)

If you want to start with a clean conan cache, use the following commands:

```bash
conan remove "*" -c 
conan cache clean
```


## Building packages

Build the packages in order (each depends on the previous):

```bash
conan create recipes/zlib/all --version=1.3.2 --profile=default --options="zlib/*:shared=True"

conan create recipes/hdf5/all --version=1.14.6 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True"

conan create recipes/netcdf/all --version=4.9.2 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True" --options="netcdf/*:shared=True" --options="netcdf/*:dap=False" --options="netcdf/*:byterange=False"

conan create recipes/netcdf-fortran/4.6.2 --version=4.6.2 --profile=default --options="zlib/*:shared=True" --options="hdf5/*:shared=True" --options="netcdf/*:shared=True" --options="netcdf/*:dap=False" --options="netcdf/*:byterange=False" --options="netcdf-fortran/*:shared=True"
```

## Exporting recipes without building

If you only want to register the recipes in the local cache (and build later
with `conan install --build=missing`):

```bash
conan export recipes/zlib/all --version=1.3.2
conan export recipes/hdf5/all --version=1.14.6
conan export recipes/netcdf/all --version=4.9.2
conan export recipes/netcdf-fortran/4.6.2 --version=4.6.2
```
