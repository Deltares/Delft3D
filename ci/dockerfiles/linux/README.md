# Linux dockerfiles

There are currently three dockerfiles in this directory:
1. `buildtools.Dockerfile`
2. `third-party-libs.Dockerfile`
3. `dimrset.Dockerfile`

To run these build instructions and build the docker images, you should
have `docker` installed and configured to build `linux` docker images.
`podman` should also work.

## Buildtools
The `buildtools.Dockerfile` contains build instructions to build the `buildtools` container image. 
It uses a 'base' almalinux 8 image copied from [dockerhub](https://hub.docker.com/_/almalinux) and 
pushed to our own [Harbor registry](https://containers.deltares.nl/harbor/projects/21/repositories/almalinux/artifacts-tab).
It installs a few essential tools and programs using `dnf` (including `make` and `wget`) along with
the C, C++ and Fortran compilers, MPI tools and libraries and the math kernel library from Intel.
Unfortunately we need quite a lot of tools to be able to build all of the third party libraries required.
Some libraries have a complex configuration set-up. For instance, PETSc requires `python3` to be configured
and built.

In addition, we fetch the source code of recent versions of autotools (autoconf, automake and libtool)
and cmake and build them from source. These can also be installed using `dnf`, but unfortunately even
the most recent versions of these packages installed with `dnf` are outdated. Some libraries that
we want to compile from source code can't be built using the outdated versions of this software.


Note: This container image is quite large. The 2023 version of the intel packages alone will push
the image size to about 9 GiB (uncompressed). The 2024 version of the intel packages are somewhat
smaller. The image containing the oneapi 2024 Intel packages should be about 7 GiB

### Build arguments
The dockerfile has a single build argument: 
- `INTEL_ONEAPI_VERSION` (default value: `2024`)

Valid values for `INTEL_ONEAPI_VERSION` are `2023` and `2024`.
This build argument allows you to choose which versions of the
C, C++ and Fortran compilers you want to install in the `buildtools`
image.

### Build
From the delft3d repository root:
```bash
docker build . -f ci/dockerfiles/linux/buildtools.Dockerfile -t localhost/buildtools:$TAG --build-arg INTEL_ONEAPI_VERSION=2024
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
docker tag localhost/buildtools:$TAG containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
docker push containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
```

### Links
- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/delft3d-buildtools/artifacts-tab)


## Third-party-libs
The `third-party-libs.Dockerfile` contains build instructions to build the `third-party-libs` container image.
The image uses the `buildtools` image as a base image. You can control which version of the intel compilers
are used to compile all of the third-party-libs using build arguments.

The software in the delft3d repository uses lots of third party libraries. The source code of some libraries are
'vendored' in `src/third_party_open` (There are also compiled binaries and libraries for Windows there).
When building on Linux, some third party libraries are needed that are not vendored in this repository,
so they need to be installed in the environment. These include:

- libuuid
- HDF5
- NetCDF (There are separate libraries for C and Fortran)
- METIS
- PROJ
- GDAL
- PETSc

Note: Some of these libraries require other third party libraries themselves. So this list is not complete.

The `third-party-libs` container image is built on the `buildtools` image. So it includes everything from
`buildtools`. In addition it should include all the third party libraries required to start building the
software in this repository.

The build instructions in the `third-party-libs.Dockerfile` fetch the source code, configure, build and 
install the third party libraries one by one. Each library has its own configuration options, but most of
the time the build steps consists of setting a few environment variables so the right compiler is used, and:
```bash
./configure
make
make install
```

### Build arguments
The dockerfile has three build argument:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
- `DEBUG` (default value: `0`)

The `INTEL_ONEAPI_VERSION` build argument is used to select the right `buildtools` image. Valid values are
`2023` and `2024`.

The `INTEL_FORTRAN_COMPILER` selects which Fortran compiler is used to compile the Fortran libraries
(there are just a few libraries for which this is relevant). Valid values are `ifort` and `ifx`. The
`ifort` compiler in combination with an `INTEL_ONEAPI_VERSION` with value `2024` will result in a lot
of warnings during compilation, since the `ifort` compiler has been deprecated and will no longer be
included in the `2025` release of the intel compilers.

The `DEBUG` build argument controls whether or not the libraries are built with debugging symbols in
the resulting binaries. This can be helpful during debugging especially when there are problems
involving third party libraries. In addition, aggressive compiler optimizations are turned off. Any value other than `0` will turn on the
`DEBUG` flag.

### Build
From the delft3d repository root:
```bash
docker build . -f ci/dockerfiles/linux/third-party-libs.Dockerfile -t localhost/third-party-libs:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifort \
    --build-arg DEBUG=0
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
docker tag localhost/third-party-libs:$TAG containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
docker push containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
```

### Links

- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/delft3d-third-party-libs/artifacts-tab)

## Dimrset
The `dimrset.Dockerfile` contains build instructions to build the `dimrset` container image. 
It uses a 'base' minimal almalinux 8 image copied from [dockerhub](https://hub.docker.com/_/almalinux) and 
pushed to our own [Harbor registry](https://containers.deltares.nl/harbor/projects/21/repositories/almalinux/artifacts-tab).

The build instructions in this file use the `third-party-libs` build image to compile the delft3d software. The resulting
binaries and libraries are installed to an 'install' directory, along with all of the third party libraries. The resulting
install directory should contain everything needed to run the delft3d software. Therefore, we can copy the install
directory to the minimal almalinux 8 image, and the resulting binaries should still work.

Note: This dockerfile copies the entire `src` directory to the container image. This directory contains a lot
of files that are not used during compilation. We can filter out a lot of them using a `.dockerignore` file.
This avoids copying unnecessary files to the build image.

### Build arguments
The dockerfile has three build argument:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
- `DEBUG` (default value: `0`)

The build arguments are the same as the ones used in the `third-party-libs` image. The build arguments are used to select 
a suitable version of the `third-party-libs` image (One that contains the right version of the compilers and libraries).

### Build
From the delft3d repository root:
```bash
docker build . -f ci/dockerfiles/linux/dimrset.Dockerfile \
    -t localhost/dimrset:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifort \
    --build-arg DEBUG=0
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
docker tag localhost/dimrset:$TAG containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
docker push containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
```

### Links
- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/delft3d-dimrset/artifacts-tab)
