# Preparing a Linux development environment using Docker
Back to [Linux development page](../../../doc/compiling_Linux.md).

The build environment is built in two steps:
1. `buildtools.Dockerfile` to build a container containing the Intel compilers, CMake and other basic build tools.
2. `third-party-libs.Dockerfile` to extend the container with all third party libraries required by one or more Delft3D components.

To run these build instructions and build the docker images, you should have `docker` installed and configured to build Linux docker images.
Using another container platform `podman` should also work.

**Notes:**
- The resulting container images are quite large.
  The (uncompressed) size of the `buildtools` image is about 10 GB, and the size of the `third-party-libs` image is about 13 GB.
- Delft3D FM depends on PETSc; the version of the PETSc library that we are currently using is not compatible with OneAPI 2025.
  We are adjusting the code to use the updated Fortran API of the latest PETSc version.

## Buildtools container
The `buildtools.Dockerfile` contains build instructions to build the `buildtools` container image. 
It uses a 'base' almalinux 8 image copied from [dockerhub](https://hub.docker.com/_/almalinux) and  pushed to our own [Harbor registry](https://containers.deltares.nl/harbor/projects/21/repositories/almalinux/artifacts-tab).
Outside Deltares, you may need to point to the original image.
In that case replace the first line of the Dockerfile starting with `FROM` by
```
FROM almalinux:8 as buildtools
```
The Dockerfile continues by installing a few essential tools and programs using `dnf` (including `make` and `wget`) along with the C, C++ and Fortran compilers, MPI tools and libraries and the math kernel library from Intel.
Unfortunately we need quite a lot of tools to be able to build all of the third party libraries required.
Some libraries have a complex configuration set-up.
For instance, PETSc requires `python3` to be configured and built.

In addition, we fetch the source code of recent versions of autotools (autoconf, automake and libtool) and cmake and build them from source.
These can also be installed using `dnf`, but unfortunately even the most recent versions of these packages installed with `dnf` are outdated.
Some libraries that we want to compile from source code can't be built using the outdated versions of this software.

### Build arguments
The dockerfile has a single build argument: 
- `INTEL_ONEAPI_VERSION` (default value: `2024`)

Valid values for `INTEL_ONEAPI_VERSION` are `2023` and `2024`.
This build argument allows you to choose which versions of the C, C++ and Fortran compilers you want to install in the `buildtools` image.

### Build
From the delft3d repository root:
```bash
export TAG=oneapi-2024
sudo docker build . -f ci/dockerfiles/linux/buildtools.Dockerfile -t localhost/buildtools:$TAG --build-arg INTEL_ONEAPI_VERSION=2024
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
Optionally push the Docker container to a repository:
```bash
sudo docker tag localhost/buildtools:$TAG containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
```
This is not necessary for the next step.


## Third-party-libs
The `third-party-libs.Dockerfile` contains build instructions to build the `third-party-libs` container image.
The image uses the `buildtools` image as a base image.
You can control which version of the intel compilers are used to compile all of the third-party-libs using build arguments.

The software in the Delft3D repository uses lots of third party libraries.
The source code of some libraries are 'vendored' (i.e. included) in `src/third_party_open`.
When building on Linux, some third party libraries are needed that are not vendored in this repository, so they need to be installed in the environment.
These include:

- libuuid
- HDF5
- NetCDF (There are separate libraries for C and Fortran)
- METIS
- PROJ
- GDAL
- PETSc
- ESMF
- precice (Recently added; not yet used in the main branch)

Note: Some of these libraries require other third party libraries themselves.
So this list is not complete.

The `third-party-libs` container image is built on the `buildtools` image.
So it includes everything from `buildtools`.
The standard `third-party-libs.Dockerfile` points to the `buildtools` image in our repository:
```
ARG BUILDTOOLS_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-buildtools
```
You can replace that by
```
ARG BUILDTOOLS_IMAGE_URL=localhost/buildtools
```
for your local builds (note that the `$TAG` is added automatically further down in the Dockerfile).
In addition it should include all the third party libraries required to start building the software in this repository.

The build instructions in the `third-party-libs.Dockerfile` fetch the source code, configure, build and install the third party libraries one by one.
Each library has its own configuration options, but most of the time the build steps consists of setting a few environment variables so the right compiler is used, and:
```bash
./configure
make
make install
```

### Build arguments
The dockerfile has several build arguments:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
- `DEBUG` (default value: `0`)
- `BUILDTOOLS_IMAGE_URL` (default value: `containers.deltares.nl/delft3d-dev/delft3d-buildtools`)
- `BUILDTOOLS_IMAGE_TAG` (default value: `oneapi-${INTEL_ONEAPI_VERSION}`)

The `INTEL_ONEAPI_VERSION` build argument is used to select the right `buildtools` image.
Valid values are `2023` and `2024`.

The `INTEL_FORTRAN_COMPILER` selects which Fortran compiler is used to compile the Fortran libraries (there are just a few libraries for which this is relevant).
Valid values are `ifort` and `ifx`.
The `ifort` compiler in combination with an `INTEL_ONEAPI_VERSION` with value `2024` will result in a lot of warnings during compilation, since the `ifort` compiler has been deprecated and will no longer be included in the `2025` release of the intel compilers.

The `DEBUG` build argument controls whether or not the libraries are built with debugging symbols in the resulting binaries.
This can be helpful during debugging especially when there are problems involving third party libraries.
In addition, aggressive compiler optimizations are turned off.
Any value other than `0` will turn on the `DEBUG` flag.

The `BUILDTOOLS_IMAGE_URL` points to the repository where the `buildtools` images are located.
This URL can be set to `localhost/buildtools` when you would like to use a `buildtools` image that was built locally.

The `BUILDTOOLS_IMAGE_TAG` ensures that the `third-party-libs` image is based on the `buildtools` image with that tag.

### Build
From the delft3d repository root:
```bash
sudo docker build . -f ci/dockerfiles/linux/third-party-libs.Dockerfile -t localhost/third-party-libs:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifx \
    --build-arg DEBUG=0
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
Optionally push the Docker container to a repository:
```bash
sudo docker tag localhost/third-party-libs:$TAG containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
```
