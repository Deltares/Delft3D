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
the image size to about 8 GiB (uncompressed). The 2024 version of the intel packages are somewhat
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
sudo docker build . -f ci/dockerfiles/linux/buildtools.Dockerfile -t localhost/buildtools:$TAG --build-arg INTEL_ONEAPI_VERSION=2024
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
sudo docker tag localhost/buildtools:$TAG containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-buildtools:$TAG
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
- ESMF

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
The dockerfile has several build arguments:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
- `DEBUG` (default value: `0`)
- `BUILDTOOLS_IMAGE_URL` (default value: `containers.deltares.nl/delft3d-dev/delft3d-buildtools`)
- `BUILDTOOLS_IMAGE_TAG` (default value: `oneapi-${INTEL_ONEAPI_VERSION}`)

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

The `BUILDTOOLS_IMAGE_URL` points to the repository where the `buildtools` images are located. This URL can be set
to `localhost/buildtools` when you would like to use a `buildtools` image that was built locally.

The `BUILDTOOLS_IMAGE_TAG` ensures that the `third-party-libs` image is based on the `buildtools` image with that tag.

### Build
From the delft3d repository root:
```bash
sudo docker build . -f ci/dockerfiles/linux/third-party-libs.Dockerfile -t localhost/third-party-libs:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifort \
    --build-arg DEBUG=0
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
sudo docker tag localhost/third-party-libs:$TAG containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:$TAG
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
sudo docker tag localhost/dimrset:$TAG containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
```

### Links
- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/delft3d-dimrset/artifacts-tab)

# Building software locally in a docker container
These containers were created to reduce the dependency on the environment of the TeamCity servers,
but they can also be used to build and run our software locally. Assuming a Windows system, we require
WSL2 to be installed to run linux. This can be any distribution that supports docker (the default WSL2 Ubuntu
was tested).

Install docker on your Linux distribution. On Ubuntu, this is done by following
[these steps to install docker using apt](https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository).
Then, log in to the repository locally. Go to containers.deltares.nl, log in, go to your user profile in the top right,
and copy the CLI secret. Then, on Ubuntu run
```bash
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
```
where USERNAME is your e-mail address and TOKEN is the CLI secret that was copied from harbor.

Next, check out the Delft3D repository on Ubuntu. If you would like to build the Delft3D repo inside docker, but
you have made no changes to the docker files, you can simply pull the pre-built containers to your machine.
To receive the third-party-libs container, which is necessary for building Delft3D, you run
```bash
sudo docker pull containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:oneapi-2024-ifx-release
```

If you have made changes to the dockerfiles, you may need to build the `buildtools` and `third-party-libs` images locally.
Go to the Delft3D root and run
```bash
sudo docker build . --file ci/dockerfiles/linux/buildtools.Dockerfile --tag localhost/buildtools:<BUILD_TOOLS_TAG> --build-arg INTEL_ONEAPI_VERSION=2024
sudo docker build . --file ci/dockerfiles/linux/third-party-libs.Dockerfile --tag localhost/third-party-libs:<THIRD_PARTY_TAG> \
    --build-arg INTEL_ONEAPI_VERSION=2024 --build-arg INTEL_FORTRAN_COMPILER=ifx --build-arg DEBUG=0 \
    --build-arg BUILDTOOLS_IMAGE_URL=localhost/buildtools --build-arg BUILDTOOLS_IMAGE_TAG=<BUILD_TOOLS_TAG>
```
Here, the `<BUILD_TOOLS_TAG>` and `<THIRD_PARTY_TAG>` can be the same, and can reflect the issue number or branch that you are working on.
Then, run the `third-party-tools` image while mounting the Delft3D source code:
```bash
sudo docker run --interactive --tty --volume  <DELFT_3D_REPO_PATH>:/checkouts/delft3d localhost/third-party-libs:<THIRD_PARTY_TAG>
```
This command will give you a bash prompt that has all third-party-dependencies and compilers available. The environment is set
by the .bashrc file that is available for the root user. To build the fm-suite, run the following commands:
```bash
cd /checkouts/delft3d
cmake -S ./src/cmake -B build_fm-suite_debug -D CONFIGURATION_TYPE:STRING=fm-suite -D CMAKE_INSTALL_PREFIX=./install_fm-suite_debug/ -D CMAKE_BUILD_TYPE=Debug
cmake --build build_fm-suite_debug
cmake --install build_fm-suite_debug
```
This should allow you to build the binaries. Since this folder was mounted from Ubuntu in WSL2, the resulting binaries will be located there.
Note that these will be written there with root privileges (since sudo was used), and sudo will be required again to remove these directories.
Afterwards, the resulting binaries can be run within a clean almalinux 8 image if the install was successful.

### Run test case
When the build is finished you can try to run a test case using the examples folder:
```
cd /checkouts/delft3d/examples/dflowfm/01_dflowfm_sequential/
./run_example.sh
```
Be sure to have the following  environment variables set:
```
export PATH="/checkouts/delft3d/install_fm-suite_debug/bin:${PATH}" 
export LD_LIBRARY_PATH="/checkouts/delft3d/install_fm-suite_debug/lib:${LD_LIBRARY_PATH}"
```
