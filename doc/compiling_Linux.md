# Compiling Delft3D on Linux

The preferred build procedure for Linux uses containers; this is the procedure described here.
Containers are lightweight, isolated environments that package an application and all its dependencies so it can run consistently across different systems.

## Prerequisites
- Install git using
  ```
  sudo dnf install git
  ```
  The example above uses `dnf` which is the current package manager on Red Hat, Alma Linux, CentOS and Fedora.
  On Debian and Ubuntu one would use `apt`.
- Download or clone the source code from https://github.com/Deltares/Delft3D
  ```
  git clone https://github.com/Deltares/Delft3D.git
  ```
  This step is eventually needed for compiling Delft3D, but it also downloads the Dockerfiles for the next step.
- Install Docker.
  The exact steps depend on your operating system.
  It will typically be something like:
  ```
  # 1. Install repo management tools
  sudo dnf install -y yum-utils
  
  # 2. Add the Docker CE repository (CentOS repo used for AlmaLinux/CentOS)
  sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
  
  # 3. Install Docker Engine + CLI + containerd + Compose plugin
  sudo dnf install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin
  
  # 4. Enable & start service
  sudo systemctl enable --now docker
  ```
- See [these instructions](../ci/dockerfiles/linux/README.md) for setting up the rest of the prerequisites: a container for the base build environment and one with the build environment including all third party dependencies.
  For Windows, we include all third party dependencies in the source distribution in (semi) compiled form, but on Linux it's common practice that you build such libraries yourself.

## Build steps
- Start the Docker image `localhost/third-party-libs` built in the prerequisites step.
  ```
  docker run -it localhost/third-party-libs bash
  ```
- Go to the desired work folder
  ```
  cd home
  ```
- Download or clone the source code from https://github.com/Deltares/Delft3D
  ```
  git clone https://github.com/Deltares/Delft3D.git
  ```

## Dimrset
The build instructions in this file use the `third-party-libs` build image to compile the delft3d software.
The resulting binaries and libraries are installed to an 'install' directory, along with all of the third party libraries.
The resulting install directory should contain everything needed to run the delft3d software.
Therefore, we can copy the install directory to the minimal almalinux 8 image, and the resulting binaries should still work.

Note: This dockerfile copies the entire `src` directory to the container image.
This directory contains a lot of files that are not used during compilation.
We use  a `.dockerignore` file in the root folder to filter out a lot of them.
This avoids copying unnecessary files to the Linux build image.

### Build arguments
The dockerfile has three build argument:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifx`)
- `DEBUG` (default value: `0`)

The build arguments are the same as the ones used in the `third-party-libs` image.
The build arguments are used to select  a suitable version of the `third-party-libs` image (One that contains the right version of the compilers and libraries).

### Build
From the delft3d repository root:
```bash
docker build . -f ci/dockerfiles/linux/dimrset.Dockerfile \
    -t localhost/dimrset:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifx \
    --build-arg DEBUG=0
```
Note: Passing the build arguments is not necessary if the default value is required.

### Push
```bash
sudo docker tag localhost/dimrset:$TAG containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
sudo docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
sudo docker push containers.deltares.nl/delft3d-dev/delft3d-dimrset:$TAG
```

# Building software locally in a docker container
These containers were created to reduce the dependency on the environment of the TeamCity servers, but they can also be used to build and run our software locally.
Assuming a Windows system, we require WSL2 to be installed to run linux.
This can be any distribution that supports docker (the default WSL2 Ubuntu was tested).

Install docker on your Linux distribution.
On Ubuntu, this is done by following [these steps to install docker using apt](https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository).
Then, log in to the repository locally.
Go to containers.deltares.nl, log in, go to your user profile in the top right, and copy the CLI secret.
Then, on Ubuntu run
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


This can again be done in a container.
```
ARG INTEL_ONEAPI_VERSION=2024
ARG INTEL_FORTRAN_COMPILER=ifx
ARG BUILD_TYPE=release
ARG BASE_TAG=oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE}

FROM containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:${BASE_TAG} AS build

WORKDIR /source

COPY ./src ./src
COPY ./test/integration_test ./test/integration_test
COPY ./test/unit_test ./test/unit_test

RUN --mount=type=cache,target=/source/build <<"EOF"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export FC=mpi${INTEL_FORTRAN_COMPILER} CXX=mpicxx CC=mpiicx

mkdir --parents /delft3d
cmake ./src/cmake -G "Unix Makefiles" -B build \
    -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
    -DCONFIGURATION_TYPE=all \
    -DCMAKE_INSTALL_PREFIX=/delft3d

cmake --build build --parallel $(nproc) --target install --config ${BUILD_TYPE}
EOF

FROM containers.deltares.nl/delft3d-dev/almalinux:8.10-minimal

COPY --from=build /delft3d/ /delft3d/