# Compiling Delft3D on Linux
Back to main [development page](development.md).

The preferred build procedure for Linux uses containers; this is the procedure described here.
Containers are lightweight, isolated environments that package an application and all its dependencies so it can run consistently across different systems.

Third-party dependencies are managed by [Conan 2](https://docs.conan.io/2/).
Pre-built binary packages are available from the Deltares Nexus repository for Deltares developers.
External developers build all dependencies from the local recipes included in this repository.

> **Note:** Not all third-party libraries have been migrated to Conan yet.
> Some still come from the `third-party-libs` Docker container. The build scripts
> handle both sources transparently.

## Prerequisites
- Install git using
  ```
  sudo dnf install -y git
  ```
  The example above uses `dnf` which is the current package manager on Red Hat, Alma Linux, CentOS and Fedora.
  On Debian and Ubuntu one would use `apt`.
- Download or clone the source code from https://github.com/Deltares/Delft3D
  ```
  git clone https://github.com/Deltares/Delft3D.git
  ```
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
- See [these instructions](../ci/dockerfiles/linux/README.md) for setting up the build-tools container and third-party-libs container (compiler environment and remaining non-Conan dependencies).
  The `buildtools` image contains the Intel oneAPI compilers, CMake, Python and Conan.
  The `third-party-libs` image extends `buildtools` with libraries not yet managed by Conan.

## One-time Conan setup

Before building for the first time (inside the container), initialise the Conan configuration.
If you use the [devcontainer](../.devcontainer/delft3d/README.md), the Conan cache is persisted
in a Docker volume across rebuilds — you only need to run this once.

**Deltares developers** (with Nexus access):
```bash
python run_conan.py initialize deltares
```

**External / open-source developers** (without Nexus access):
```bash
python run_conan.py initialize external
```

## Build steps

### Using the devcontainer (recommended)

The easiest way to build on Linux is to open this repository in the
[devcontainer](../.devcontainer/delft3d/README.md). The devcontainer is based on the
`third-party-libs` image and persists your Conan cache in a Docker volume.
Once the container is running:

```bash
cd /workspaces/delft3d

# One-time Conan setup (Deltares developers)
python run_conan.py initialize deltares

# Build (downloads pre-built Conan binaries from Nexus)
python build.py --config fm-suite --build --build-type Release
```

### Using `build.py` in an interactive container

When not using the devcontainer, run the build inside the third-party-libs container directly:

```bash
docker run -it -v .:/delft3d containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:oneapi-2024-ifx-release
```

Inside the container:
```bash
cd /delft3d

# One-time Conan setup (Deltares developers)
python run_conan.py initialize deltares

# Build (downloads pre-built binaries from Nexus)
python build.py --config fm-suite --build --build-type Release
```

**Open-source developers** without Nexus access build all dependencies from source:
```bash
python run_conan.py initialize external
python build.py --config fm-suite --build --build-type Release --build-dependencies
```

### Using `docker build` (non-interactive)

You can also use the Dockerfile for a non-interactive build:
```bash
export TAG=oneapi-2024
docker build . -f doc/delft3d.Dockerfile \
    -t localhost/delft3d:$TAG \
    --build-arg INTEL_ONEAPI_VERSION=2024 \
    --build-arg INTEL_FORTRAN_COMPILER=ifx \
    --build-arg BUILD_TYPE=Release \
    --build-arg BASE_TAG=$TAG
```

### Build arguments
The dockerfile has the following build arguments:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifx`)
- `BUILD_TYPE` (default value: `Release`)
- `CONFIGURATION` (default value: `all`)
- `THIRDPARTYLIBS_IMAGE_URL` (default value: `containers.deltares.nl/delft3d-dev/delft3d-third-party-libs`)
- `BASE_IMAGE_URL` (default value: `containers.deltares.nl/base_linux_containers/8-base:latest`)
- `BASE_TAG` (default value: `oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE}`)

The `INTEL_ONEAPI_VERSION` build argument is used to select the right `buildtools` image.
Valid values are `2023` and `2024`.

The `INTEL_FORTRAN_COMPILER` selects which Fortran compiler is used to compile the Fortran libraries (there are just a few libraries for which this is relevant).
Valid values are `ifort` and `ifx`.
The `ifort` compiler in combination with an `INTEL_ONEAPI_VERSION` with value `2024` will result in a lot of warnings during compilation, since the `ifort` compiler has been deprecated and will no longer be included in the `2025` release of the intel compilers.

The `BUILD_TYPE` should be `Release` for a regular optimized, release version of the code, or `Debug` for debug version of the code.

The `CONFIGURATION` determines whether to build Delft3D 4 (`d3d4-suite`), Delft3D FM (`fm-suite`) or both ( `all`).

The `THIRDPARTYLIBS_IMAGE_URL` points to the repository where the `third-party-libs` images are located.
This URL can be set to `localhost/third-party-libs` when you would like to use a `third-party-libs` image that was built locally.
Note that the `BASE_TAG` is added automatically by the Dockerfile (see the `BASE_TAG` argument if you want to deviate from the default).

The `BASE_TAG` ensures that the `delft3d` image is based on the `third-party-libs` image with that tag.
Note that the default tag used here deviates from the one set in the prerequisites, so you will typically have to overrule it.

## Power-user workflow (Conan + CMake separately)

Inside the build container, you can drive Conan and CMake individually:

```bash
# 1. Install dependencies (generates CMakeDeps files)
#    The profile was installed by --initialize-conan; the lockfile ensures reproducibility.
conan install . --profile:all=delft3d_linux --settings:all build_type=Release \
      --output-folder=build_fm-suite_release/conan --lockfile=conan.lock

# 2. CMake configure
cmake -S ./src/cmake -B build_fm-suite_release -G "Unix Makefiles" \
      -D CONFIGURATION_TYPE:STRING="fm-suite" \
      -D CMAKE_BUILD_TYPE=Release \
      -D CMAKE_INSTALL_PREFIX=./install_fm-suite_release

# 3. Build & install
cmake --build build_fm-suite_release --parallel
cmake --install build_fm-suite_release
```

To build missing dependencies from source (e.g. after changing a recipe):
```bash
conan install . --profile:all=delft3d_linux --settings:all build_type=Release \
      --output-folder=build_fm-suite_release/conan --build=missing
```

## Run
Now, you should be able to run your first simulations.

### Delft3D 4
For a first Delft3D 4 simulation, change to the examples folder:
```
cd examples/delft3d4/01_standard
```
Run the simulation
```
sudo docker run --rm -v "$PWD":/work -w /work localhost/delft3d:oneapi-2024 /delft3d/bin/run_dflow2d3d.sh
```

### Delft3D FM
For a first Delft3D FM simulation, change to the examples folder:
```
cd examples/dflowfm/01_dflowfm_sequential
```
Run the simulation
```
sudo docker run --rm -v "$PWD":/work -w /work localhost/delft3d:oneapi-2024 /delft3d/bin/run_dimr.sh
```
