# Compiling Delft3D on Linux
Back to main [development page](development.md).

The preferred build procedure for Linux uses containers; this is the procedure described here.
Containers are lightweight, isolated environments that package an application and all its dependencies so it can run consistently across different systems.

Third-party dependencies are managed by [Conan 2](https://docs.conan.io/2/) and the build is driven
by two helper scripts in the repository root:

- [run_conan.py](../run_conan.py): one-time Conan configuration and dependency install.
- [build.py](../build.py): runs Conan, CMake configure, and (optionally) build and install with `make`.

Pre-built binary packages for the third-party dependencies are hosted on the Deltares Nexus
([internal-artifacts.deltares.nl](https://internal-artifacts.deltares.nl/)).
Deltares developers download them directly, while external developers build them locally from the recipes in [conan/recipes](../conan/recipes).

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

## Build steps

Our toolchain is quite elaborate and we're relying on many third-party tools and libraries.
The most reliable way to build Delft3D is to use container images that already have all the
tools required to build our software. We provide two options for building Delft3D on Linux:
1. Connecting to the [Delft3D devcontainer](/.devcontainer/delft3d/README.md) in VSCode (recommended).
2. Running the [Delft3D build container](/ci/dockerfiles/linux) interactively.

All commands below are intended to run inside one of these containers.
The Conan setup is identical for both.

### One-time Conan setup

Before building for the first time, you need to install the Conan profile (compiler/toolchain
description), configure some conan settings, and configure the remotes from where Conan downloads packages.
The helper script [run_conan.py](../run_conan.py) takes care of this.

The script uses the repository's default Conan profile. Setting `CONAN_DEFAULT_PROFILE` overrides that
selection and should be done at your own risk: it can keep selecting an outdated or incompatible profile.
When the repository's default profile changes, update the environment variable to the new profile name or unset it.

If you use the [devcontainer](../.devcontainer/delft3d/README.md), the Conan cache is persisted
in a Docker volume across container rebuilds, so you only need to do this setup once.

#### Deltares developers (with Nexus access)

Deltares hosts pre-built binary packages on the
[Nexus repository](https://internal-artifacts.deltares.nl/#browse/browse:delft3d-conan-dev).
This repository is currently not public, so you need credentials.

**1. Install the Conan configuration.**
From the repository root inside the container:
```bash
cd /workspaces/delft3d

python run_conan.py initialize deltares
```
This installs the compiler profile, global settings, and registers the Deltares Nexus remotes
(`delft3d-conan-dev` and `deltares-conan-center-proxy`). It also creates the Conan home directory
(`~/.conan2`) if it did not yet exist.

**2. Configure Nexus credentials.**
Visit the [user token page](https://internal-artifacts.deltares.nl/#user/usertoken) on Nexus
(sign in with SSO using your Deltares credentials).
Click the user button at the top right, then **User Token** and **Access user token**.
Copy the user token name and user token pass code. These tokens expire after a year
or can be reset manually.

Create a file called `credentials.json` in your Conan home directory (`~/.conan2`) with the
following content (replace `<NEXUS_USER_NAME>` and `<NEXUS_PASS_CODE>`):
```json
{
    "credentials": [
        {
            "remote": "delft3d-conan-dev",
            "user": "<NEXUS_USER_NAME>",
            "password": "<NEXUS_PASS_CODE>"
        },
        {
            "remote": "deltares-conan-center-proxy",
            "user": "<NEXUS_USER_NAME>",
            "password": "<NEXUS_PASS_CODE>"
        }
    ]
}
```

#### External / open-source developers (without Nexus access)

From the repository root inside the container:
```bash
cd /workspaces/delft3d

python run_conan.py initialize external
```
This installs the same compiler profile and settings.
You will build all third-party dependencies locally from the recipes in [conan/recipes](../conan/recipes).
Once built, the packages are cached in `~/.conan2` and reused on
subsequent builds. You only have to rebuild them when the recipes change.

When invoking the build script you will need to pass the additional `--build-dependencies` flag (see below).

### Build Delft3D using the devcontainer (recommended)

The easiest way to build on Linux is to open this repository in the
[devcontainer](../.devcontainer/delft3d/README.md). The devcontainer is based on the
`third-party-libs` image and persists your Conan cache in a Docker volume.
Once the container is running:

```bash
cd /workspaces/delft3d

# Build (downloads pre-built Conan binaries from Nexus, configures CMake, compiles sources)
python build.py --config fm-suite --build --build-type Release
```

External developers (after `run_conan.py initialize external`) build all dependencies from source
the first time:
```bash
python build.py --config fm-suite --build --build-type Release --build-dependencies
```
Subsequent invocations reuse the cached packages, so `--build-dependencies` is only needed again
after a recipe change. Still, passing `--build-dependencies` does nothing unless there are
missing third-party packages.

See `python build.py --help` for all options (e.g. `--config d3d4-suite` or `--config all`).

### Build Delft3D in an interactive container

When not using the devcontainer, run the build inside the third-party-libs container directly:

```bash
sudo docker run -it -v .:/delft3d containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:oneapi-2024-ifx-release
```

Inside the container, follow the same steps as above:
```bash
cd /delft3d

# One-time, if not done before in this Conan cache:
python run_conan.py initialize deltares    # or 'external'

# Build
python build.py --build --build-type Release
# External developers add --build-dependencies on the first build (or after a recipe change).
```

### Using `docker build` (non-interactive)

You can also use the Dockerfile for a non-interactive build.
This always builds all third-party dependencies from the local recipes in [conan/recipes](../conan/recipes)
(i.e. the external-developer flow) and no Nexus credentials are required:
```bash
export TAG=oneapi-2024
sudo docker build . -f doc/delft3d.Dockerfile \
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

## Power-user workflow (raw Conan + CMake)

Use Conan and CMake directly when you want to manage dependency installation and CMake
configuration separately, for example to iterate on CMake without reinstalling dependencies.

Install the repository's Conan configuration (profiles, settings, and remotes) in your Conan home:
```bash
conan config install conan/config
```
Use `conan profile list` to find an installed profile compatible with your compiler. The lockfile
[conan.lock](../conan.lock) pins recipe revisions for reproducibility. Linux uses the single-config
`Unix Makefiles` generator, so select the consumer build type during both `conan install` and CMake
configuration. Third-party packages are always built in their `Release` configuration. The CMakeDeps
generator uses `&:build_type=...` to select the configuration for the consumer, which is Delft3D.

```bash
# 1. Install dependencies (generates CMakeDeps files).
#    The first call may build packages (or download them from Nexus).
#    Use `conan profile list` to find the current profile name.
DELFT3D_CONAN_PROFILE="<PROFILE_FROM_CONAN_PROFILE_LIST>"
conan install . --profile:all="$DELFT3D_CONAN_PROFILE" \
      --settings:all build_type=Release \
      --settings:all &:build_type=Release \
      --output-folder=build_fm-suite_release/conan \
      --lockfile=conan.lock

# 2. CMake configure
cmake -S ./src/cmake -B build_fm-suite_release -G "Unix Makefiles" \
      -D CONFIGURATION_TYPE:STRING="fm-suite" \
      -D CMAKE_BUILD_TYPE=Release \
      -D CMAKE_INSTALL_PREFIX=./install_fm-suite_release

# 3. Build & install
cmake --build build_fm-suite_release --parallel
cmake --install build_fm-suite_release
```

To build missing dependencies from source (e.g. after changing a recipe), add `--build=missing`:
```bash
conan install . --profile:all="$DELFT3D_CONAN_PROFILE" \
      --settings:all build_type=Release \
      --settings:all &:build_type=Release \
      --output-folder=build_fm-suite_release/conan \
      --lockfile=conan.lock \
      --build=missing
```
Use `--build=*` (and `--remote=local-recipes`) instead to rebuild every package from the local
recipes only.

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
