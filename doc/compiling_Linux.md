# Compiling Delft3D on Linux
Back to main [development page](development.md).

The preferred build procedure for Linux uses containers; this is the procedure described here.
Containers are lightweight, isolated environments that package an application and all its dependencies so it can run consistently across different systems.

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
- To build the Delft3D source code, we use the `third-party-libs` container created in the last step of the prerequisites:
  ```
  # Optionally repeat: export TAG=oneapi-2024
  docker build . -f doc/delft3d.Dockerfile \
      -t localhost/delft3d:$TAG \
      --build-arg INTEL_ONEAPI_VERSION=2024 \
      --build-arg INTEL_FORTRAN_COMPILER=ifx \
      --build-arg BUILD_TYPE=Release \
      --build-arg THIRDPARTYLIBS_IMAGE_URL=localhost/third-party-libs \
      --build-arg BASE_TAG=$TAG
  ```

### Build arguments
The dockerfile has seven build argument:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
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

The `BUILD_TYPE` should be `Release` for a regular optimized, release version of the code, `Debug` for debug version of the code, or `RelWithDebInfo` for something in between: a less optimized, release version of the code that still contains debug information.

The `CONFIGURATION` determines whether to build Delft3D 4 (`d3d4-suite`), Delft3D FM (`fm-suite`) or both ( `all`).

The `THIRDPARTYLIBS_IMAGE_URL` points to the repository where the `third-party-libs` images are located.
This URL can be set to `localhost/third-party-libs` when you would like to use a `third-party-libs` image that was built locally.
Note that the `BASE_TAG` is added automatically by the Dockerfile (see the `BASE_TAG` argument if you want to deviate from the default).

The `BASE_TAG` ensures that the `delft3d` image is based on the `third-party-libs` image with that tag.
Note that the default tag used here deviates from the one set in the prerequisites, so you will typically have to overrule it.

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
