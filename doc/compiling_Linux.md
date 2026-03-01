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
      --build-arg DEBUG=0 \
      --build-arg THIRDPARTYLIBS_IMAGE_URL=localhost/third-party-libs \
      --build-arg BASE_TAG=$TAG
  ```

### Build arguments
The dockerfile has three build argument:
- `INTEL_ONEAPI_VERSION` (default value: `2024`)
- `INTEL_FORTRAN_COMPILER` (default value: `ifort`)
- `DEBUG` (default value: `0`)
- `THIRDPARTYLIBS_IMAGE_URL` (default value: `containers.deltares.nl/delft3d-dev/delft3d-third-party-libs`)
- `BASE_IMAGE_URL` (default value: `containers.deltares.nl/base_linux_contaners/8-base:latest`)
- `BASE_TAG` (default value: `oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE}`)

The build arguments are the same as the ones used in the `third-party-libs` image.
The build arguments are used to select  a suitable version of the `third-party-libs` image (One that contains the right version of the compilers and libraries).

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
cd examples/dflowf/01_dflowfm_sequential
```
Run the simulation
```
sudo docker run --rm -v "$PWD":/work -w /work localhost/delft3d:oneapi-2024 /delft3d/bin/run_dimr.sh
```
