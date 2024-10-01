# Linux dockerfiles

There are currently two dockerfiles in this directory:
1. `python39.Dockerfile`
2. `testbench.Dockerfile`

To run these build instructions and build the docker images, you should
have `docker` installed and configured to build `linux` docker images.
`podman` should also work.

## python39
The `python39.Dockerfile` contains build instructions to build the `python:3.9` container image. 
It uses a 'base' almalinux 8 image copied from [dockerhub](https://hub.docker.com/_/almalinux) and 
pushed to our own [Harbor registry](https://containers.deltares.nl/harbor/projects/21/repositories/almalinux/artifacts-tab).
It installs python version `3.9` using the `dnf` package manager. It is at the
time of this writing the latest version of python that is installable using
the standard `dnf` repositories. In addition, `pip` is installed as well. So
we're able to install python packages.

### Build
From the `./test/deltares_testbench/` directory:
```bash
docker build . -f ci/dockerfiles/python39.Dockerfile -t localhost/python:3.9
```

### Push
```bash
docker tag localhost/python:3.9 containers.deltares.nl/delft3d-dev/python:3.9
docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
docker push containers.deltares.nl/delft3d-dev/python:3.9
```

### Links
- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/python/artifacts-tab)


## Testbench
The `testbench.Dockerfile` contains build instructions to build the `testbench` container image.
It uses the `python` image, because it is used to run the `TestBench.py` program, which is written
in `Python`. The testbench requires the `dimrset` binaries to run the test cases described in the
testbench config XML files (stored in the `configs` directory). The binaries are copied from the
`dimrset` container image on Harbor to the place where the testbench expects to find them.

Note: This dockerfile copies entire directories of files to the container image. This directory contains a lot
of files that are unnecessary when running the test bench. We can filter out a lot of them using the `.dockerignore` file. This avoids copying unnecessary files to the build image.

### Build arguments
The dockerfile has three build argument:
- `DIMRSET_TAG` (default value: `oneapi-2024-ifort-release`)

The `DIMRSET_TAG` build argument is used to select which `dimrset` image to copy the binaries from.

### Build
From the `./test/deltares_testbench/` directory:
```bash
docker build . -f ci/dockerfiles/testbench.Dockerfile \
    -t localhost/testbench:$TAG \
    --build-arg DIMRSET_TAG=oneapi-2024-ifort-release
```
Note: Passing the build arguments is not necessary if the default value is required.

### Run
This docker image uses the 'entrypoint': `python3 TestBench.py`. Meaning that any
arguments in a `docker run` supplied after the container image will be interpreted as
arguments of the `TestBench.py` program. The default argument is `--help`.

```bash
# Prints help message:
docker run --rm -it localhost/testbench:$TAG --help

# Run with testbench arguments. Also mount the MinIO credentials file into the container.
docker run --rm -it -v=$HOME/.aws:/root/.aws:ro localhost/testbench:$TAG --compare --config=configs/dimr/dimr_dflowfm_lnx64.xml
```

### Push
```bash
docker tag localhost/testbench:$TAG containers.deltares.nl/delft3d-dev/delft3d-testbench:$TAG
docker login --username=$USERNAME --password=$TOKEN containers.deltares.nl
docker push containers.deltares.nl/delft3d-dev/delft3d-testbench:$TAG
```

### Links

- [Harbor](https://containers.deltares.nl/harbor/projects/21/repositories/delft3d-testbench/artifacts-tab)
