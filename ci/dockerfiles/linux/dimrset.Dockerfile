ARG INTEL_ONEAPI_VERSION=2023
ARG INTEL_FORTRAN_COMPILER=ifort
ARG BUILD_TYPE=release
ARG BASE_TAG=oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE}

FROM containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:${BASE_TAG} AS build

ARG INTEL_ONEAPI_VERSION=2023
ARG INTEL_FORTRAN_COMPILER=ifort
ARG BUILD_TYPE=release

WORKDIR /source

COPY ./src ./src
COPY ./test/integration_test ./test/integration_test
COPY ./test/unit_test ./test/unit_test

RUN --mount=type=cache,target=/source/build <<"EOF"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:$PKG_CONFIG_PATH
export FC=mpi${INTEL_FORTRAN_COMPILER} CXX=mpicxx CC=mpiicx

mkdir /delft3d
cmake ./src/cmake -G "Unix Makefiles" -B build \
    -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
    -DCONFIGURATION_TYPE=all \
    -DCMAKE_INSTALL_PREFIX=/delft3d

cmake --build build -j --target install --config ${BUILD_TYPE}
EOF

FROM containers.deltares.nl/delft3d-dev/almalinux:8.10-minimal

COPY --from=build /delft3d/ /delft3d/