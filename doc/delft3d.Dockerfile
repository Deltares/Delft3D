ARG INTEL_ONEAPI_VERSION=2024
ARG INTEL_FORTRAN_COMPILER=ifx
# CMake build type (case-sensitive): Release, Debug
ARG BUILD_TYPE=Release
ARG CONFIGURATION=all
ARG THIRDPARTYLIBS_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-third-party-libs
ARG BASE_IMAGE_URL=containers.deltares.nl/base_linux_containers/8-base:latest
ARG BASE_TAG=oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE}

FROM ${THIRDPARTYLIBS_IMAGE_URL}:${BASE_TAG} AS build

ARG BUILD_TYPE
ARG CONFIGURATION

WORKDIR /source

COPY . .

RUN --mount=type=cache,target=/source/build <<"EOF"
#!/usr/bin/env bash
source /etc/bashrc
set -eo pipefail
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
export CMAKE_INCLUDE_PATH=/usr/local/include:$CMAKE_INCLUDE_PATH
export CMAKE_LIBRARY_PATH=/usr/local/lib:$CMAKE_LIBRARY_PATH

./build.sh ${CONFIGURATION} --build --build_type ${BUILD_TYPE} --build_dir "${PWD}/build" --install_dir /delft3d
EOF

FROM ${BASE_IMAGE_URL}

COPY --from=build /delft3d/ /delft3d/