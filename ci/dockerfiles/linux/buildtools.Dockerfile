# syntax=containers.deltares.nl/docker-proxy/docker/dockerfile:1.4

# note that although the BASE_IMAGE_URL argument allows you to easily change the base image,
# all the following code assumes that you're running an Alma Linux or compatible environment.
ARG BASE_IMAGE_URL=containers.deltares.nl/base_linux_containers/8-base:latest

FROM ${BASE_IMAGE_URL} AS buildtools

ARG INTEL_ONEAPI_VERSION=2024

# Install intel C/C++ and Fortran compilers, the
# math kernel library and MPI library/tools
RUN --mount=type=cache,target=/var/cache/dnf,id=compilers-cache-${INTEL_ONEAPI_VERSION} <<"EOF"
set -eo pipefail

# Enable RPM caching in the /var/cache/dnf directory.
echo 'keepcache=1' >> /etc/dnf/dnf.conf

cat <<EOT > /etc/yum.repos.d/oneAPI.repo
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOT

dnf update --assumeyes
dnf install --assumeyes epel-release
dnf config-manager --set-enabled powertools
# gcc and gcc-c++ are dependencies of the intel compilers.
# For oneAPI 2023, they are not listed as dependencies in dnf, so
# we have to install them explicitly. We explicitly install a
# modern version than the standard, since the Intel C++ compiler
# uses the standard library of gcc/g++.
dnf install --assumeyes \
    gcc-toolset-14 xz make m4  \
    binutils patchelf diffutils  \
    which less procps unzip wget git

# For Intel oneAPI, explicitly list the common-vars version, otherwise some much newer versions of packages will also be installed
# as dependencies. Furthure, do not use intel 2023.2.1, since the dependencies of mkl 2023.2.0 will then also install the C++
# runtime of the C++ compiler for 2023.2.0, duplicating the one of 2023.2.1 and increasing the image size.
# We cannot install mpi 2023.10 with oneAPI 2023, since then petsc does not build. Therefore, we use a newer version and a corresponding common-vars.
if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    COMMON_VARS_VERSION="2024.2.1"
    COMPILER_DPCPP_CPP_VERSION="2023.2.0"
    COMPILER_FORTRAN_VERSION="2023.2.0"
    MKL_DEVEL_VERSION="2023.2.0"
    MPI_DEVEL_VERSION="2021.13.1"
elif [[ $INTEL_ONEAPI_VERSION = "2024" ]]; then
    COMMON_VARS_VERSION="2024.2.1"
    COMPILER_DPCPP_CPP_VERSION="2024.2.1"
    COMPILER_FORTRAN_VERSION="2024.2.1"
    MKL_DEVEL_VERSION="2024.2.2"
    MPI_DEVEL_VERSION="2021.13.1"
elif [[ $INTEL_ONEAPI_VERSION = "2025" ]]; then
    COMMON_VARS_VERSION="2025.3.1"
    COMPILER_DPCPP_CPP_VERSION="2025.3.2"
    COMPILER_FORTRAN_VERSION="2025.3.2"
    MKL_DEVEL_VERSION="2025.3.1"
    MPI_DEVEL_VERSION="2021.17.2"
fi

dnf install --assumeyes \
    intel-oneapi-common-vars-${COMMON_VARS_VERSION} \
    intel-oneapi-compiler-dpcpp-cpp-${COMPILER_DPCPP_CPP_VERSION} \
    intel-oneapi-compiler-fortran-${COMPILER_FORTRAN_VERSION} \
    intel-oneapi-mkl-devel-${MKL_DEVEL_VERSION} \
    intel-oneapi-mpi-devel-${MPI_DEVEL_VERSION}

if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    # For some reason, in oneapi 2023, the latest symlink is not set correctly.
    ln --symbolic --force --no-target-directory /opt/intel/oneapi/mpi/2021.13 /opt/intel/oneapi/mpi/latest
fi

cat <<EOT >> /etc/bashrc
source /opt/intel/oneapi/setvars.sh
source /opt/rh/gcc-toolset-14/enable
EOT

EOF

# ninja is required for building cmake
RUN --mount=type=cache,target=/var/cache/ninja <<"EOF"
set -eo pipefail

URL=https://github.com/ninja-build/ninja/releases/download/v1.12.1/ninja-linux.zip
INSTALLER_DIR=/var/cache/ninja/$(basename $(dirname "$URL"))
if [[ -d "$INSTALLER_DIR" ]]; then
    echo "CACHED $INSTALLER_DIR"
else
    mkdir -p $INSTALLER_DIR
    wget --quiet --output-document="${INSTALLER_DIR}/ninja-linux.zip" $URL
fi

pushd $INSTALLER_DIR
unzip ninja-linux.zip
chmod +x ninja
mv ninja /usr/bin/
popd
EOF

RUN --mount=type=cache,target=/var/cache/cmake <<"EOF"
set -eo pipefail

URL=https://github.com/Kitware/CMake/releases/download/v4.2.3/cmake-4.2.3-linux-x86_64.sh
INSTALLER_DIR=/var/cache/cmake/$(basename -s '.sh' "$URL")
if [[ -d "$INSTALLER_DIR" ]]; then
    echo "CACHED $INSTALLER_DIR"
else
    mkdir -p $INSTALLER_DIR
    wget --quiet --output-document="${INSTALLER_DIR}/install_cmake.sh" $URL
fi

pushd $INSTALLER_DIR
sh install_cmake.sh --skip-license --prefix=/usr
popd
EOF

RUN wget --quiet --output-document=- https://astral.sh/uv/0.11.11/install.sh | UV_INSTALL_DIR=/usr/bin sh

RUN <<"EOF"
set -eo pipefail

# Configure `uv` to install python and tools in the `/opt/uv` directory. So they are shared with all users.
export UV_PYTHON_INSTALL_DIR=/opt/uv/share/uv/python
export UV_TOOL_DIR=/opt/uv/share/uv/tools
export UV_PYTHON_BIN_DIR=/opt/uv/bin
export UV_TOOL_BIN_DIR=/opt/uv/bin

# Install python and python tools.
uv python install 3.12 --default
uv tool install 'conan ~= 2.28.1' 
EOF

# Add python 3.12 and uv tools to PATH for all users.
ENV PATH=/opt/uv/bin:$PATH
