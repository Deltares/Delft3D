# syntax=docker/dockerfile:1.4

FROM containers.deltares.nl/delft3d-dev/almalinux:8.10 AS buildtools

ARG INTEL_ONEAPI_VERSION=2024

# Install intel C/C++ and Fortran compilers, the
# math kernel library and MPI library/tools
RUN --mount=type=cache,target=/var/cache/dnf <<"EOF"
set -eo pipefail

cat <<EOT > /etc/yum.repos.d/oneAPI.repo
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOT

if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    COMPILER_DPCPP_CPP_VERSION="2023.2.1"
    COMPILER_FORTRAN_VERSION="2023.2.1"
    MKL_DEVEL_VERSION="2023.2.0"
    MPI_DEVEL_VERSION="2021.13"
elif [[ $INTEL_ONEAPI_VERSION = "2024" ]]; then
    COMPILER_DPCPP_CPP_VERSION="2024.2"
    COMPILER_FORTRAN_VERSION="2024.2"
    MKL_DEVEL_VERSION="2024.2"
    MPI_DEVEL_VERSION="2021.13"
fi

dnf update -y
dnf install -y epel-release
dnf config-manager --set-enabled powertools
dnf install -y \
    which binutils patchelf diffutils procps m4 make gcc gcc-c++ \
    openssl openssl-devel wget perl python3 \
    intel-oneapi-compiler-dpcpp-cpp-${COMPILER_DPCPP_CPP_VERSION} \
    intel-oneapi-compiler-fortran-${COMPILER_FORTRAN_VERSION} \
    intel-oneapi-mkl-devel-${MKL_DEVEL_VERSION} \
    intel-oneapi-mpi-devel-${MPI_DEVEL_VERSION}

if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    # For some reason, in oneapi 2023, the latest symlink is not set correctly.
    ln -s --force -T /opt/intel/oneapi/mpi/2021.13 /opt/intel/oneapi/mpi/latest 
fi
EOF

# Build autotools, because some libraries require recent versions of it.
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

for URL in \
    'https://ftp.gnu.org/gnu/autoconf/autoconf-2.72.tar.xz' \
    'https://ftp.gnu.org/gnu/automake/automake-1.17.tar.xz' \
    'https://ftp.gnu.org/gnu/libtool/libtool-2.4.7.tar.xz'
do
    BASEDIR=$(basename -s '.tar.xz' "$URL")
    if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
        echo "CACHED ${BASEDIR}"
    else
        echo "Fetching ${BASEDIR}.tar.xz..."
        wget -q -O - "$URL" | tar -xJf - -C '/var/cache/src/'
    fi

    pushd "/var/cache/src/${BASEDIR}"
    ./configure CC=icx CXX=icpx FC=ifx CFLAGS="-O3" CXXFLAGS="-O3" FCFLAGS="-O3"
    make -j8
    make install
    popd
done
EOF

# CMake
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/Kitware/CMake/releases/download/v3.30.3/cmake-3.30.3.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${BASEDIR}.tar.gz..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

export CC=icx CXX=icpx CFLAGS="-O3" CXXFLAGS="-O3"

pushd /var/cache/src/cmake-3.30.3
./bootstrap --parallel=8
make -j8
make install
popd
EOF
