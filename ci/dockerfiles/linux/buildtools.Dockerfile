# syntax=docker/dockerfile:1.4

FROM containers.deltares.nl/delft3d-dev/almalinux:8.10 AS buildtools

ARG INTEL_ONEAPI_VERSION=2024

# Install intel C/C++ and Fortran compilers, the
# math kernel library and MPI library/tools
RUN --mount=type=cache,target=/var/cache/dnf,id=compilers-cache-${INTEL_ONEAPI_VERSION} <<"EOF"
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


dnf update --assumeyes
dnf install --assumeyes epel-release
dnf config-manager --set-enabled powertools
dnf install --assumeyes \
    which binutils patchelf diffutils procps m4 make \
    openssl openssl-devel wget perl python3

if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    dnf install --assumeyes \
    intel-oneapi-common-licensing-2023.2.0 \
    intel-oneapi-common-vars-2023.2.0 \
    intel-oneapi-compiler-dpcpp-cpp-2023.2.1 \
    intel-oneapi-libdpstd-devel-2022.2.0
    intel-oneapi-compiler-fortran-common-2023.2.1 \
    intel-oneapi-compiler-fortran-runtime-2023.2.1 \
    intel-oneapi-compiler-shared-2023.2.1 \
    intel-oneapi-compiler-fortran-2023.2.1 \
    intel-oneapi-condaindex-2023.2.0 \
    intel-oneapi-mkl-2023.2.0 \
    intel-oneapi-mkl-common-devel-2023.2.0 \
    intel-oneapi-mkl-devel-2023.2.0 \
    intel-oneapi-mpi-2021.10.0 \
    intel-oneapi-mpi-devel-2021.10.0
elif [[ $INTEL_ONEAPI_VERSION = "2024" ]]; then
    dnf install --assumeyes \
    intel-oneapi-common-licensing-2024.2.1 \
    intel-oneapi-common-oneapi-vars-2024.2.1 \
    intel-oneapi-common-vars-2024.2.1
    intel-oneapi-compiler-dpcpp-cpp-2024.2.1 \
    intel-oneapi-dpcpp-cpp-2024.2.1 \
    intel-oneapi-libdpstd-devel-2022.6.1 \
    intel-oneapi-compiler-fortran-common-2024.2.1 \
    intel-oneapi-compiler-fortran-runtime-2024.2.1 \
    intel-oneapi-compiler-shared-2024.2.1 \
    intel-oneapi-compiler-fortran-2024.2.1 \
    intel-oneapi-mkl-classic-devel-2024.2.2 \
    intel-oneapi-mkl-sycl-devel-2024.2.2 \
    intel-oneapi-mkl-devel-2024.2.2 \
    intel-oneapi-mpi-2021.13.1 \
    intel-oneapi-mpi-devel-2021.13.1
fi

if [[ $INTEL_ONEAPI_VERSION = "2023" ]]; then
    # ifx: error #10417: Problem setting up the Intel(R) Compiler compilation environment. Requires 'install path' setting gathered from 'gcc'
    dnf install --assumeyes gcc gcc-c++
    # For some reason, in oneapi 2023, the latest symlink is not set correctly.
    ln --symbolic --force --no-target-directory /opt/intel/oneapi/mpi/2021.13 /opt/intel/oneapi/mpi/latest
fi
EOF

# Build autotools, because some libraries require recent versions of it.
RUN --mount=type=cache,target=/var/cache/src/,id=autotools-cache-${INTEL_ONEAPI_VERSION} <<"EOF"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
        wget --quiet --output-document=- "$URL" | tar --extract --xz --file=- --directory='/var/cache/src/'
    fi

    pushd "/var/cache/src/${BASEDIR}"
    ./configure CC=icx CXX=icpx FC=ifx CFLAGS="-O3" CXXFLAGS="-O3" FCFLAGS="-O3"
    make --jobs=8
    make install
    popd
done
EOF

# CMake
RUN --mount=type=cache,target=/var/cache/src/,id=cmake-cache-${INTEL_ONEAPI_VERSION} <<"EOF"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

URL='https://github.com/Kitware/CMake/releases/download/v3.30.3/cmake-3.30.3.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${BASEDIR}.tar.gz..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

export CC=icx CXX=icpx CFLAGS="-O3" CXXFLAGS="-O3"

pushd /var/cache/src/cmake-3.30.3
./bootstrap --parallel=8
make --jobs=8
make install
popd
EOF
