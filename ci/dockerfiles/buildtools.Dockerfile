FROM containers.deltares.nl/delft3d-dev/almalinux:8.10 AS buildtools

ARG INTEL_ONEAPI_VERSION=2024

# Install intel C/C++ and Fortran compilers, the
# math kernel library and MPI library/tools
RUN <<"EOF-dnf-install"
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

dnf clean all
rm -rf /var/cache/dnf
EOF-dnf-install

# Build autotools, because some libraries require recent versions of it.
RUN <<"EOF-autotools"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
cat << EOT | xargs -n1 -P3 -I% bash -c 'wget -q -O - % | tar -xJf - -C /tmp/apps/'
https://ftp.gnu.org/gnu/autoconf/autoconf-2.72.tar.xz 
https://ftp.gnu.org/gnu/automake/automake-1.17.tar.xz 
https://ftp.gnu.org/gnu/libtool/libtool-2.4.7.tar.xz
EOT

for build_dir in "autoconf-2.72" "automake-1.17" "libtool-2.4.7"; do
    pushd /tmp/apps/$build_dir
    ./configure CC=icx CXX=icpx FC=ifx CFLAGS="-O3" CXXFLAGS="-O3" FCFLAGS="-O3"
    make -j8
    make install
    popd
done

rm -rf /tmp/apps
EOF-autotools

# CMake
RUN <<"EOF-cmake"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps/
wget -q -O - https://github.com/Kitware/CMake/releases/download/v3.30.3/cmake-3.30.3.tar.gz \
    | tar -xzf - -C /tmp/apps/

export CC=icx CXX=icpx CFLAGS="-O3" CXXFLAGS="-O3"

pushd /tmp/apps/cmake-3.30.3
./bootstrap --parallel=8
make -j8
make install
popd

rm -rf /tmp/apps
EOF-cmake
