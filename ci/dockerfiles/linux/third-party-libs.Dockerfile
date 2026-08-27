# syntax=containers.deltares.nl/docker-proxy/docker/dockerfile:1.4

ARG INTEL_ONEAPI_VERSION=2024
ARG BUILDTOOLS_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-buildtools
ARG BUILDTOOLS_IMAGE_TAG=oneapi-${INTEL_ONEAPI_VERSION}

ARG BUILDTOOLS_IMAGE_PATH=${BUILDTOOLS_IMAGE_URL}:${BUILDTOOLS_IMAGE_TAG}

FROM ${BUILDTOOLS_IMAGE_PATH} AS base

ARG INTEL_ONEAPI_VERSION
ARG INTEL_FORTRAN_COMPILER=ifx
ARG DEBUG=0
ARG CACHE_ID_SUFFIX=cache-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${DEBUG}

FROM base AS compression-libs

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=compression-libs-${CACHE_ID_SUFFIX} <<"EOF-compression-libs"
source /etc/bashrc
set -eo pipefail

export CC=icx CXX=icpx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
CXXFLAGS="$CFLAGS"
export CFLAGS CXXFLAGS

for BASEDIR_URL in \
    'zlib-1.3.1,https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz' \
    'zstd-1.5.6,https://github.com/facebook/zstd/archive/refs/tags/v1.5.6.tar.gz'
do
    BASEDIR="${BASEDIR_URL%%,*}"
    URL="${BASEDIR_URL#*,}"
    if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
        echo "CACHED ${BASEDIR}"
    else
        echo "Fetching ${URL}..."
        wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src/'
    fi

    pushd "/var/cache/src/${BASEDIR}"
    [[ -f configure ]] && ./configure
    make --jobs=$(nproc)
    make install
    popd
done
EOF-compression-libs

FROM base AS uuid

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=uuid-${CACHE_ID_SUFFIX} <<"EOF-uuid"
source /etc/bashrc
set -eo pipefail

URL='https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v2.40/util-linux-2.40.2.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

export CC=icx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
export CFLAGS

pushd "/var/cache/src/${BASEDIR}"
./configure --prefix=/usr/local --disable-all-programs --enable-libuuid
make --jobs=$(nproc)
make install
popd
EOF-uuid

FROM base AS metis

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=metis-${CACHE_ID_SUFFIX} <<"EOF-metis"
source /etc/bashrc
set -eo pipefail

GKLIB_COMMIT_ID='6e7951358fd896e2abed7887196b6871aac9f2f8'
METIS_COMMIT_ID='a6e6a2cfa92f93a3ee2971ebc9ddfc3b0b581ab2'
for BASEDIR_URL in \
    "METIS-${METIS_COMMIT_ID},https://github.com/KarypisLab/METIS/archive/${METIS_COMMIT_ID}.tar.gz" \
    "GKlib-${GKLIB_COMMIT_ID},https://github.com/KarypisLab/GKlib/archive/${GKLIB_COMMIT_ID}.tar.gz"
do
    BASEDIR="${BASEDIR_URL%%,*}"
    URL="${BASEDIR_URL#*,}"
    if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
        echo "CACHED ${BASEDIR}"
    else
        echo "Fetching ${URL}..."
        wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
    fi
done

pushd "/var/cache/src/GKlib-${GKLIB_COMMIT_ID}"
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx
else
    make config prefix=/usr/local cc=icx debug=1 gdb=1
fi
make --jobs=$(nproc)
make install

popd

pushd "/var/cache/src/METIS-${METIS_COMMIT_ID}"
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx shared=1
else
    make config prefix=/usr/local cc=icx shared=1 debug=1 gdb=1
fi
make --jobs=$(nproc)
make install
popd
EOF-metis

FROM base AS xerces-c

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=xerces-c-${CACHE_ID_SUFFIX} <<"EOF-xerces-c"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/apache/xerces-c/archive/refs/tags/v3.2.5.tar.gz'
BASEDIR='xerces-c-3.2.5'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
if [[ "$DEBUG" = "0" ]]; then
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-O3 -DNDEBUG -fPIC" -DCMAKE_CXX_FLAGS="-O3 -DNDEBUG -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-g -O0 -fPIC" -DCMAKE_CXX_FLAGS="-g -O0 -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Debug
fi
make --jobs=$(nproc)
make install
popd
EOF-xerces-c

FROM base AS hdf5

ARG INTEL_FORTRAN_COMPILER
ARG CACHE_ID_SUFFIX
# Do not allow a debug build, since the build fails for --enable-build-mode="debug"

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=hdf5-${CACHE_ID_SUFFIX} <<"EOF-hdf5"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz'
BASEDIR='hdf5-hdf5-1_14_2'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

MPIFC="mpi${INTEL_FORTRAN_COMPILER}"

pushd "/var/cache/src/${BASEDIR}"
./configure CC=mpiicx CXX=mpiicpx FC=$MPIFC \
    --prefix=/usr/local \
    --enable-build-mode="production" \
    --enable-fortran \
    --enable-parallel \
    --disable-szlib \
    --with-zlib=/usr/local/include,/usr/local/lib
make --jobs=$(nproc)
make install
popd
EOF-hdf5

FROM base AS netcdf

ARG INTEL_FORTRAN_COMPILER
ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=hdf5 --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=netcdf-c-${CACHE_ID_SUFFIX} <<"EOF-netcdf-c"
source /etc/bashrc
set -eo pipefail

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
export CMAKE_INCLUDE_PATH=/usr/local/include:$CMAKE_INCLUDE_PATH
export CMAKE_LIBRARY_PATH=/usr/local/lib:$CMAKE_LIBRARY_PATH

URL='https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.9.2.tar.gz'
BASEDIR='netcdf-c-4.9.2'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
cmake .. \
    -DCMAKE_C_COMPILER=mpiicx \
    -DCMAKE_CXX_COMPILER=mpiicpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DENABLE_PARALLEL4=ON \
    -DNETCDF_ENABLE_FILTER_SZIP=OFF \
    -DENABLE_DAP=OFF \
    -DBUILD_UTILITIES=OFF \
    -DENABLE_TESTS=OFF \
    -DENABLE_BYTERANGE=OFF


make --jobs=$(nproc)
make install
popd
EOF-netcdf-c

RUN --mount=type=cache,target=/var/cache/src/,id=netcdf-fortran-${CACHE_ID_SUFFIX} <<"EOF-netcdf-fortran"
source /etc/bashrc
set -eo pipefail

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
export CMAKE_INCLUDE_PATH=/usr/local/include:$CMAKE_INCLUDE_PATH
export CMAKE_LIBRARY_PATH=/usr/local/lib:$CMAKE_LIBRARY_PATH

URL='https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz'
BASEDIR='netcdf-fortran-4.6.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

export HDF5_PLUGIN_PATH=/usr/local/lib
[[ $DEBUG = "0" ]] \
    && FLAGS="-O3 -DNDEBUG -mcmodel=large" \
    || FLAGS="-O0 -g"
MPIFC="mpi${INTEL_FORTRAN_COMPILER}"

pushd "/var/cache/src/${BASEDIR}"
./configure CC=mpiicx CXX=mpiicpx FC=$MPIFC F90=$MPIFC F77=$MPIFC \
    CFLAGS="$FLAGS" CXXFLAGS="$FLAGS" CPPFLAGS="$FLAGS" \
    FCFLAGS="$FLAGS" FFLAGS="$FLAGS" F77FLAGS="$FLAGS" F90FLAGS="$FLAGS" \
    --enable-large-file-tests --with-pic

make --jobs=$(nproc)
make install
popd
EOF-netcdf-fortran

FROM base as esmf

# Do not provide a debug option, since ESMF is an external application that we do not link to.
ARG INTEL_FORTRAN_COMPILER
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=esmf-${CACHE_ID_SUFFIX} <<"EOF-esmf"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/esmf-org/esmf/archive/refs/tags/v8.9.1.tar.gz'
BASEDIR='esmf-8.9.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"

export ESMF_DIR="/var/cache/src/${BASEDIR}"
export ESMF_COMM=mpiuni # we do not need mpi
export ESMF_COMPILER=intel
export ESMF_C=icx
export ESMF_CXX=icpx
export ESMF_F90=${INTEL_FORTRAN_COMPILER}
export ESMF_NETCDF=split
export ESMF_NETCDF_INCLUDE=/usr/local/include
export ESMF_NETCDF_LIBPATH=/usr/local/lib
export ESMF_INSTALL_PREFIX=/usr/local
export ESMF_INSTALL_BINDIR=bin
export ESMF_INSTALL_LIBDIR=lib
export ESMF_INSTALL_HEADERDIR=include
export ESMF_INSTALL_MODDIR=include
export ESMF_INSTALL_DOCDIR=doc
export ESMF_CXXSTD=sysdefault

if [[ $DEBUG = "0" ]]; then
    export ESMF_BOPT=O
    export ESMF_OPTLEVEL=2
else
    export ESMF_BOPT=g
fi

make --jobs=$(nproc)
make install
popd
EOF-esmf

FROM base AS googletest

RUN <<"EOF-googletest"
set -eo pipefail
dnf install --assumeyes gtest-devel

mkdir -p /usr/local/lib
cp /usr/lib64/libgtest.so* /usr/local/lib/
cp /usr/lib64/libgtest_main.so* /usr/local/lib/

mkdir -p /usr/local/include
cp -r /usr/include/gtest /usr/local/include/
EOF-googletest

FROM base AS pugixml

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=pugixml-${CACHE_ID_SUFFIX} <<"EOF-pugixml"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/zeux/pugixml/releases/download/v1.15/pugixml-1.15.tar.gz'
BASEDIR='pugixml-1.15'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

cmake -S . -B build \
    -D CMAKE_BUILD_TYPE=$BUILD_TYPE \
    -D BUILD_SHARED_LIBS=ON \
    -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D CMAKE_INSTALL_LIBDIR=lib

cmake --build build --parallel $(nproc)
cmake --install build
popd
EOF-pugixml

FROM base AS all

RUN set -eo pipefail && \
    cat <<EOT >> /etc/bashrc
export FC=mpi${INTEL_FORTRAN_COMPILER}
export CXX=mpicxx # We would like to use mpiicpx, but some tests get different results
export CC=mpiicx
export LD_LIBRARY_PATH=/usr/local/lib:\$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:\$PKG_CONFIG_PATH
export CMAKE_PREFIX_PATH=/usr/local:\$CMAKE_PREFIX_PATH
export LIBRARY_PATH=/usr/local/lib:\$LIBRARY_PATH
EOT

COPY --from=uuid --link /usr/local /usr/local/
COPY --from=metis --link /usr/local /usr/local/
COPY --from=xerces-c --link /usr/local /usr/local/
COPY --from=esmf --link /usr/local/ /usr/local/
COPY --from=googletest --link /usr/local/ /usr/local/
COPY --from=pugixml --link /usr/local/ /usr/local/
