# syntax=containers.deltares.nl/docker-proxy/docker/dockerfile:1.4

ARG INTEL_ONEAPI_VERSION=2024
ARG BUILDTOOLS_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-buildtools
ARG BUILDTOOLS_IMAGE_TAG=oneapi-${INTEL_ONEAPI_VERSION}

ARG BUILDTOOLS_IMAGE_PATH=${BUILDTOOLS_IMAGE_URL}:${BUILDTOOLS_IMAGE_TAG}

FROM ${BUILDTOOLS_IMAGE_PATH} AS base

ARG INTEL_ONEAPI_VERSION
ARG INTEL_FORTRAN_COMPILER=ifort
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

FROM base AS curl-custom

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=curl-${CACHE_ID_SUFFIX} <<"EOF-curl"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

dnf install rpm-build -y
dnf download --source curl
rpm -ivh curl-*.src.rpm
cd /root/rpmbuild/SOURCES
tar xf curl-*.tar.xz
rm -f curl-*.tar.xz
cd curl-*
./configure --without-ssl --without-libpsl --prefix=/usr/local
make --jobs=$(nproc)
make install
EOF-curl

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

GKLIB_COMMIT_ID='8bd6bad750b2b0d90800c632cf18e8ee93ad72d7'
for BASEDIR_URL in \
    'METIS-5.2.1,https://github.com/KarypisLab/METIS/archive/refs/tags/v5.2.1.tar.gz' \
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

pushd "/var/cache/src/METIS-5.2.1"
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx shared=1
else
    make config prefix=/usr/local cc=icx shared=1 debug=1 gdb=1
fi
make --jobs=$(nproc)
make install
popd
EOF-metis

FROM base AS expat

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=expat-${CACHE_ID_SUFFIX} <<"EOF-expat"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/libexpat/libexpat/archive/refs/tags/R_2_6_2.tar.gz'
BASEDIR='libexpat-R_2_6_2/expat'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

[[ $DEBUG = "0" ]] && FLAGS="-O3 -DNDEBUG -fPIC" || FLAGS="-g -O0 -fPIC"

pushd "/var/cache/src/${BASEDIR}"
./buildconf.sh
./configure CC=icx CXX=icpx CFLAGS="$FLAGS" CXXFLAGS="$FLAGS"
make --jobs=$(nproc)
make install
popd
EOF-expat

FROM base AS xerces-c

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=xerxes-c-${CACHE_ID_SUFFIX} <<"EOF-xerces-c"
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

FROM base AS petsc

ARG INTEL_ONEAPI_VERSION
ARG INTEL_FORTRAN_COMPILER
ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=petsc-${CACHE_ID_SUFFIX} <<"EOF-petsc"
source /etc/bashrc
set -eo pipefail

URL='https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-3.19.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

MPIFC="mpi${INTEL_FORTRAN_COMPILER}"
[[ ${INTEL_ONEAPI_VERSION} = "2024" ]] && [[ ${INTEL_FORTRAN_COMPILER} = "ifort" ]] \
    && FFLAGS="-diag-disable=10448" || FFLAGS=""
[[ $DEBUG = "0" ]] && FLAGS="-O3" || FLAGS="-g -O0"

pushd "/var/cache/src/${BASEDIR}"
./configure \
    --prefix=/usr/local \
    --with-cc=mpiicx --with-cxx=mpiicpx --with-fc=$MPIFC \
    --with-debugging=0 --COPTFLAGS="$FLAGS" --CXXOPTFLAGS="$FLAGS" --FOPTFLAGS="$FLAGS" \
    --FFLAGS="$FFLAGS"
make
make install
popd
EOF-petsc

FROM base AS sqlite3

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=sqlite3-${CACHE_ID_SUFFIX} <<"EOF-sqlite3"
source /etc/bashrc
set -eo pipefail

URL='https://www.sqlite.org/2024/sqlite-autoconf-3460100.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3 -DNDEBUG"
else
    ./configure CC=icx CFLAGS="-g -O0" CPPFLAGS="-DSQLITE_DEBUG"
fi
make --jobs=$(nproc)
make install
popd
EOF-sqlite3

FROM base AS tiff

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=tiff-${CACHE_ID_SUFFIX} <<"EOF-tiff"
source /etc/bashrc
set -eo pipefail

URL='https://download.osgeo.org/libtiff/tiff-4.6.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
if [[ $DEBUG = "0" ]]; then
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-O3 -DNDEBUG" -DCMAKE_CXX_FLAGS="-O3 -DNDEBUG" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-g -O0" -DCMAKE_CXX_FLAGS="-g -O0" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Debug
fi
make --jobs=$(nproc)
make install
popd
EOF-tiff

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
COPY --from=curl-custom --link /usr/local/ /usr/local/

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
    -DENABLE_DAP=OFF

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

FROM base AS proj

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=tiff --link /usr/local/ /usr/local/
COPY --from=sqlite3 --link /usr/local/ /usr/local/
COPY --from=curl-custom --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=proj-${CACHE_ID_SUFFIX} <<"EOF-proj"
source /etc/bashrc
set -eo pipefail

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
export CMAKE_INCLUDE_PATH=/usr/local/include:$CMAKE_INCLUDE_PATH
export CMAKE_LIBRARY_PATH=/usr/local/lib:$CMAKE_LIBRARY_PATH

URL='https://download.osgeo.org/proj/proj-9.2.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
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
    -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DSQLITE3_INCLUDE_DIR=/usr/local/include \
    -DSQLITE3_LIBRARY=/usr/local/lib/libsqlite3.so \
    -DEXE_SQLITE3=/usr/local/bin/sqlite3 \
    -DENABLE_TIFF=ON \
    -DENABLE_CURL=OFF \
    -DBUILD_PROJSYNC=OFF \
    -DBUILD_TESTING=OFF
cmake --build . --config $BUILD_TYPE --parallel $(nproc)
cmake --build . --target install
popd
EOF-proj

FROM base AS gdal

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=expat --link /usr/local/ /usr/local/
COPY --from=xerces-c --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local/ /usr/local/
COPY --from=proj --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=gdal-${CACHE_ID_SUFFIX} <<"EOF-gdal"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/OSGeo/gdal/releases/download/v3.9.2/gdal-3.9.2.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

cmake .. \
    -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DGDAL_BUILD_OPTIONAL_DRIVERS=OFF -DOGR_BUILD_OPTIONAL_DRIVERS=OFF \
    -DGDAL_USE_MYSQL=OFF -DGDAL_USE_SQLITE3=ON \
    -DGDAL_USE_HDF5=ON -DGDAL_USE_NETCDF=ON \
    -DGDAL_USE_EXPAT=ON -DGDAL_USE_XERCESC=ON \
    -DGDAL_USE_ZSTD=ON -DGDAL_USE_ZLIB=ON \
    -DGDAL_USE_TIFF=ON -DGDAL_USE_CURL=OFF

cmake --build . --config $BUILD_TYPE --parallel $(nproc)
cmake --build . --target install

popd
EOF-gdal

FROM base as esmf

# Do not provide a debug option, since ESMF is an external application that we do not link to.
ARG INTEL_FORTRAN_COMPILER
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=esmf-${CACHE_ID_SUFFIX} <<"EOF-esmf"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/esmf-org/esmf/archive/refs/tags/v8.8.0.tar.gz'
BASEDIR='esmf-8.8.0'
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

FROM base AS boost

ARG DEBUG
ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=boost-${CACHE_ID_SUFFIX} <<"EOF-boost"
source /etc/bashrc
set -eo pipefail

URL='https://archives.boost.io/release/1.90.0/source/boost_1_90_0.tar.gz'
BASEDIR='boost_1_90_0'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"

export CC=icx CXX=icpx
[[ $DEBUG = "0" ]] && VARIANT=release || VARIANT=debug

./bootstrap.sh --prefix=/usr/local

# Patch intel-linux.jam to remove -ip flag (not supported by icpx)
sed -i 's/-O3 -ip/-O3/g' tools/build/src/tools/intel-linux.jam

./b2 --without-python variant=${VARIANT} toolset=intel-linux link=shared pch=off threading=multi -j$(nproc) install

popd
EOF-boost

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

FROM base AS eigen

ARG CACHE_ID_SUFFIX

RUN --mount=type=cache,target=/var/cache/src/,id=eigen-${CACHE_ID_SUFFIX} <<"EOF-eigen"
source /etc/bashrc
set -eo pipefail

URL='https://gitlab.com/libeigen/eigen/-/archive/5.0.1/eigen-5.0.1.tar.gz'
BASEDIR='eigen-5.0.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}"

cmake -S . -B build \
    -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icpx -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D EIGEN_BUILD_TESTING=OFF -D EIGEN_BUILD_BLAS=OFF -D EIGEN_BUILD_LAPACK=OFF -D EIGEN_BUILD_DOC=OFF -D EIGEN_BUILD_DEMOS=OFF
cmake --install build
popd
EOF-eigen

FROM base AS libxml2

RUN <<"EOF-libxml2"
set -eo pipefail
dnf install --assumeyes libxml2-devel

mkdir -p /usr/local/lib
cp /usr/lib64/libxml2.so* /usr/local/lib/

mkdir -p /usr/local/include
cp -r /usr/include/libxml2 /usr/local/include/

mkdir -p /usr/local/lib/pkgconfig
cp /usr/lib64/pkgconfig/libxml-2.0.pc /usr/local/lib/pkgconfig/
EOF-libxml2

FROM base AS precice

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=libxml2 --link /usr/local/ /usr/local/
COPY --from=eigen --link /usr/local/ /usr/local/
COPY --from=boost --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=precice-${CACHE_ID_SUFFIX} <<"EOF-precice"
source /etc/bashrc
set -eo pipefail

URL='https://github.com/precice/precice/archive/v3.3.1.tar.gz'
BASEDIR='precice-3.3.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

cmake --preset=development \
    -D CMAKE_C_COMPILER=icx -D CMAKE_CXX_COMPILER=icpx \
    -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D CMAKE_INSTALL_LIBDIR=lib \
    -D PRECICE_FEATURE_PETSC_MAPPING=OFF \
    -D PRECICE_FEATURE_GINKGO_MAPPING=OFF \
    -D PRECICE_FEATURE_PYTHON_ACTIONS=OFF \
    -D CMAKE_BUILD_TYPE=$BUILD_TYPE \
    -D BUILD_SHARED_LIBS=ON \
    -D BUILD_TESTING=OFF \
    -D CMAKE_CXX_FLAGS="-Wno-enum-constexpr-conversion"

cmake --build build --parallel $(nproc)
cmake --install build
popd
EOF-precice

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
COPY --from=petsc --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local /usr/local/
COPY --from=gdal --link /usr/local/ /usr/local/
COPY --from=esmf --link /usr/local/ /usr/local/
COPY --from=boost --link /usr/local/ /usr/local/
COPY --from=googletest --link /usr/local/ /usr/local/
COPY --from=precice --link /usr/local/ /usr/local/
COPY --from=curl-custom --link /usr/local /usr/local/
