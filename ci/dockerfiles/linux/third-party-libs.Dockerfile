# syntax=docker/dockerfile:1.4

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

export CC=icx CXX=icpx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
CXXFLAGS="$CFLAGS"
export CFLAGS CXXFLAGS

for BASEDIR_URL in \
    'zlib-1.3.1,https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz' \
    'libaec-0.3.2,https://swprojects.dkrz.de/redmine/attachments/download/453/libaec-0.3.2.tar.gz' \
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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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

FROM base AS curl

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=curl-${CACHE_ID_SUFFIX} <<"EOF-curl"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

URL='https://curl.se/download/curl-8.9.1.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3" CPPFLAGS="-DNDEBUG" --with-openssl --with-zlib --with-zstd
else
    ./configure CC=icx CFLAGS="-O0" --enable-debug --with-openssl --with-zlib --with-zstd
fi
make --jobs=$(nproc)
make install
popd
EOF-curl

FROM base AS sqlite3

ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=sqlite3-${CACHE_ID_SUFFIX} <<"EOF-sqlite3"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=hdf5-${CACHE_ID_SUFFIX} <<"EOF-hdf5"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

URL='https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz'
BASEDIR='hdf5-hdf5-1_14_2'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

MPIFC="mpi${INTEL_FORTRAN_COMPILER}"
[[ $DEBUG = "0" ]] && BUILD_MODE="production" || BUILD_MODE="debug"

pushd "/var/cache/src/${BASEDIR}"
./configure CC=mpiicx CXX=mpiicpx FC=$MPIFC \
    --prefix=/usr/local \
    --enable-build-mode=$BUILD_MODE \
    --enable-fortran \
    --enable-parallel \
    --with-zlib=/usr/local/include,/usr/local/lib \
    --with-szlib=/usr/local
make --jobs=$(nproc)
make install
popd
EOF-hdf5

FROM base AS netcdf

ARG INTEL_FORTRAN_COMPILER
ARG DEBUG
ARG CACHE_ID_SUFFIX

COPY --from=hdf5 --link /usr/local/ /usr/local/
COPY --from=curl --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=netcdf-c-${CACHE_ID_SUFFIX} <<"EOF-netcdf-c"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
    -DZLIB_INCLUDE_DIR=/usr/local/include \
    -DZLIB_LIBRARY=/usr/local/lib/libz.so \
    -DSzip_INCLUDE_DIRS=/usr/local/include \
    -DSzip_RELEASE_LIBRARY=/usr/local/lib/libsz.so

make --jobs=$(nproc)
make install
popd
EOF-netcdf-c

RUN --mount=type=cache,target=/var/cache/src/,id=netcdf-fortran-${CACHE_ID_SUFFIX} <<"EOF-netcdf-fortran"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

URL='https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz'
BASEDIR='netcdf-fortran-4.6.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
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
COPY --from=curl --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/,id=proj-${CACHE_ID_SUFFIX} <<"EOF-proj"
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
    -DENABLE_TIFF=ON
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
set -eo pipefail
source /opt/intel/oneapi/setvars.sh

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
    -DGDAL_USE_TIFF=ON

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
set -eo pipefail

URL='https://github.com/esmf-org/esmf/archive/refs/tags/v8.8.0.tar.gz'
BASEDIR='esmf-8.8.0'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

source /opt/intel/oneapi/setvars.sh

pushd "/var/cache/src/${BASEDIR}"

export ESMF_DIR="/var/cache/src/${BASEDIR}"
export ESMF_COMM=mpiuni # we do not need mpi per se
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

FROM base AS all

RUN set -eo pipefail && \
    cat <<EOT >> /root/.bashrc
source /opt/intel/oneapi/setvars.sh
export FC=mpi${INTEL_FORTRAN_COMPILER}
export CXX=mpicxx # We would like to use mpiicpx, but some tests get different results
export CC=mpiicx
export LD_LIBRARY_PATH=/usr/local/lib:\$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:\$PKG_CONFIG_PATH
EOT

COPY --from=uuid --link /usr/local /usr/local/
COPY --from=metis --link /usr/local /usr/local/
COPY --from=petsc --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local /usr/local/
COPY --from=gdal --link /usr/local/ /usr/local/
COPY --from=esmf --link /usr/local/ /usr/local/
