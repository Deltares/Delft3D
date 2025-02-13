# syntax=docker/dockerfile:1.4

ARG INTEL_ONEAPI_VERSION=2024
ARG BUILDTOOLS_IMAGE_TAG=oneapi-${INTEL_ONEAPI_VERSION}

FROM containers.deltares.nl/delft3d-dev/delft3d-buildtools:${BUILDTOOLS_IMAGE_TAG} AS base

ARG INTEL_FORTRAN_COMPILER=ifort
ARG INTEL_ONEAPI_VERSION=2024
ARG DEBUG=0

FROM base AS compression-libs

ARG DEBUG
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-compression-libs"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

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
        wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src/'
    fi

    pushd "/var/cache/src/${BASEDIR}"
    [[ -f configure ]] && ./configure
    make -j8
    make install
    popd
done
EOF-compression-libs

FROM base AS uuid

ARG DEBUG
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-uuid"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v2.40/util-linux-2.40.2.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

export CC=icx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
export CFLAGS

pushd "/var/cache/src/${BASEDIR}"
./configure --prefix=/usr/local --disable-all-programs --enable-libuuid
make -j8
make install
popd
EOF-uuid

FROM base AS metis

ARG DEBUG
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-metis"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

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
        wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
    fi
done

pushd "/var/cache/src/GKlib-${GKLIB_COMMIT_ID}"
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx
else
    make config prefix=/usr/local cc=icx debug=1 gdb=1
fi
make -j8
make install

popd

pushd "/var/cache/src/METIS-5.2.1"
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx shared=1
else
    make config prefix=/usr/local cc=icx shared=1 debug=1 gdb=1
fi
make -j8
make install
popd
EOF-metis

FROM base AS expat

ARG DEBUG
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-expat"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/libexpat/libexpat/archive/refs/tags/R_2_6_2.tar.gz'
BASEDIR='libexpat-R_2_6_2/expat'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

[[ $DEBUG = "0" ]] && FLAGS="-O3 -DNDEBUG -fPIC" || FLAGS="-g -O0 -fPIC"

pushd "/var/cache/src/${BASEDIR}"
./buildconf.sh
./configure CC=icx CXX=icpx CFLAGS="$FLAGS" CXXFLAGS="$FLAGS"
make -j8
make install
popd
EOF-expat

FROM base AS xerces-c

ARG DEBUG
RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-xerces-c"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/apache/xerces-c/archive/refs/tags/v3.2.5.tar.gz'
BASEDIR='xerces-c-3.2.5'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

mkdir "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
if [[ "$DEBUG" = "0" ]]; then
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-O3 -DNDEBUG -fPIC" -DCMAKE_CXX_FLAGS="-O3 -DNDEBUG -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-g -O0 -fPIC" -DCMAKE_CXX_FLAGS="-g -O0 -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Debug
fi
make -j8
make install
popd
EOF-xerces-c

FROM base AS petsc

ARG INTEL_FORTRAN_COMPILER
ARG INTEL_ONEAPI_VERSION
ARG DEBUG

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-petsc"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-3.19.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
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

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-curl"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://curl.se/download/curl-8.9.1.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3" CPPFLAGS="-DNDEBUG" --with-openssl --with-zlib --with-zstd
else
    ./configure CC=icx CFLAGS="-O0" --enable-debug --with-openssl --with-zlib --with-zstd
fi
make -j8
make install
popd
EOF-curl

FROM base AS sqlite3

ARG DEBUG

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-sqlite3"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://www.sqlite.org/2024/sqlite-autoconf-3460100.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3 -DNDEBUG"
else
    ./configure CC=icx CFLAGS="-g -O0" CPPFLAGS="-DSQLITE_DEBUG"
fi
make -j8
make install
popd
EOF-sqlite3

FROM base AS tiff

ARG DEBUG

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-tiff"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://download.osgeo.org/libtiff/tiff-4.6.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

mkdir -p "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
if [[ $DEBUG = "0" ]]; then
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-O3 -DNDEBUG" -DCMAKE_CXX_FLAGS="-O3 -DNDEBUG" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
        -DCMAKE_C_FLAGS="-g -O0" -DCMAKE_CXX_FLAGS="-g -O0" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Debug
fi
make -j8
make install
popd
EOF-tiff

FROM base AS hdf5

ARG DEBUG
ARG INTEL_FORTRAN_COMPILER

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-hdf5"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz'
BASEDIR='hdf5-hdf5-1_14_2'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
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
make -j -l6
make install
popd
EOF-hdf5

FROM base AS netcdf

ARG DEBUG
ARG INTEL_FORTRAN_COMPILER

COPY --from=hdf5 --link /usr/local/ /usr/local/
COPY --from=curl --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-netcdf-c"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://downloads.unidata.ucar.edu/netcdf-c/4.9.2/netcdf-c-4.9.2.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

mkdir -p "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
cmake .. \
    -DCMAKE_C_COMPILER=mpiicx \
    -DCMAKE_CXX_COMPILER=mpiicpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DENABLE_PARALLEL4=ON \
    -DZLIB_INCLUDE_DIR=/usr/local/include \
    -DZLIB_LIBRARY=/usr/local/lib/libz.so \
    -DSzip_INCLUDE_DIRS=/usr/local/include \
    -DSzip_RELEASE_LIBRARY=/usr/local/lib/libsz.so

make -j8
make install
popd
EOF-netcdf-c

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-netcdf-fortran"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz'
BASEDIR='netcdf-fortran-4.6.1'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
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

make -j8
make install
popd
EOF-netcdf-fortran

FROM base AS proj

ARG DEBUG

COPY --from=tiff --link /usr/local/ /usr/local/
COPY --from=sqlite3 --link /usr/local/ /usr/local/
COPY --from=curl --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-proj"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://download.osgeo.org/proj/proj-9.2.0.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

mkdir -p "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
cmake .. \
    -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DSQLITE3_INCLUDE_DIR=/usr/local/include \
    -DSQLITE3_LIBRARY=/usr/local/lib/libsqlite3.so \
    -DEXE_SQLITE3=/usr/local/bin/sqlite3 \
    -DENABLE_TIFF=ON
cmake --build . --config $BUILD_TYPE -j 8
cmake --build . --target install
popd
EOF-proj

FROM base AS gdal

ARG DEBUG

COPY --from=expat --link /usr/local/ /usr/local/
COPY --from=xerces-c --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local/ /usr/local/
COPY --from=proj --link /usr/local/ /usr/local/

RUN --mount=type=cache,target=/var/cache/src/ <<"EOF-gdal"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

URL='https://github.com/OSGeo/gdal/releases/download/v3.9.2/gdal-3.9.2.tar.gz'
BASEDIR=$(basename -s '.tar.gz' "$URL")
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget -q -O - "$URL" | tar -xzf - -C '/var/cache/src'
fi

mkdir -p "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

cmake .. \
    -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DGDAL_BUILD_OPTIONAL_DRIVERS=OFF -DOGR_BUILD_OPTIONAL_DRIVERS=OFF \
    -DGDAL_USE_MYSQL=OFF -DGDAL_USE_SQLITE3=ON \
    -DGDAL_USE_HDF5=ON -DGDAL_USE_NETCDF=ON \
    -DGDAL_USE_EXPAT=ON -DGDAL_USE_XERCESC=ON \
    -DGDAL_USE_ZSTD=ON -DGDAL_USE_ZLIB=ON \
    -DGDAL_USE_TIFF=ON
    
cmake --build . --config $BUILD_TYPE -j 8
cmake --build . --target install

popd
EOF-gdal

FROM base AS all

COPY --from=uuid --link /usr/local /usr/local/
COPY --from=metis --link /usr/local /usr/local/
COPY --from=petsc --link /usr/local/ /usr/local/
COPY --from=gdal --link /usr/local/ /usr/local/