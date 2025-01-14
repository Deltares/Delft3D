# syntax=docker/dockerfile:1.4

ARG INTEL_ONEAPI_VERSION=2024

FROM containers.deltares.nl/delft3d-dev/delft3d-buildtools:oneapi-${INTEL_ONEAPI_VERSION} AS base

ARG INTEL_FORTRAN_COMPILER=ifort
ARG INTEL_ONEAPI_VERSION=2024
ARG DEBUG=0

FROM base AS compression-libs

ARG DEBUG
RUN <<"EOF-compression-libs"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
cat << EOT | xargs -n1 -P3 -I% bash -c 'wget -q -O - % | tar -xzf - -C /tmp/apps/'
    https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz 
    https://swprojects.dkrz.de/redmine/attachments/download/453/libaec-0.3.2.tar.gz
    https://github.com/facebook/zstd/archive/refs/tags/v1.5.6.tar.gz
EOT

export CC=icx CXX=icpx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
CXXFLAGS="$CFLAGS"
export CFLAGS CXXFLAGS

for build_dir in "zlib-1.3.1" "libaec-0.3.2"; do
    pushd /tmp/apps/$build_dir
    ./configure
    make -j8
    make install
    popd
done

pushd /tmp/apps/zstd-1.5.6
make -j8 
make install
popd

rm -rf /tmp/apps
EOF-compression-libs

FROM base AS uuid

ARG DEBUG
RUN <<"EOF-uuid"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v2.40/util-linux-2.40.2.tar.gz \
    | tar -xzf - -C /tmp/apps

export CC=icx
[[ $DEBUG = "0" ]] && CFLAGS="-O3" || CFLAGS="-g -O0"
export CFLAGS

pushd /tmp/apps/util-linux-2.40.2
./configure --prefix=/usr/local --disable-all-programs --enable-libuuid
make -j8
make install
popd

rm -rf /tmp/apps
EOF-uuid

FROM base AS metis

ARG DEBUG
RUN <<"EOF-metis"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

export GKLIB_COMMIT_ID=8bd6bad750b2b0d90800c632cf18e8ee93ad72d7
mkdir -p /tmp/apps
cat << EOT | xargs -n1 -P2 -I% bash -c 'wget -q -O - % | tar -xzf - -C /tmp/apps/'
    https://github.com/KarypisLab/METIS/archive/refs/tags/v5.2.1.tar.gz
    https://github.com/KarypisLab/GKlib/archive/${GKLIB_COMMIT_ID}.tar.gz
EOT

pushd /tmp/apps/GKlib-$GKLIB_COMMIT_ID
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx debug=$DEBUG_OPT gdb=$DEBUG_OPT
else
    make config prefix=/usr/local cc=icx debug=1 gdb=1
fi
make -j8
make install
popd

pushd /tmp/apps/METIS-5.2.1
if [[ $DEBUG = "0" ]]; then
    make config prefix=/usr/local cc=icx shared=1
else
    make config prefix=/usr/local cc=icx shared=1 debug=1 gdb=1
fi
make -j8
make install
popd

rm -rf /tmp/apps
EOF-metis

FROM base AS expat

ARG DEBUG
RUN <<"EOF-expat"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://github.com/libexpat/libexpat/archive/refs/tags/R_2_6_2.tar.gz \
    | tar -xzf - -C /tmp/apps

[[ $DEBUG = "0" ]] && FLAGS="-O3 -DNDEBUG -fPIC" || FLAGS="-g -O0 -fPIC"


pushd /tmp/apps/libexpat-R_2_6_2/expat
./buildconf.sh
./configure CC=icx CXX=icpx CFLAGS="$FLAGS" CXXFLAGS="$FLAGS"
make -j8
make install
popd

rm -rf /tmp/apps
EOF-expat

FROM base AS xerces-c

ARG DEBUG
RUN <<"EOF-xerces-c"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://dlcdn.apache.org/xerces/c/3/sources/xerces-c-3.2.5.tar.gz \
    | tar -xzf - -C /tmp/apps

[[ $DEBUG = "0" ]] && FLAGS="-O3 -DNDEBUG -fPIC" || FLAGS="-g -O0 -fPIC"

pushd /tmp/apps/xerces-c-3.2.5
./configure CC=icx CXX=icpx CFLAGS="$FLAGS" CXXFLAGS="$FLAGS"
make -j8
make install
popd

rm -rf /tmp/apps
EOF-xerces-c

FROM base AS petsc

ARG INTEL_FORTRAN_COMPILER
ARG INTEL_ONEAPI_VERSION
ARG DEBUG

RUN <<"EOF-petsc"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://web.cels.anl.gov/projects/petsc/download/release-snapshots/petsc-3.19.0.tar.gz \
    | tar -xzf - -C /tmp/apps

MPIFC="mpi${INTEL_FORTRAN_COMPILER}"
[[ ${INTEL_ONEAPI_VERSION} = "2024" ]] && [[ ${INTEL_FORTRAN_COMPILER} = "ifort" ]] \
    && FFLAGS="-diag-disable=10448" || FFLAGS=""
[[ $DEBUG = "0" ]] && FLAGS="-O3" || FLAGS="-g -O0"

pushd /tmp/apps/petsc-3.19.0
./configure \
    --prefix=/usr/local \
    --with-cc=mpiicx --with-cxx=mpiicpx --with-fc=$MPIFC \
    --with-debugging=0 --COPTFLAGS="$FLAGS" --CXXOPTFLAGS="$FLAGS" --FOPTFLAGS="$FLAGS" \
    --FFLAGS="$FFLAGS"
make
make install
popd

rm -rf /tmp/apps
EOF-petsc

FROM base AS curl

ARG DEBUG

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN <<"EOF-curl"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://curl.se/download/curl-8.9.1.tar.gz \
    | tar -xzf - -C /tmp/apps

pushd /tmp/apps/curl-8.9.1
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3" CPPFLAGS="-DNDEBUG" --with-openssl --with-zlib --with-zstd
else
    ./configure CC=icx CFLAGS="-O0" --enable-debug --with-openssl --with-zlib --with-zstd
fi
make -j8
make install
popd

rm -rf /tmp/apps
EOF-curl

FROM base AS sqlite3

ARG DEBUG

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN <<"EOF-sqlite3"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://www.sqlite.org/2024/sqlite-autoconf-3460100.tar.gz \
    | tar -xzf - -C /tmp/apps

pushd /tmp/apps/sqlite-autoconf-3460100
if [[ $DEBUG = "0" ]]; then
    ./configure CC=icx CFLAGS="-O3 -DNDEBUG"
else
    ./configure CC=icx CFLAGS="-g -O0" CPPFLAGS="-DSQLITE_DEBUG"
fi
make -j8
make install
popd

rm -rf /tmp/apps
EOF-sqlite3

FROM base AS tiff

ARG DEBUG

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN <<"EOF-tiff"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://download.osgeo.org/libtiff/tiff-4.6.0.tar.gz \
    | tar -xzf - -C /tmp/apps

mkdir -p /tmp/apps/tiff-4.6.0/build
pushd /tmp/apps/tiff-4.6.0/build
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

rm -rf /tmp/apps
EOF-tiff

FROM base AS hdf5

ARG DEBUG
ARG INTEL_FORTRAN_COMPILER

COPY --from=compression-libs --link /usr/local/ /usr/local/

RUN <<"EOF-hdf5"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz \
    | tar -xzf - -C /tmp/apps

MPIFC="mpi${INTEL_FORTRAN_COMPILER}"
[[ $DEBUG = "0" ]] && BUILD_MODE="production" || BUILD_MODE="debug"

pushd /tmp/apps/hdf5-hdf5-1_14_2
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

rm -rf /tmp/apps/
EOF-hdf5

FROM base AS netcdf

ARG DEBUG
ARG INTEL_FORTRAN_COMPILER

COPY --from=hdf5 --link /usr/local/ /usr/local/
COPY --from=curl --link /usr/local/ /usr/local/

RUN <<"EOF-netcdf-c"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps/
wget -q -O - https://downloads.unidata.ucar.edu/netcdf-c/4.9.2/netcdf-c-4.9.2.tar.gz \
    | tar -xzf - -C /tmp/apps/

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

mkdir -p /tmp/apps/netcdf-c-4.9.2/build
pushd /tmp/apps/netcdf-c-4.9.2/build
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

rm -rf /tmp/apps
EOF-netcdf-c

RUN <<"EOF-netcdf-fortran"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz \
    | tar -xzf - -C /tmp/apps

export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
export HDF5_PLUGIN_PATH=/usr/local/lib
[[ $DEBUG = "0" ]] \
    && FLAGS="-O3 -DNDEBUG -mcmodel=large" \
    || FLAGS="-O0 -g"
MPIFC="mpi${INTEL_FORTRAN_COMPILER}"

pushd /tmp/apps/netcdf-fortran-4.6.1
./configure CC=mpiicx CXX=mpiicpx FC=$MPIFC F90=$MPIFC F77=$MPIFC \
    CFLAGS="$FLAGS" CXXFLAGS="$FLAGS" CPPFLAGS="$FLAGS" \
    FCFLAGS="$FLAGS" FFLAGS="$FLAGS" F77FLAGS="$FLAGS" F90FLAGS="$FLAGS" \
    --enable-large-file-tests --with-pic

make -j8
make install
popd

rm -rf /tmp/apps
EOF-netcdf-fortran

FROM base AS proj

ARG DEBUG

COPY --from=tiff --link /usr/local/ /usr/local/
COPY --from=sqlite3 --link /usr/local/ /usr/local/
COPY --from=curl --link /usr/local/ /usr/local/

RUN <<"EOF-proj"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://download.osgeo.org/proj/proj-9.2.0.tar.gz \
    | tar -xzf - -C /tmp/apps

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

mkdir -p /tmp/apps/proj-9.2.0/build
pushd /tmp/apps/proj-9.2.0/build
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

rm -rf /tmp/apps
EOF-proj

FROM base AS gdal

ARG DEBUG

COPY --from=expat --link /usr/local/ /usr/local/
COPY --from=xerces-c --link /usr/local/ /usr/local/
COPY --from=netcdf --link /usr/local/ /usr/local/
COPY --from=proj --link /usr/local/ /usr/local/

RUN <<"EOF-gdal"
set -eo pipefail
. /opt/intel/oneapi/setvars.sh

mkdir -p /tmp/apps
wget -q -O - https://github.com/OSGeo/gdal/releases/download/v3.9.2/gdal-3.9.2.tar.gz \
    | tar -xzf - -C /tmp/apps

mkdir -p /tmp/apps/gdal-3.9.2/build
pushd /tmp/apps/gdal-3.9.2/build

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

rm -rf /tmp/apps
EOF-gdal

FROM base AS all

COPY --from=uuid --link /usr/local /usr/local/
COPY --from=metis --link /usr/local /usr/local/
COPY --from=petsc --link /usr/local/ /usr/local/
COPY --from=gdal --link /usr/local/ /usr/local/