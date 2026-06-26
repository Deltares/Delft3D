# syntax=docker/dockerfile:1.4
#
# Minimal third-party library image for a *proof-of-concept* nvfortran (NVHPC) build
# of a SERIAL D-Flow FM executable, as a stepping stone towards OpenMP GPU offload.
#
# Design decisions (see chat rationale):
#   * Only ONE library genuinely needs nvfortran: netcdf-fortran (compiler-specific .mod files).
#     Everything else is C/C++ and is built with the stable GNU toolchain; the C-ABI
#     (bind(C)) boundary between those libs and nvfortran-compiled FM is compiler-agnostic.
#   * MPI comes from the OpenMPI bundled in the NVHPC SDK. We override the wrapper backends:
#       OMPI_CC=gcc, OMPI_CXX=g++, OMPI_FC=nvfortran
#     so only Fortran goes through nvfortran.
#   * Dropped vs the Intel image (not needed for a serial dflowfm PoC, no CMake change required):
#       PETSc  (disabled in CMake via if(NVHPC) -- serial uses internal Saad solver, Icgsolver=4)
#       ESMF, VTK, ASTE, pugixml, xerces-c, googletest
#   * Kept because the preCICE adapter is compiled unconditionally into dflowfm_kernel
#     (no #ifdef guard): boost, eigen, libxml2, preCICE.
#
# Build:  docker build -f ci/dockerfiles/linux/third-party-libs-nvhpc.Dockerfile -t delft3d-third-party-libs:nvhpc .

ARG NVHPC_TAG=24.5-devel-cuda12.4-ubuntu22.04
FROM nvcr.io/nvidia/nvhpc:${NVHPC_TAG} AS base

SHELL ["/bin/bash", "-eo", "pipefail", "-c"]
ENV DEBIAN_FRONTEND=noninteractive

# Use the NVHPC-bundled OpenMPI, but force per-language backends:
#   C and C++ -> GNU (stable, ABI-compatible), Fortran -> nvfortran.
ENV OMPI_CC=gcc \
    OMPI_CXX=g++ \
    OMPI_FC=nvfortran \
    LD_LIBRARY_PATH=/usr/local/lib:${LD_LIBRARY_PATH} \
    PKG_CONFIG_PATH=/usr/local/lib/pkgconfig \
    CMAKE_PREFIX_PATH=/usr/local

RUN <<"EOF"
apt-get update
apt-get install -y --no-install-recommends \
    build-essential gcc g++ make cmake wget ca-certificates \
    pkg-config m4 file uuid-dev libxml2-dev patch
rm -rf /var/lib/apt/lists/*
EOF

# The NVHPC SDK images already place nvfortran/nvc/nvc++ and the bundled OpenMPI
# wrappers (mpicc/mpic++/mpif90) on PATH, so no version-pinned PATH is needed here.
# If a future tag changes that, add the SDK bin dirs to PATH explicitly.

WORKDIR /src

# --- zlib + zstd (C) ---------------------------------------------------------
RUN <<"EOF"
set -eo pipefail
export CC=gcc CFLAGS="-O3 -fPIC"
wget -qO- https://github.com/madler/zlib/archive/refs/tags/v1.3.1.tar.gz | tar xz
cd zlib-1.3.1 && ./configure --prefix=/usr/local && make -j"$(nproc)" && make install && cd ..
wget -qO- https://github.com/facebook/zstd/archive/refs/tags/v1.5.6.tar.gz | tar xz
cd zstd-1.5.6 && make -j"$(nproc)" && make install PREFIX=/usr/local && cd ..
EOF

# --- METIS + GKlib (C) -------------------------------------------------------
RUN <<"EOF"
set -eo pipefail
GKLIB=6e7951358fd896e2abed7887196b6871aac9f2f8
METIS=a6e6a2cfa92f93a3ee2971ebc9ddfc3b0b581ab2
wget -qO- https://github.com/KarypisLab/GKlib/archive/${GKLIB}.tar.gz | tar xz
wget -qO- https://github.com/KarypisLab/METIS/archive/${METIS}.tar.gz | tar xz
cd GKlib-${GKLIB} && make config prefix=/usr/local cc=gcc && make -j"$(nproc)" && make install && cd ..
cd METIS-${METIS} && make config prefix=/usr/local cc=gcc shared=1 && make -j"$(nproc)" && make install && cd ..
EOF

# --- HDF5 (C only, parallel) -------------------------------------------------
RUN <<"EOF"
set -eo pipefail
wget -qO- https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-1_14_2.tar.gz | tar xz
cd hdf5-hdf5-1_14_2
CC=mpicc ./configure --prefix=/usr/local \
    --enable-build-mode=production \
    --enable-parallel \
    --disable-fortran \
    --disable-szlib \
    --with-zlib=/usr/local/include,/usr/local/lib
make -j"$(nproc)" && make install
EOF

# --- netcdf-c (C, parallel) --------------------------------------------------
RUN <<"EOF"
set -eo pipefail
wget -qO- https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.9.2.tar.gz | tar xz
cd netcdf-c-4.9.2 && mkdir build && cd build
cmake .. \
    -DCMAKE_C_COMPILER=mpicc \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DENABLE_PARALLEL4=ON \
    -DNETCDF_ENABLE_FILTER_SZIP=OFF \
    -DENABLE_DAP=OFF -DBUILD_UTILITIES=OFF -DENABLE_TESTS=OFF -DENABLE_BYTERANGE=OFF
make -j"$(nproc)" && make install
EOF

# --- netcdf-fortran (Fortran -> nvfortran) -----------------------------------
# This is THE stage that requires nvfortran. mpif90 -> nvfortran via OMPI_FC above.
RUN <<"EOF"
set -eo pipefail
export HDF5_PLUGIN_PATH=/usr/local/lib
wget -qO- https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz | tar xz
cd netcdf-fortran-4.6.1
CC=mpicc FC=mpif90 F90=mpif90 F77=mpif90 \
    CFLAGS="-O2 -fPIC" \
    FCFLAGS="-O2 -fPIC" FFLAGS="-O2 -fPIC" \
    ./configure --prefix=/usr/local --with-pic
make -j"$(nproc)" && make install
EOF

# --- eigen (header-only) -----------------------------------------------------
RUN <<"EOF"
set -eo pipefail
wget -qO- https://gitlab.com/libeigen/eigen/-/archive/5.0.1/eigen-5.0.1.tar.gz | tar xz
cd eigen-5.0.1
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DEIGEN_BUILD_TESTING=OFF -DEIGEN_BUILD_DOC=OFF -DEIGEN_BUILD_DEMOS=OFF
cmake --install build
EOF

# --- boost (C++ -> gcc) ------------------------------------------------------
RUN <<"EOF"
set -eo pipefail
wget -qO- https://archives.boost.io/release/1.90.0/source/boost_1_90_0.tar.gz | tar xz
cd boost_1_90_0
./bootstrap.sh --prefix=/usr/local --with-toolset=gcc
./b2 --without-python variant=release toolset=gcc link=shared pch=off threading=multi -j"$(nproc)" install
EOF

# --- preCICE (C++ -> g++, C API bridges to nvfortran) ------------------------
# Built WITH MPI (mpic++ -> g++ via OMPI_CXX). PETSc/Ginkgo/Python features off.
RUN <<"EOF"
set -eo pipefail
wget -qO- https://github.com/precice/precice/archive/v3.3.1.tar.gz | tar xz
cd precice-3.3.1
cmake -S . -B build \
    -DCMAKE_CXX_COMPILER=mpic++ \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_INSTALL_LIBDIR=lib \
    -DPRECICE_FEATURE_PETSC_MAPPING=OFF \
    -DPRECICE_FEATURE_GINKGO_MAPPING=OFF \
    -DPRECICE_FEATURE_PYTHON_ACTIONS=OFF \
    -DBUILD_SHARED_LIBS=ON \
    -DBUILD_TESTING=OFF
cmake --build build --parallel "$(nproc)"
cmake --install build
EOF

# Final environment for downstream FM build.
RUN cat >> /etc/bashrc <<'EOT' 2>/dev/null || cat >> /etc/profile <<'EOT'
export FC=mpif90
export CC=mpicc
export CXX=mpic++
export OMPI_CC=gcc OMPI_CXX=g++ OMPI_FC=nvfortran
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
export LIBRARY_PATH=/usr/local/lib:$LIBRARY_PATH
EOT
