#!/bin/bash
#. /usr/share/Modules/init/bash -DCMAKE_Fortran_COMPILER=mpiifort -DCMAKE_CXX_COMPILER=mpicxx

module load intel/2023.1.0
module load intelmpi/2021.10.0
source /opt/apps/intelmpi/2021.10.0/setvars.sh
module load netcdf/4.9.2_4.6.1_intel2023.1.0
module load gdal/3.6.3_intel2023.1.0
module load gcc/12.2.0_gcc12.2.0
#      . $SETVARS_VARS_PATH -ofi_internal=1
     export FC=mpiifort
     export CXX=mpiicx
     export CC=mpiicc
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/u/buwalda/petsc/arch-linux-c-opt/lib/pkgconfig/
#rm -r -f build_all
cmake ./src/cmake -DCMAKE_CXX_COMPILER=mpicxx -D CONFIGURATION_TYPE:STRING=All -D CMAKE_BUILD_TYPE=Debug -B build_all

cd build_all
cmake --build . -j --target install --config Debug 