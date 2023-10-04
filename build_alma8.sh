#!/bin/bash
. /usr/share/Modules/init/bash
module use --append /opt/apps/modules
module load intel/2023.1.0
module load intelmpi/2021.10.0
#source /opt/apps/intelmpi/2021.10.0/setvars.sh
module load netcdf/4.9.2_4.6.1_intel2023.1.0
module load gdal/3.6.3_intel2023.1.0
module load gcc/12.2.0_gcc12.2.0
module load patchelf/0.17.2_intel2023.1.0

export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/apps/petsc/3.19.0_intel2023.1.0/arch-linux-c-opt/lib/pkgconfig/

#      . $SETVARS_VARS_PATH -ofi_internal=1
     export FC=mpiifort
     export CXX=mpiicx
     export CC=mpiicc

#rm -r -f build_all
cmake ./src/cmake -DCMAKE_CXX_COMPILER=mpicxx -D CONFIGURATION_TYPE:STRING=All -D CMAKE_BUILD_TYPE=Release -B build_all

cd build_all
cmake --build . -j --target install --config Release