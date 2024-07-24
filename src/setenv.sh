#!/bin/bash
###############################################
### load your build environment for Deltares 
#   Linux systems (H7/Teamcity)
#
# Usage:
#   source setenv.sh $COMPILER_TOOLCHAIN
# Depending on what is specified for 
# $COMPILER_TOOLCHAIN different modules are loaded.
#
# Options are:
#       - intel23     -> intel23 modules with IFORT
#       - intel24     -> intel24 modules with IFORT
#       - intel24LLVM -> intel24 modules with IFX (LLVM based Fortran compiler)
#
###############################################
echo "Module Load"

if [[ "$1" == intel24* ]]; then
     echo "Loading Intel24 compiled modules"

     module load cmake/3.30.0_intel2024.0.0
     module load intel/2024.2.0
     module load intelmpi/2021.13.0
     module load netcdf/4.9.2_4.6.1_intel2023.1.0_standalone
     module load gdal/3.6.3_intel2023.1.0_standalone
     module load proj/9.2.0_intel2024.0.0
     module load tiff/4.5.0rc3_intel2024.0.0
     module load patchelf/0.17.2_intel2023.1.0_standalone
     module load ninja/1.11.1_native
     module load petsc/3.19.0_intel2023.1.0_standalone

elif [ "$1" == "gnu" ]; then
     echo "Loading GNU compiled modules"

     module load cmake/3.30.0_intel2024.0.0
     module load gcc/12.2.0_gcc12.2.0
     module load openmpi/4.1.5_gcc12.2.0
     module load netcdf/4.9.2_4.6.1_gcc12.2.0
     module load gdal/3.6.3_gcc12.2.0
     module load proj/9.2.0_gcc12.2.0
     module load tiff/4.5.0rc3_gcc12.2.0
     module load patchelf/0.17.2_gcc12.2.0
     module load ninja/1.11.1_gcc12.2.0_standalone
     module load petsc/3.19.0_gcc12.2.0

else
     echo "Loading Intel23 compiled modules"

     module load cmake/3.30.0_intel2024.0.0
     module load intel/2023.1.0
     module load intelmpi/2021.10.0
     module load netcdf/4.9.2_4.6.1_intel2023.1.0
     module load gdal/3.6.3_intel2023.1.0
     module load patchelf/0.17.2_intel2023.1.0
     module load ninja/1.11.1_native
     #This has to be replaced by a module load eventually
     export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/apps/petsc/3.19.0_intel2023.1.0/arch-linux-c-opt/lib/pkgconfig/
fi

echo "Export environment variables"
if [ "$1" == "gnu" ]; then
     export FC=mpifort
     export CXX=mpicxx
     export CC=mpicc
else
     if [ "$1" == "intel21" ]; then
          export CXX=mpiicpc
          export CC=mpiicc
     elif [ "$1" == "intel23" ]; then
          export CXX=mpicxx
          export CC=mpiicc
     else 
          export CXX=mpiicpx
          export CC=mpiicx
     fi

     if [[ "$1" == *LLVM ]]; then
          export FC=mpiifx
     else
          export FC=mpiifort
     fi
fi

echo "FC=$FC"
echo "CXX=$CXX"
echo "CC=$CC"

