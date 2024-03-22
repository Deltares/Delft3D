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

if [ "$1" == "intel21" ]; then
     echo "Loading Intel21 compiled modules"
  
     module load    intel/21.2.0
     module display intel/21.2.0
 
     module load    intelmpi/21.2.0
     module display intelmpi/21.2.0

      . $SETVARS_VARS_PATH -ofi_internal=1
 
     module load    netcdf/v4.7.4_v4.5.3_intel21.2.0
     module display netcdf/v4.7.4_v4.5.3_intel21.2.0
  
     module load    petsc/3.13.3_intel21.2.0_intelmpi21.2.0_no_mkl
     module display petsc/3.13.3_intel21.2.0_intelmpi21.2.0_no_mkl
  
     module load    metis/5.1.0_intel21.2.0
     module display metis/5.1.0_intel21.2.0
  
     module load    cmake/3.19.3_intel21.2.0 
     module display cmake/3.19.3_intel21.2.0 
     
     module load    gcc/7.3.0
     module display gcc/7.3.0
     
     module load    proj/7.1.0_gcc7.3.0
     module display proj/7.1.0_gcc7.3.0
     
     module load    gdal/3.1.2_gcc7.3.0
     module display gdal/3.1.2_gcc7.3.0
     
     module load    svn/1.9.12serf_gcc7.3.0
     module display svn/1.9.12serf_gcc7.3.0
     
     module load    patchelf/0.12
     module display patchelf/0.12
elif [[ "$1" == intel24* ]]; then
     echo "Loading Intel24 compiled modules"
  
     module load intel/2024.0.0
     module load intelmpi/2021.11.0
     module load cmake/3.28.1_intel2023.1.0_standalone
     module load netcdf/4.9.2_4.6.1_intel2023.1.0_standalone
     module load gdal/3.6.3_intel2023.1.0_standalone
     module load proj/9.2.0_intel2024.0.0
     module load tiff/4.5.0rc3_intel2024.0.0
     module load gcc/12.2.0_gcc12.2.0
     module load patchelf/0.17.2_intel2023.1.0_standalone
     module load ninja/1.11.1_gcc12.2.0_standalone
     module load petsc/3.19.0_intel2023.1.0_standalone
elif [ "$1" == "gnu" ]; then
     echo "Loading GNU compiled modules"
  
     module load gcc/12.2.0_gcc12.2.0
     module load cmake/3.28.1_intel2023.1.0_standalone
     module load netcdf/4.9.2_4.6.1_intel2023.1.0_standalone
     module load gdal/3.6.3_intel2023.1.0_standalone
     module load proj/9.2.0_intel2024.0.0
     module load tiff/4.5.0rc3_intel2024.0.0
     module load patchelf/0.17.2_intel2023.1.0_standalone
     module load ninja/1.11.1_gcc12.2.0_standalone
     module load petsc/3.19.0_intel2023.1.0_standalone
else 
     echo "Loading Intel23 compiled modules"
  
     module load intel/2023.1.0
     module load intelmpi/2021.10.0
     module load netcdf/4.9.2_4.6.1_intel2023.1.0
     module load gdal/3.6.3_intel2023.1.0
     module load gcc/12.2.0_gcc12.2.0
     module load patchelf/0.17.2_intel2023.1.0
     module load ninja/1.11.1_gcc12.2.0
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

