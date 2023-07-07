#!/bin/bash
###############################################
### load your build environment 	    ###
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
else 
     echo "Loading Intel18 compiled modules"
  
     module load    intel/18.0.3
     module display intel/18.0.3
    
     module load    mpich/3.3.2_intel18.0.3
     module display mpich/3.3.2_intel18.0.3
  
     module load    netcdf/v4.7.4_v4.5.3_intel18.0.3
     module display netcdf/v4.7.4_v4.5.3_intel18.0.3
  
     module load    petsc/3.13.3_intel18.0.3_mpich3.3.2
     module display petsc/3.13.3_intel18.0.3_mpich3.3.2
  
     module load    metis/5.1.0_intel18.0.3
     module display metis/5.1.0_intel18.0.3
  
     module load    cmake/3.18.0_intel18.0.3 
     module display cmake/3.18.0_intel18.0.3 
fi

# Shapelib is intertangled with the code in third_party_open
# loading the module is useless
#module load    shapelib/1.5.0_intel18.0.3
#module display shapelib/1.5.0_intel18.0.3

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

echo "Export environment variables"
if [ "$1" == "intel21" ]; then
     export FC=mpiifort
     export CXX=mpiicpc
     export CC=mpiicc
else
     export FC=mpif90
     export CXX=mpicxx
     export CC=mpicc
fi
echo "FC=$FC"
echo "CXX=$CXX"
echo "CC=$CC"
