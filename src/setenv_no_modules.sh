#!/bin/bash
###############################################
### load your build environment 	    ###
###############################################
echo "Load dependencies:"

if [ "$1" == "intel21" ]; then
  
     # Intel compiler:
     myconfig=$config
     . /opt/apps/intel/2021.2.0/setvars.sh
     . /opt/apps/intel/2021.2.0/tbb/latest/env/vars.sh
     export config=$myconfig
 
     # Intel MPI:
     . /opt/apps/intelmpi/2021.2.0/mpi/latest/env/vars.sh -ofi_internal=1
 
     # MPI:
     export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/apps/netcdf/v4.7.4_v4.5.3_intel21.2.0/lib/pkgconfig
  
     # PetSc:
     export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/apps/petsc/3.13.3_intel21.2.0_intelmpi21.2.0_no_mkl/lib/pkgconfig
  
     # Metis:
     export METIS_DIR=/opt/apps/metis/5.1.0_intel21.2.0
  
     # CMake:
     export PATH=/opt/apps/cmake/3.19.3_intel21.2.0/bin:$PATH
else 
     echo "Sorry, only intel21 supported"
fi

# gcc:
export PATH=/opt/apps/gcc/7.3.0/bin:/opt/apps/gcc/7.3.0/include:$PATH
export LD_LIBRARY_PATH=/opt/apps/7.3.0/lib64:$LD_LIBRARY_PATH
 
# proj:
export PKG_CONFIG_PATH=/opt/apps/proj/7.1.0_gcc7.3.0/lib/pkgconfig:$PKG_CONFIG_PATH

# gdal:
export PKG_CONFIG_PATH=/opt/apps/gdal/3.1.2_gcc7.3.0/lib/pkgconfig:$PKG_CONFIG_PATH

# svn:
export PATH=/opt/apps/svn/1.9.12serf_gcc7.3.0/bin:$PATH
export LD_LIBRARY_PATH=/opt/apps/serf/1.3.9_gcc7.3.0/lib:$LD_LIBRARY_PATH

# patchelf:
export PATH=/opt/apps/patchelf/0.12/bin:$PATH


echo "Export environment variables"
if [ "$1" == "intel21" ]; then
     export FC=mpiifort
     export CXX=mpiicpc
     export CC=mpiicc
fi
echo "FC=$FC"
echo "CXX=$CXX"
echo "CC=$CC"
