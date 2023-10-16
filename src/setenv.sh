#!/bin/bash
###############################################
### load your build environment 	    ###
###############################################
echo "Load dependencies:"

if [ "$1" == "intel21" ]; then
  
     # Intel compiler:
     myconfig=$config
     #. /opt/apps/intel/2021.2.0/setvars.sh
     #. /opt/apps/intel/2021.2.0/tbb/latest/env/vars.sh
     export config=$myconfig
 
     # Intel MPI:
     #. /opt/apps/intelmpi/2021.2.0/mpi/latest/env/vars.sh -ofi_internal=1
     module load cmake/3.20.1
     module load szip/2.1
     #module load intel/18.0.5.274
     #module load impi/2018.0.4
     module load intel/2022.3.0
     module load impi/2022.3.0
     module load netcdf/4.7.0
  
     # CMake:
     export PATH=/apps/cmake/3.20.1/bin:$PATH
else 
     echo "Sorry, only intel21 supported"
fi

# gcc:
#export PATH=/opt/apps/gcc/7.3.0/bin:/opt/apps/gcc/7.3.0/include:$PATH
#export LD_LIBRARY_PATH=/opt/apps/7.3.0/lib64:$LD_LIBRARY_PATH

#netcdf and netcdf-fortran 
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/apps/netcdf/4.7.0/intel/18.0.5.274/impi/2018.0.4/lib/pkgconfig
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/apps/netcdf/4.7.0/intel/18.0.5.274/lib/pkgconfig
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel_release/lib/pkgconfig

#proj
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/proj_7.1.0/lib64/pkgconfig
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/proj-7.1.0/PROJ_LIBS/lib64/pkgconfig

#gdal
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/gdal_projv7/lib64/pkgconfig
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/gdal-master/GDAL_LIBS/lib64/pkgconfig

#PETSC
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/petsc/lib/pkgconfig
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/petsc-3.19.2/PETSC_LIBS/lib/pkgconfig

# Metis
export METIS_DIR=/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/metis-5.1.0/METIS_LIBS


# svn:
#export PATH=/opt/apps/svn/1.9.12serf_gcc7.3.0/bin:$PATH
#export LD_LIBRARY_PATH=/opt/apps/serf/1.3.9_gcc7.3.0/lib:$LD_LIBRARY_PATH

echo "Export environment variables"
if [ "$1" == "intel21" ]; then
     export FC=mpiifort
     export CXX=mpiicpc
     export CC=mpiicc
fi
echo "FC=$FC"
echo "CXX=$CXX"
echo "CC=$CC"
