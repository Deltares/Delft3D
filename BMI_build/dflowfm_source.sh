#!/usr/bin/env bash

####################
### (1) Load all needed environment modules.
module load cmake/3.20.1
module load szip/2.1
module load intel/2022.3.0
module load impi/2022.3.0
module load netcdf/4.7.0

export METIS_DIR=/scratch2/STI/coastal/save/COASTAL_ACT_NWC/Libs/intel/metis
export PATH=/apps/cmake/3.20.1/bin:$PATH

#netcdf and netcdf-fortran
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/apps/netcdf/4.7.0/intel/18.0.5.274/impi/2018.0.4/lib/pkgconfig

#proj
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/proj-7.1.0/PROJ_LIBS/lib64/pkgconfig

#gdal
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/gdal-master/GDAL_LIBS/lib64/pkgconfig

#PETSC
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/scratch2/NCEPDEV/ohd/Jason.Ducker/DFLOWFM_LIBS/petsc-3.19.2/PETSC_LIBS/lib/pkgconfig

export LD_LIBRARY_PATH=export LD_LIBRARY_PATH=/scratch2/NCEPDEV/ohd/Jason.Ducker/ngen_master_troute/ngen-master/extern/test_bmi_dflow/dflowfm_install/BMI/lib:$LD_LIBRARY_PATH

export FC=mpiifort
export CXX=mpiicpc
export CC=mpiicc
