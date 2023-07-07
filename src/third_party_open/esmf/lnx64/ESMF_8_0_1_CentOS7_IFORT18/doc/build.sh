#! /bin/bash

module load intel/18.0.3
module load netcdf/v4.7.4_v4.5.3_intel18.0.3
module load mpich/3.3.2_intel18.0.3

scriptdirname=`readlink \-f \$0`
rootdir=`dirname $scriptdirname`

export ESMF_DIR=$rootdir/esmf-ESMF_8_0_1
export ESMF_COMM=mpiuni
export ESMF_COMPILER=intel
export ESMF_INSTALL_PREFIX=$rootdir/results

cd esmf-ESMF_8_0_1

echo =========================================
echo gmake info:
gmake info

echo =========================================
echo make lib:
gmake all
gmake install

