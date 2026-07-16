#!/bin/bash

#
# Use this script to start ESMF_RegridWeightGen in Delft3D
# Assumptions: 
# - The ESMF_RegridWeightGen binary is located in the same directory as this script
# - The name of the ESMF_RegridWeightGen binary is hard-coded in this script
# - All needed so-files are in directory ./../lib
#

if [ -f "esmf_sh.log" ]; then
  rm -rf esmf_sh.log
fi
echo screen output of ESMF_RegridWeightGen_in_Delft3D-WAVE.sh is written to this file >esmf_sh.log
echo and will be overwritten everytime that ESMF_RegridWeightGen_in_Delft3D-WAVE.sh is executed >>esmf_sh.log
echo >>esmf_sh.log

function usage () {
echo "Usage:" >>esmf_sh.log
echo "ESMF_RegridWeightGen_in_Delft3D-WAVE.bat <sourcefile> <destfile> <weightfile> [addflags]" >>esmf_sh.log
echo "   <sourcefile>: (input)    name of source file      (NetCDF)" >>esmf_sh.log
echo "   <destfile>  : (input)    name of destination file (NetCDF)" >>esmf_sh.log
echo "   <weightfile>: (output)   name of weight file      (NetCDF)" >>esmf_sh.log
echo "   [addflags]  : (optional) additional flags. Possible values: CARTESIAN" >>esmf_sh.log
}

function error1 () {
echo >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
echo     ERROR: Source file does not exist: $srcfile >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
}

function error2 () {
echo >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
echo     ERROR: Destination file does not exist: $destfile >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
}

function errorexec () {
echo >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
echo     ERROR: Executable does not exist: $regridexec >>esmf_sh.log
echo     ************************************************************** >>esmf_sh.log
}

    # Get the location of this script and ESMF_RegridWeightGen
workdir=`pwd`
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
D3D_HOME=$scriptdir/..
regridexec=$D3D_HOME/bin/ESMF_RegridWeightGen

echo Executing batchscript "ESMF_RegridWeightGen_in_Delft3D-WAVE.sh" for Delft3D-WAVE >>esmf_sh.log
echo This script is located in directory $scriptdir >>esmf_sh.log
echo Using $regridexec >>esmf_sh.log

    # Arguments
if [ "$3" == '' ]; then
    echo "ERROR: script called without the correct number of arguments" >>esmf_sh.log
    usage
    exit
fi
srcfile=$1
destfile=$2
wfile=$3
addflags='--method bilinear'
if [ "$4" == 'CARTESIAN' ]; then
    addflags="$addflags --src_loc corner --dst_loc corner"
fi

defaultflags=--ignore_unmapped
arguments="$defaultflags $addflags --source $srcfile --destination $destfile --weight $wfile"

    # Remove output file
if [ -f $wfile ]; then
    rm -f $wfile
fi
if [ -f PET0.RegridWeightGen.Log ]; then
    rm -f PET0.RegridWeightGen.Log
fi
    # Check whether needed files exist
if [ ! -f $srcfile ]; then
    error1
fi
if [ ! -f $destfile ]; then
    error2
fi
if [ ! -f $regridexec ]; then
    errorexec
fi
   # RUN
echo Calling ESMF_RegridWeightGen with arguments: $arguments >>esmf_sh.log
$regridexec $arguments >>esmf_sh.log
