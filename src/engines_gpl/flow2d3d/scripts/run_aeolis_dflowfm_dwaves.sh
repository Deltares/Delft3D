#!/bin/bash

export OMP_NUM_THREADS=1
ulimit -s unlimited

module load anaconda/lnx64_conda
source activate python3

# WARNING: Changing LD_LIBRARY_PATH in the Python script does not work. It must be defined here
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname` 
export LD_LIBRARY_PATH=$scriptdir/../lib:$LD_LIBRARY_PATH

curdir=`pwd`
echo LD_LIBRARY_PATH : $LD_LIBRARY_PATH
echo Workdirectory   : $curdir
echo Executing python AL_DF_WV_2011_2016_Hs.py
python AL_DF_WV_2011_2016_Hs.py
