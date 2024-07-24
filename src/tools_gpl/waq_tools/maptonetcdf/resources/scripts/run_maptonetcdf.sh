#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs maptonetcdf on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs maptonetcdf on Linux with all given command line arguments."
    echo
    echo "Usage: ${0##*/} <mapFile.map> <ncFile.nc> <numLayers> [OPTIONS]"
    echo
    echo "Command line arguments:"
    echo "<mapFile.map>       maptonetcdf .map input file (mandatory)."
    echo "<ncFile.nc>         maptonetcdf .nc output file (mandatory)."
    echo "<numLayers>         number of layers (mandatory)."
    echo "-h, --help, --usage print this help message and exit"
}

# ============
# === MAIN ===
# ============

## Set number of open files to unlimited
ulimit -s unlimited

## Start processing command line options:
case $1 in
    -h|--help|--usage)
    print_usage_info
    exit 0
    ;;
esac

mapfile=$1
ncfile=$2
numLayers=$3

## Remove starting and trailing double quotes (needed to get teamcity runs working)
mapfile=`sed -e 's/^"//' -e 's/"$//' <<<"$mapfile"`
ncfile=`sed -e 's/^"//' -e 's/"$//' <<<"$ncfile"`
numLayers=`sed -e 's/^"//' -e 's/"$//' <<<"$numLayers"`

workdir=`pwd`

if [ ! -f $mapfile ] || [ ! -f $ncfile ]; then
    echo "ERROR: input file $mapfile and/or $ncfile do not exist in working directory $workdir"
    print_usage_info
    exit 1
fi

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    mapFile          : $mapfile"
echo "    ncFile           : $ncfile"
echo "    numLayers        : $numLayers"
echo "    Working directory: $workdir"
echo "    Bin dir          : $bindir"
echo "    Lib dir          : $libdir"
echo

## Run
echo "executing:"
echo "$bindir/maptonetcdf $mapfile $ncfile $numLayers"
echo 
$bindir/maptonetcdf $mapfile $ncfile $numLayers

## Wait until all child processes are finished
wait

