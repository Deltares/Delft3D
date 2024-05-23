#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs maptonetcdf on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <mapFile.map> <ncFile.nc> <numLayers> [OPTIONS]"
    echo "Run maptonetcdf on Linux."
    echo
    echo  "   mapFile.map        : (Mandatory) maptonetcdf .map input file."
    echo  "   ncFile.nc          : (Mandatory) maptonetcdf .nc output file."
    echo  "   numLayers          : (Mandatory) number of layers."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
}


# ============
# === MAIN ===
# ============

#
## Defaults
ulimit -s unlimited


#
## Start processing command line options:

mapfile=$1
ncfile=$2
numLayers=$3

## Remove starting and trailing double quotes (needed to get teamcity runs working)
mapfile=`sed -e 's/^"//' -e 's/"$//' <<<"$mapfile"`
ncfile=`sed -e 's/^"//' -e 's/"$//' <<<"$ncfile"`
numLayers=`sed -e 's/^"//' -e 's/"$//' <<<"$numLayers"`

case $mapfile in
    -h|--help)
    print_usage_info
    ;;
esac

workdir=`pwd`

if [ ! -f $mapfile ] || [ ! -f $ncfile ]; then
    echo "ERROR: input file $mapfile and/or $ncfile do not exist in working directory $workdir"
    print_usage_info
fi


if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    D3D_HOME=$scriptdir/..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/bin/.."
    # Scriptdir: remove "/.." at the end of the string
    scriptdir=${D3D_HOME%"/.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME


echo "    mapFile          : $mapfile"
echo "    ncFile           : $ncfile"
echo "    numLayers        : $numLayers"
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
echo 

    #
    # Set the directories containing the binaries
    #

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH

module load intelmpi/21.2.0 &>/dev/null
export FI_PROVIDER=tcp


echo "executing:"
echo "$bindir/maptonetcdf $mapfile $ncfile $numLayers"
echo 
$bindir/maptonetcdf $mapfile $ncfile $numLayers



    # Wait until all child processes are finished
wait

