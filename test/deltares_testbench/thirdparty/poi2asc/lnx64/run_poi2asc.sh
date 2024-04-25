#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs poi2asc on Linux
    # Main function is to remove starting and trailing quotes to get teamcity runs working
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <mapFile.map> <ncFile.nc> <bigEndian> [OPTIONS]"
    echo "Run poi2asc on Linux."
    echo
    echo  "   poiFile.poi        : (Mandatory) poi2asc .poi input file."
    echo  "   tableFile.table    : (Mandatory) poi2asc .table output file."
    echo  "   bigEndian          : (Optional) argument for big files (not used in current version)."
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

poiFile=$1
tableFile=$2
bigEndian=$3


## Remove beginning and trailing double quotes (needed to get teamcity runs working)
poiFile=`sed -e 's/^"//' -e 's/"$//' <<<"$poiFile"`
tableFile=`sed -e 's/^"//' -e 's/"$//' <<<"$tableFile"`
bigEndian=`sed -e 's/^"//' -e 's/"$//' <<<"$bigEndian"`


case $poiFile in
    -h|--help)
    print_usage_info
    ;;
esac

workdir=`pwd`

if [ ! -f $poiFile ]; then
    echo "ERROR: input file $poiFile does not exist in working directory $workdir"
    print_usage_info
fi


if [ -z "${bin_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    bin_HOME=$scriptdir
else
    # bin_HOME is passed through via argument --bin_HOME
    # Commonly its value is "/some/path/bin/.."
    # Scriptdir: remove "/.." at the end of the string
    scriptdir=${bin_HOME%""}
fi
if [ ! -d $bin_HOME ]; then
    echo "ERROR: directory $bin_HOME does not exist"
    print_usage_info
fi
export bin_HOME


echo "    poiFile          : $poiFile"
echo "    tableFile        : $tableFile"
echo "    bigEndian        : $bigEndian"
echo "    Working directory: $workdir"
echo 

bindir=$bin_HOME
libdir=$bin_HOME/lib


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH

module load intelmpi/21.2.0 &>/dev/null
export FI_PROVIDER=tcp


echo "executing:"
echo "$bindir/poi2asc $poiFile $tableFile $bigEndian"
echo 
$bindir/poi2asc $poiFile $tableFile $bigEndian



    # Wait until all child processes are finished
wait

