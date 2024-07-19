#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs waqpb_export on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs waqpb_export on Linux with all given command line arguments."
    echo
    echo "Usage: ${0##*/} -version*.* -serial?????????? <proc_def folder> [OPTIONS]..."
    echo
    echo "-version*.*         delwaq version number (a real number, e.g. -version7.0, mandatory)."
    echo "-serial??????????   proc_def serial number (10 digits, e.g. -serial2024071099, mandatory)."
    echo "<proc_def folder>   location of proc_def and csv files subfolder (a folder named csvFiles is assumed,"
    echo "                    e.g. . (for the current work dir), mandatory)."
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

## Check if there is a third argument given
if [ -z $3 ]; then
    echo "ERROR: not all mandatory arguments are given!"
    echo
    print_usage_info
    exit 0
fi

version=$1
serial=$2
proc_defloc=$3
csvloc=$proc_defloc/csvFiles

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    bin dir           : $bindir"
echo "    lib dir           : $libdir"
echo "    proc_def location : $proc_defloc"
echo "    csv files location: $csvloc"
echo

## Run

workdir=`pwd`
cd $csvloc
echo "executing:"
echo "$bindir/waqpb_export $version $serial"
echo
$bindir/waqpb_export $version $serial
if [ $? == 0 ]; then
    # move proc_def files
    mv -f proc_def.dat $proc_defloc
    mv -f proc_def.def $proc_defloc
else
    echo "waqpb_export did not run correctly"
fi
cd $workdir

## Wait until all child processes are finished
wait

