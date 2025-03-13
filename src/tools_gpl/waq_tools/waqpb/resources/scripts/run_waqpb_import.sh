#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs waqpb_import on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs waqpb_import on Linux with all given command line arguments."
    echo
    echo "Usage:   ${0##*/} [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<proc_def folder>    Location of proc_def and csv files subfolder (mandatory)."
    echo "                     Use the character . to assume a subfolder with the name csvFiles."
    echo
    echo "-h, --help, --usage  Print this help message and exit"
    echo
    echo "-newtab, --newtab    This optional argument will disregard the content of any existing *.csv files."
    echo "                     If any are found, they will be overwritten."
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

## Check if there a first argument given
if [ -z $1 ]; then
    echo "ERROR: not all mandatory arguments are given!"
    echo
    print_usage_info
    exit 0
fi

## Check if a second argument -newtab or --newtab is given
newtabArg=""
if [ $2 = -newtab ] || [ $2 = --newtab ]; then
  newtabArg="-newtab"
fi

proc_defloc=$1
csvloc=$proc_defloc/csvFiles

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    bin dir          : $bindir"
echo "    lib dir          : $libdir"
echo "    proc_def location : $proc_defloc"
echo "    csv files location: $csvloc"
echo

## Run
workdir=`pwd`
cd $csvloc
echo "executing:"
echo "$bindir/waqpb_import $newtabArg" 
echo
$bindir/waqpb_import $newtabArg
cd $workdir


## Wait until all child processes are finished
wait

