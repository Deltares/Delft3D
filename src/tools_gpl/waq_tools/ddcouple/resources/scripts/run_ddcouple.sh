#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs ddcouple on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs ddcouple on Linux with all given command line arguments."
    echo
    echo "Usage: ${0##*/} <ddb-file> [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<ddb-file>          ddcouple input file"
    echo "-parallel or -p     for merging parallel Delft3D FLOW output (ddb-file must be mentioned)"
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

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    bin dir          : $bindir"
echo "    lib dir          : $libdir"
echo

## Run
echo "executing:"
echo "$bindir/ddcouple $*"
echo 
$bindir/ddcouple $*

## Wait until all child processes are finished
wait

