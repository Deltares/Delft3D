#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs agrhyd on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs agrhyd on Linux with all given command line arguments."
    echo
    echo "Usage:   ${0##*/} <ini-file> [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<ini-file>          agrhyd input file"
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
echo "$bindir/agrhyd $*"
echo
$bindir/agrhyd $*

## Wait until all child processes are finished
wait

