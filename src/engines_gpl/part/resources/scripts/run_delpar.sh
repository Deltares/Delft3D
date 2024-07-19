#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs delpar on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs delpar on Linux with all given command line arguments."
    echo
    echo "Usage:   ${0##*/} [<inp/mdp-file>] [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<inp/mdp-file>      delpar input file (optional*)"
    echo "-h, --help, --usage print this help message and exit"
    echo
    echo "* when no input file name is provided, delpar will look for a file named runid.par with the name of the input file"
}

# ============
# === MAIN ===
# ============

## Get inputfile
inputfile=$1

## Set number of open files to unlimited
ulimit -s unlimited

## Start processing command line options:
case $inputfile in
    -h|--help|--usage)
    print_usage_info
    exit 0
    ;;
    runid.par)
    inputfile=
    # inputfile is made empty on purpose, delpar looks for runid.par by default if no argument is given.
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
echo "$bindir/delpar $inputfile"
echo
$bindir/delpar $inputfile

## Wait until all child processes are finished
wait

