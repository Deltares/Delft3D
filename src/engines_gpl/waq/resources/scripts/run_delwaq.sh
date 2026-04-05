#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs delwaq on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs delwaq on Linux with all given command line arguments."
    echo
    echo "Usage: ${0##*/} <delwaq.inp> [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<delwaq.inp>        delwaq input file (mandatory)"
    echo "-p <proc_def>       use an alternative process library file instead of ../share/delft3d/proc_def"
    echo "-np                 do not use any Delwaq processes (all substances will be seen as tracers)"
    echo "-eco [<bloom.spe>]  use BLOOM, optionally using an alternative algea database for the default ../share/delft3d/bloom.spe"
    echo "-openpb <*.so>      use additional subroutines from an addtional library"
    echo "-validation_mode    only run the validation of the input file, do not run the actual calculation"
    echo "-*                  any other options are also passed on to the delwaq executable"
    echo "-h, --help, --usage print this help message and exit"
    echo
}

# ============
# === MAIN ===
# ============

## Set number of open files to unlimited
ulimit -s unlimited

## Check on first command line argument:
case $1 in
    ""|-h|--help|--usage)
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
echo "    input file       : $1"
echo

## Run
echo "executing:"
echo "$bindir/delwaq $*"
echo
$bindir/delwaq $*

# Wait until all child processes are finished
wait
