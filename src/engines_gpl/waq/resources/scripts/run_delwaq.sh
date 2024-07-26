#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

#
# This script runs delwaq on Linux
#

function print_usage_info {
    echo "Purpose: Sets LD_LIBRARY_PATH and runs delwaq on Linux with all given command line arguments."
    echo "         Provides defaults for -p and -eco from ../share/delft3d when not provided by the user."
    echo
    echo "Usage: ${0##*/} <delwaq.inp> [OPTIONS]..."
    echo
    echo "Command line arguments:"
    echo "<delwaq.inp>        delwaq input file"
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

## Defaults
inputfile=
procfile=
userprocfile=
eco=
np=
userspefile=none

#
## Start processing command line options:
inputfile=$1
shift
case $inputfile in
    -h|--help|--usage)
    print_usage_info
    exit 0
    ;;
esac

while [[ $# -ge 1 ]]
do
key="$1"
shift
case $key in
    -p)
    userprocfile="$1"
    shift
    ;;
    -np)
    np=true
    ;;
    -eco)
    eco=true
    if [[ $# -ge 1 ]]
        then
        userspefile="$1" ## using only -eco would result in using the default spe-file in ../share/delft3d/
    else
        userspefile=none
    fi
    ;;
    *)
    ## always copy all additional arguments to delwaq
    echo $key
    switches="$switches $key"
    ;;
esac
done

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
sharedir=$bindir/../share/delft3d
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    bin dir          : $bindir"
echo "    lib dir          : $libdir"
echo "    share dir        : $sharedir"
echo

if [ ! "$userprocfile" == "" ]
    then
    procfile=$userprocfile
else
    procfile=$sharedir/proc_def
fi

if [ ! -f $procfile ]; then
    if [ ! -f $procfile.dat ]; then
        echo "ERROR: procfile $procfile does not exist"
        print_usage_info
        exit 1
    fi
fi

spefile=$sharedir/bloom.spe
if [ "$eco" == "true" ]
   then
   if [ ! -f $userspefile ]; then
       if [ ! -f $spefile ]; then
          echo "ERROR: default bloom.spe $spefile does not exist"
          echo "ERROR: the optional specified bloom.spe $userspefile does not exist either"
          print_usage_info
          exit 1
       else
          echo "Using default bloom.spe"
       fi
   else
       echo "Using specified bloom.spe $userspefile"
       spefile=$userspefile
   fi
fi

echo "    input file       : $inputfile"
echo "    proc_def file    : $procfile"
if [ "$eco" == "true" ]; then
   echo "    bloom.spe file   : $spefile"
fi
echo

## Run
if [ "$eco" == "true" ]; then
    echo "executing:"
    echo "$bindir/delwaq $inputfile -p $procfile -eco $spefile $switches"
    echo
    $bindir/delwaq $inputfile -p "$procfile" -eco "$spefile" $switches
elif [ "$np" == "true" ]; then
    echo "executing:"
    echo "$bindir/delwaq $inputfile -np $switches"
    echo
    $bindir/delwaq $inputfile -np $switches
else
    echo "executing:"
    echo "$bindir/delwaq $inputfile -p $procfile $switches"
    echo
    $bindir/delwaq $inputfile -p "$procfile" $switches
fi

# Wait until all child processes are finished
wait
