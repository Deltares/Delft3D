#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Waqmerge on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <input.mdu> [OPTION]..."
    echo "Run Waqmerge on Linux."
    echo
    echo "<input.mdu>"
    echo "       (Mandatory) Waqmerge input file"
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

argfile=$1
case $argfile in
    -h|--help)
    print_usage_info
    ;;
esac

workdir=`pwd`

if [ ! -f $argfile ]; then
    if [ ! -f $argfile.inp ]; then
        echo "ERROR: input mdu file $argfile does not exist in working directory $workdir"
        print_usage_info
    fi
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


echo "    Argfile       : $argfile"
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
echo "$bindir/waqmerge $argfile"
echo 
$bindir/waqmerge $argfile



    # Wait until all child processes are finished
wait

