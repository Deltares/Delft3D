#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs waqpb_import on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <input.mdu> [OPTION]..."
    echo "Run waqpb_import on Linux."
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
workdir=`pwd`



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

export FI_PROVIDER=tcp


echo "executing:"
echo "$bindir/waqpb_import"
echo 
$bindir/waqpb_import



    # Wait until all child processes are finished
wait

