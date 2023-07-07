#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-WAVE on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <mdw-file> [OPTION]..."
    echo "Run a Delft3D-WAVE model on Linux."
    echo
    echo "<mdw-file>"
    echo "        Name of mdw-file"

    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
mdwfile=
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

mdwfile=$1
shift
while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
    ;;
    --D3D_HOME)
    D3D_HOME="$1"
    shift
    ;;
    --NNODES)
    NNODES="$1"
    shift
    ;;
esac
done


if [ ! -f $mdwfile ]; then
    echo "ERROR: mdwfile $mdwfile does not exist"
    print_usage_info
fi


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
 
echo "    mdw-file         : $mdwfile"
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
export PATH=$bindir:$PATH

module load intelmpi/21.2.0 &>/dev/null
export FI_PROVIDER=tcp


    echo "executing:"
    echo "$bindir/wave $mdwfile 0"
    echo 
$bindir/wave $mdwfile 0 


    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
