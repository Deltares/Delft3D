#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW mormerge on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <mm-file> [OPTION]..."
    echo "Run a Delft3D-FLOW mormerge model on Linux."
    echo
    echo "<mm-file>"
    echo "       name of the mm-file"
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-scriptfile <scriptfile>"
    echo "       name of mormerge script file, default mormerge.tcl"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
mmfile=
scriptfile=
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

mmfile=$1
shift
while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
    ;;
    -scriptfile)
    scriptfile="$1"
    shift
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


if [ ! -f $mmfile ]; then
    echo "ERROR: mm-file $mmfile does not exist"
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
 
if [ -z "$scriptfile" ]; then
    export scriptfile=$D3D_HOME/bin/mormerge.tcl
fi
if [ ! -f $scriptfile ]; then
    echo "ERROR: scriptfile $scriptfile does not exist"
    print_usage_info
fi

echo "    mm-file          : $mmfile"
echo "    scriptfile       : $scriptfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
echo 

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH


    echo "executing:"
    echo "$scriptfile -i $mmfile -s $scriptfile"
    echo 
    $scriptfile -i $mmfile -s $scriptfile



    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
