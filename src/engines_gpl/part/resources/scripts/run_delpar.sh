#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delpar on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <delpar.inp> [OPTION]..."
    echo "Run a Delpar model on Linux."
    echo
    echo "<*.inp>/runid.par"
    echo "       Delpar input file/Delpar configuration file, containing name of *.inp and *.mdu file"
    echo
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
inputfile=
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

inputfile=$1
case $inputfile in
    -h|--help)
    print_usage_info
    ;;
esac

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
esac
done


if [ ! -f $inputfile ]; then
    if [ ! -f $inputfile.inp ]; then
        echo "ERROR: inputfile $inputfile does not exist"
        print_usage_info
    fi
fi
if [ "$inputfile" = "runid.par" ]; then
    echo "input file is set to 'runid.par'."
    inputfile=""
    # inputfile is made empty on purpose, to maintain backwards compatibility with Delft3d4
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

echo "    inputfile        : $inputfile"
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


    echo "executing:"
    echo "$bindir/delpar" $inputfile
    echo
$bindir/delpar $inputfile


    # Wait until all child processes are finished
wait

