#!/bin/bash

ulimit -s unlimited

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]... [--] [DFMOUTPUTOPTIONS]..."
    echo "Run dfmoutput program."
    echo
    echo
    echo "Options for ${0##*/}:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo
    echo "--"
    echo "       All following arguments are passed to dfmoutput."
    echo "       (optional)"
    echo "Options for dfmoutput, see dfmoutput --help"

    exit 1
}

while [[ $# -ge 1 ]]
do
key="$1"
shift
case $key in
    -h|--help)
    print_usage_info
    ;;
    --)
    dfmoutputoptions=$*
    shift $#
    break       # exit loop, all remaining options to dflowfm executable
    ;;
esac
done

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
export D3D_HOME=$scriptdir/.. 
export LD_LIBRARY_PATH=$D3D_HOME/lib:$LD_LIBRARY_PATH

##$SCRIPT_DIR/../dflowfm "$@"
echo $D3D_HOME/bin/dfmoutput --verbose $dfmoutputoptions "$@"
#echo $dfmoutputoptions
$D3D_HOME/bin/dfmoutput --verbose $dfmoutputoptions "$@"

