#!/bin/bash

ulimit -s unlimited

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]... [--] [dfm_volume_toolOPTIONS]..."
    echo "Run dfm_volume_tool program."
    echo
    echo
    echo "Options for ${0##*/}:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo
    echo "--"
    echo "       All following arguments are passed to dfm_volume_tool."
    echo "       (optional)"
    echo "Options for dfm_volume_tool, see dfm_volume_tool --help"

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
    dfm_volume_tooloptions=$*
    break	# exit loop, all remaining options to dflowfm executable
    ;;
    *)
    dfm_volume_tooloptions="$key $*"
    break	# exit loop, $key+all remaining options to dflowfm executable
    ;;
esac
done

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
export D3D_HOME=$scriptdir/.. 
export LD_LIBRARY_PATH=$D3D_HOME/lib:$LD_LIBRARY_PATH

##$SCRIPT_DIR/../dflowfm "$@"
echo $D3D_HOME/bin/dfm_volume_tool $dfm_volume_tooloptions
echo $dfm_volume_tooloptions
$D3D_HOME/bin/dfm_volume_tool $dfm_volume_tooloptions

