#!/bin/bash

ulimit -s unlimited

function print_usage_info {
   echo "Usage: ${0##*/}"
   echo "Run trim2dep program."
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
   echo "Purpose: "
   echo "Sets PATH and runs trim2dep on Linux. Trim2dep is an application which converts "
   echo "a field from a NEFIS trim-file to a .dep file for further processing. This is used "
   echo "by the Simulation Management Tool (SMT), which is a quasi-steady hydrograph runner "
   echo "for Delft3D 4 simulations.  "
   echo ""
   echo "run_trim2dep.bat uses a text file \"trim2dep.cmd\" as input"
   echo ""
   echo "The input file \"trim2dep.cmd\" should be in the location where the run_trim2dep.sh"
   echo "script is called from and should look like the example below where the comment "
   echo "statements (# ...) are removed."
   echo ""
   echo "-------------------------contents trim2dep.cmd------------------------------"
   echo "trimfile/trim-br1    # TRIM or COM file"
   echo "refplane.dep         # depth-file"
   echo "map-const            # group name (see Delft3D\VSI.exe)"
   echo "DPS0                 # element name"
   echo "1                    # Which record from trim/com file"
   echo "-999                 # Inactive cell value (mapconst:KCS) (Only for Reals)"
   echo "----------------------------------------------------------------------------"
   echo.

    exit 1
}

# ============
# === MAIN ===
# ============

## Set number of open files to unlimited
ulimit -s unlimited

## Start processing command line options:
case $1 in
    -h|--help|--usage)
    print_usage_info
    exit 0
    ;;
esac

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
export D3D_HOME=$scriptdir/.. 
export LD_LIBRARY_PATH=$D3D_HOME/lib:$LD_LIBRARY_PATH

##$SCRIPT_DIR/../dflowfm "$@"
echo $D3D_HOME/bin/trim2dep 
$D3D_HOME/bin/trim2dep 

