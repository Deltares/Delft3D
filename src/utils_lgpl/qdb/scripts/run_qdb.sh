#!/bin/bash

ulimit -s unlimited

function print_usage_info {
   echo "Usage: ${0##*/}"
   echo "Run qdb program."
   echo
   echo
   echo "Options for ${0##*/}:"
   echo "-h, --help"
   echo "       print this help message and exit"
   echo
   echo "Purpose: "
   echo "Sets PATH and runs qdb on Linux. QDB is an application which converts stores "
   echo "and obtains fields from a NEFIS discharge database to and from NEFIS files for "
   echo "further processing by the Simulation Management Tool (SMT), which is a quasi-"
   echo "steady hydrograph runner echo for Delft3D 4 simulations.  "
   echo ""
   echo "Usage:"
   echo "run_qdb.bat [ -h ^| --help ^| --usage]"
   echo ""
   echo "    -h ^| --help ^| --usage show this usage (optional)"
   echo ""
   echo "run_qdb.bat uses a text file \"qdb.cmd\" as input"
   echo ""
   echo "The input file \"qdb.cmd\" should be in the location where the run_qdb.bat "
   echo "script is called from and should look like the example below where the comment "
   echo "statements (# ...) are removed."
   echo ""
   echo "-------------------------contents qdb.cmd------------------------------"
   echo "STORE DON'T OVERWRITE  # Command for database - allowed options (RETRIEVE, STORE1, STORE, COPY or LIST) followed by "
   echo "2500                   # Discharge level"
   echo "trim-yac3c             # Simulation file "
   echo "qdb-yac3c              # Database file"
   echo "-----------------------------------------------------------------------"
   echo ""

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
echo $D3D_HOME/bin/qdb 
$D3D_HOME/bin/qdb
