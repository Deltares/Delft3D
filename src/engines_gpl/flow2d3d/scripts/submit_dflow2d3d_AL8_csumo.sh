#! /bin/bash
  
# Usage:
    #
    # This script runs Delft3D-FLOW in parallel on Linux H7
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # Execute in the working directory:
    # /path/to/delft3d/installation/lnx64/bin/submit_dflow2d3d_AL8.sh

# Set bash options. Exit on failures (and propagate errors in pipes).
set -eo pipefail

# These variables should be modified.
NODES=1
TASKS_PER_NODE=3
JOB_NAME=Delft3D4-FLOW
PARTITION="4vcpu"
TIME_LIMIT="00:15:00"
CONFIG_FILE="${PWD}/config_d_hydro.xml"
# Optional variables:


function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run a Delft3D-FLOW model in parallel on Linux."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of partitions per node, default $TASKS_PER_NODE"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-j, --jobname <jobname>"
    echo "       jobname prefix, default Delft3D4-FLOW"
    echo "-m, --masterfile <filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    echo "-n, --NODES <N>"
    echo "       number of nodes, default $NODES"
    echo "-p, --PARTITION <PARTITION>"
    echo "       PARTITION, default $PARTITION"
    echo "       see also: https://publicwiki.deltares.nl/display/Deltareken/Compute+nodes"
    echo "-t, --TIME_LIMIT <TIME_LIMIT>"
    echo "       TIME_LIMIT, default $TIME_LIMIT" 
    echo "--rtc"
    echo "       Online with RTC. Not possible with parallel Delft3D-FLOW."
    echo "-w, --wavefile <wname>"
    echo "       name of mdw file"
    echo "-csumo"
    echo "       Path to .sh script for starting C-SUMO executable"
    echo "-mcrdir"
    echo "       folder where the Matlab Runtime Compiler can be found"
    echo "-csumodeployed"
    echo "       Run a compiled COSUMO version (true) or run COSUMO from MATLAB (false)"
    echo "-csumodir"
    echo "       In case csumodeployed=false: folder where the COSUMO functions can be loaded from"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
configfile=config_d_hydro.xml
D3D_HOME=
runscript_extraopts=
wavefile=runwithoutwaveonlinebydefault
withrtc=false
csumoscript=/p/1202339-rndcoastalhd/COSUMO/01_code/exe/latest/lnx_h7_2023b/run_COSUMO.sh # latest compiled version of C-SUMO
mcrdir=/p/1202339-rndcoastalhd/COSUMO/10_MCR_linux/h7/2023b/installed/R2023b/  # Matlab Runtime compiler location (should be consistent with C-SUMO executable)
csumodeployed=true
csumodir=/p/1202339-rndcoastalhd/COSUMO/01_code/
matlabversion=2014a

ulimit -s unlimited

#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
    key="$1"
    shift

    case $key in
        -c|--corespernode)
        TASKS_PER_NODE=$1
        shift
        ;;
        -h|--help)
        print_usage_info
        ;;
        -n|--NODES)
        NODES="$1"
        shift
        ;;
        -p|--PARTITION)
        PARTITION="$1"
        shift
        ;;
        -t|--TIME_LIMIT)
        TIME_LIMIT="$1"
        shift
        ;;
        --rtc)
        withrtc=true
        shift
        ;;
        -w|--wavefile)
        wavefile="$1"
        shift
        ;;
        -j|--jobname)
        JOB_NAME="$1"
        shift
        ;;
        -m|--masterfile)
        configfile="$1"
        shift
        ;;
        -csumoscript)
        csumoscript="$1"
        shift
        ;;
        -mcrdir)
        mcrdir="$1"
        shift
        ;;
        -csumodeployed)
        csumodeployed="$1"
        shift
        ;;
        -csumodir)
        csumodir="$1"
        shift
        ;;
        -matlabversion)
        matlabversion="$1"
        shift
        ;;
        --)
        echo "-- sign detected, remained options are going to be passed to dimr"
        runscript_extraopts="$runscript_extraopts $*"
        break       # exit loop, stop shifting, all remaining arguments without dashes handled below
        ;;
        -*)
        echo "option ${key} seems dedicated for dimr, therefore passing it and the following ones to the dimr"
        runscript_extraopts="$key $*"
        break       # exit loop, $key+all remaining options to dflowfm executable
        ;;
    esac
done


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi


workdir=`pwd`

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
D3D_HOME=$scriptdir/..
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
RUNSCRIPT=$scriptdir/rd2d3d_AL8_csumo.sh
runscript_opts="$runscript_opts --D3D_HOME ${D3D_HOME}"

JOB_NAME="${JOB_NAME}_${NODES}x${TASKS_PER_NODE}"

echo "    Configfile                : $configfile"
echo "    D3D_HOME                  : $D3D_HOME"
echo "    Working directory         : $workdir"
echo "    nr of nodes               : $NODES"
echo "    nr of tasks per node      : $TASKS_PER_NODE"
echo "    SLURM Partition Name      : $PARTITION"
echo "    Maximum run time          : $TIME_LIMIT"
echo "    Job name                  : $JOB_NAME"
echo 

    #
    # Set the directories containing the binaries
    #


runscript_opts="-m ${configfile} -c $TASKS_PER_NODE"
if $csumodeployed ; then
    runscript_opts="$runscript_opts -csumoscript $csumoscript -mcrdir $mcrdir" 
else
    runscript_opts="$runscript_opts -csumodeployed $csumodeployed -csumodir $csumodir -matlabversion $matlabversion"    
fi
if [ "$wavefile" != "runwithoutwaveonlinebydefault" ]; then
    runscript_opts="$runscript_opts -w ${wavefile}"
fi
if $withrtc ; then
    runscript_opts="$runscript_opts --rtc"
fi
runscript_opts="$runscript_opts --NODES $NODES --D3D_HOME ${D3D_HOME}"
runscript_opts="$runscript_opts $runscript_extraopts"
echo "    run script options        : $runscript_opts"
# Run simulation
echo "Run simulation..."
sbatch --job-name=$JOB_NAME \
    --partition=$PARTITION \
    --time=$TIME_LIMIT \
    --nodes=$NODES \
    --ntasks-per-node=$TASKS_PER_NODE \
    ${RUNSCRIPT} ${runscript_opts}
