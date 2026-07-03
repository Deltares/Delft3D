#! /bin/bash
  
# Purpose:
# This script runs (coupled) Delft3D-FLOW simulations on Linux Alma8 slurm system.
# This is the master script for submitting a job to a slurm partition.
# Adapt and use it for your own purpose
#
# Usage example:
# Execute in the working directory:
# /path/to/delft3d/installation/lnx64/bin/submit_dflow2d3d.sh

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
    echo "       Number of partitions per node, default $TASKS_PER_NODE"
    echo "-h, --help"
    echo "       Print this help message and exit"
    echo "-j, --jobname <jobname>"
    echo "       Jobname prefix, default Delft3D4-FLOW"
    echo "-m, --masterfile <filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    echo "-n, --NODES <N>"
    echo "       Number of nodes, default $NODES"
    echo "-p, --PARTITION <PARTITION>"
    echo "       Slurm resource partition (queue), default $PARTITION"
    echo "-t, --TIME_LIMIT <TIME_LIMIT>"
    echo "       Upper limit for run time days-hours:minutes:seconds, default $TIME_LIMIT" 
    echo "--rtc"
    echo "       Online with RTC. Not possible with parallel Delft3D-FLOW."
    echo "-w, --wavefile <wname>"
    echo "       Name of mdw file"
    echo "--csumo"
    echo "       Path to .sh script for starting C-SUMO executable (compiled C-SUMO)"
    echo "--mcrdir"
    echo "       Folder where the Matlab Runtime Compiler can be found (compiled C-SUMO)"
    echo "--csumodir"
    echo "       Folder where the COSUMO functions can be loaded from (C-SUMO from MATLAB)"
    echo "--matlabversion"
    echo "       MATLAB version to use (C-SUMO from MATLAB)"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
configfile=config_d_hydro.xml
D3D_HOME=
runscript_extraopts=()

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
        -j|--jobname)
        JOB_NAME="$1"
        shift
        ;;
        -m|--masterfile)
        configfile="$1"
        shift
        ;;
        --)
        echo "-- sign detected, remaining options are going to be passed to dimr"
        runscript_extraopts+=("$@")
        break       # exit loop, stop shifting, all remaining arguments without dashes handled below
        ;;
        *)
        runscript_extraopts+=("$key")
        ;;
    esac
done


if [[ ! -f $configfile ]]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi


workdir=$PWD

scriptdirname=$(readlink -f "$0")
scriptdir=${scriptdirname%/*}
D3D_HOME="$scriptdir/.."
if [[ ! -d $D3D_HOME ]]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
RUNSCRIPT="$scriptdir/rd2d3d.sh"

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

runscript_opts=()
runscript_opts+=(-m "${configfile}")
runscript_opts+=(-c $TASKS_PER_NODE)
runscript_opts+=(--NODES $NODES)
runscript_opts+=(--D3D_HOME "${D3D_HOME}")
runscript_opts+=("${runscript_extraopts[@]}")
echo "    run script options        : ${runscript_opts[@]}"
# Run simulation
echo "Run simulation..."
sbatch --job-name=$JOB_NAME \
    --partition=$PARTITION \
    --time=$TIME_LIMIT \
    --nodes=$NODES \
    --ntasks-per-node=$TASKS_PER_NODE \
    -- "${RUNSCRIPT}" "${runscript_opts[@]}"
