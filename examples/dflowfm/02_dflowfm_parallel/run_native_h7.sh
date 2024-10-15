#! /bin/bash
  
# Usage:
#   - Copy this script into your working folder, next to the dimr config file.
#   - Modify this script where needed (e.g. number of nodes, number of tasks per node).
#   - Execute this script from the command line of H7 using: ./run_native_h7.sh
#
# This is an h7 specific script for single or multi-node simulations.

# Set bash options. Exit on failures (and propagate errors in pipes).
set -eo pipefail

# These variables should be modified.
NODES=1
TASKS_PER_NODE=3
JOB_NAME="test_model"
PARTITION="test"
TIME_LIMIT="00:15:00"
DIMR_FOLDER="/p/d-hydro/dimrset/latest"
DIMR_FILE="${PWD}/dimr_config.xml"
  
# Compute the total number of tasks (across all nodes).
NTASKS=$(( $NODES * $TASKS_PER_NODE ))

# Modify the value of the process tag in dimr_config.xml.
PROCESS_STR="$(seq -s " " 0 $(( $NTASKS - 1 )))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESS_STR\2/" $DIMR_FILE

# The name of the MDU file is read from the DIMR configuration file.
MDU_FILENAME=$(sed -n 's/\r//; s/<inputFile>\(.*[.]mdu\)<\/inputFile>/\1/p' $DIMR_FILE | sed -n 's/^\s*\(.*\)\s*$/\1/p')

# Partition data using dflowfm.
if [[ $NTASKS -gt 1 ]]; then
    pushd dflowfm
    echo "Partitioning parallel model..."
    echo "Run dflowfm on $MDU_FILENAME with $NTASKS partitions."
    sbatch --job-name=partition_${JOB_NAME} \
        --partition=1vcpu \
        --time=00:15:00 \
        --nodes=1 \
        --ntasks-per-node=1 \
        --wait \
        ${DIMR_FOLDER}/lnx64/bin/submit_dflowfm_h7.sh \
            --partition:ndomains=${NTASKS}:icgsolver=6 $MDU_FILENAME
    popd
else
    echo "Sequential model..."
fi

# Run simulation using dimr.
echo "Run simulation..."
sbatch --job-name=$JOB_NAME \
    --partition=$PARTITION \
    --time=$TIME_LIMIT \
    --nodes=$NODES \
    --ntasks-per-node=$TASKS_PER_NODE \
    ${DIMR_FOLDER}/lnx64/bin/submit_dimr_h7.sh -m $DIMR_FILE
