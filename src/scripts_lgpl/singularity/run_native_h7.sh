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
# Export makes sure that these variables are available in the environments of programs
# run in this shell script.
export NODES=1
export TASKS_PER_NODE=1
export JOB_NAME="test_model"
export PARTITION="test"
export TIME_LIMIT_PARTITIONING="00:15:00"
export TIME_LIMIT_SIMULATION="00:15:00"
export DIMR_FOLDER="/p/d-hydro/dimrset/latest"
export DIMR_FILE="${PWD}/dimr_config.xml"

# This setting might help to prevent errors due to temporary locking of NetCDF files. 
export HDF5_USE_FILE_LOCKING=FALSE

  
# Compute the total number of tasks (across all nodes).
export NTASKS=$(( $NODES * $TASKS_PER_NODE ))

# Modify the value of the process tag in dimr_config.xml.
PROCESS_STR=$(seq -s " " 0 $(( $NTASKS - 1 )))
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESS_STR\2/" $DIMR_FILE

# The name of the MDU file is read from the DIMR configuration file.
export MDU_FILENAME=$(sed -n 's/\r//; s/<inputFile>\(.*[.]mdu\)<\/inputFile>/\1/p' $DIMR_FILE | sed -n 's/^\s*\(.*\)\s*$/\1/p')

# Do partitioning and simulation in the background. 
# The 'partition_and_run' function will continue in the background even after this script has finished.
# This function is run in a 'child process'. In addition, it uses variables defined in this shell script.
# Any variables used in `partition_and_run` must be exported, so they are available in the child process.
partition_and_run() {
    # Partition data using dflowfm.
    if [[ $NTASKS -gt 1 ]]; then
        pushd dflowfm  # Change working directory to `dflowfm`.
        echo "Partitioning parallel model..."
        echo "Run dflowfm on $MDU_FILENAME with $NTASKS partitions."
        sbatch --job-name=partition_${JOB_NAME} \
            --partition=1vcpu \
            --time=$TIME_LIMIT_PARTITIONING \
            --nodes=1 \
            --ntasks-per-node=1 \
            --wait \
            ${DIMR_FOLDER}/lnx64/bin/submit_dflowfm_h7.sh \
                --partition:ndomains=${NTASKS}:icgsolver=6 $MDU_FILENAME
        popd  # Change working directory back to the original directory.
    else
        echo "Sequential model..."
    fi

    # Run simulation using dimr.
    echo "Run simulation..."
    sbatch --job-name=$JOB_NAME \
        --partition=$PARTITION \
        --time=$TIME_LIMIT_SIMULATION \
        --nodes=$NODES \
        --ntasks-per-node=$TASKS_PER_NODE \
        ${DIMR_FOLDER}/lnx64/bin/submit_dimr_h7.sh -m $DIMR_FILE
}

# Run 'partition_and_run' in the background. 'nohup' only works on programs, not built-ins and functions. 
# We export the 'partition_and_run' function and run it in a child process to work around this limitation.
# Anything written to `stdout` and `stderr` will be written to the file `nohup.out`.
echo "Running simulation in the background..."
export -f partition_and_run 
nohup bash -c partition_and_run &
