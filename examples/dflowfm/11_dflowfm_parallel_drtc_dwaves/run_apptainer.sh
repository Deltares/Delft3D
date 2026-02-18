#!/usr/bin/env bash
#
# This file provides an example of how to run Delft3D FM in an Apptainer container.
#
# Note: This file can only be used in a Linux environment where Apptainer is installed.
#

# Defaults to daily build; can also be overridden (for automation) with: --image <container-name>
image=docker://containers.deltares.nl/delft3d/delft3dfm:daily

# Directory containing the entire model, that will be mounted inside the container.
# Default: the location of this script.
model_dir=$(dirname "$(realpath "$0")")
#model_dir=/home/username/project/model

# Relative to ${model_dir}, where ${command} will be executed inside the container.
work_dir=.
#work_dir=/computation

# The command to run INSIDE the container.
command=./run_example.sh
#command="run_dimr.sh"

# Check run parameters.
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --image) image="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

run_cmd="apptainer run --bind \"${model_dir}:/data\" --pwd \"/data/${work_dir}\" $image \"$command\""

echo "[INFO] $run_cmd"

eval "$run_cmd"
