#!/usr/bin/env bash
#
# This file provides an example of how to run Delft3D FM in a Docker container.
#
# Note: This file can only be used in a Linux or WSL2 (Windows Subsystem for Linux) environment.
#

# Defaults to daily build; can also be overridden (for automation) with: --image <container-name>
image=containers.deltares.nl/delft3d/delft3dfm:daily

# Additional options, like increased shared memory for parallel runs.
#docker_options="--shm-size 4G"

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

docker run \
    --user $(id -u) \
    --rm \
    ${docker_options} \
    --mount "type=bind,source=${model_dir},target=/data" \
    --workdir "/data/${work_dir}" \
    $image \
    $command