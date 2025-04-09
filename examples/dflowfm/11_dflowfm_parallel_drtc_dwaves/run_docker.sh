#!/bin/bash

# Initialize variables
image="containers.deltares.nl/delft3d/delft3dfm:daily"  # Default value
mount_cmd="type=bind,source=$(pwd),target=/mnt/example"
work_dir="/mnt/example"
example_script="./run_example.sh"

# Parse command-line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --image) image="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Run the Docker command with the image parameter
docker run --shm-size 4G --rm --mount $mount_cmd --workdir $work_dir $image $example_script