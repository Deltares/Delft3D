#!/bin/bash

# Initialize variables
image="containers.deltares.nl/delft3d/delft3dfm:daily"  # Default value

# Parse command-line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --image) image="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Loop through each subdirectory
for dir in */ ; do
    # Check if run_docker.sh exists in the subdirectory
    if [ -f "$dir/run_docker.sh" ]; then
        echo "Found run_docker.sh in $dir. Executing..."
        # Run the script
        (cd "$dir" && ./run_docker.sh --image "$image")
    else
        echo "No run_docker.sh found in $dir."
    fi
done