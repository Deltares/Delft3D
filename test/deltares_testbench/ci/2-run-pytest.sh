#!/bin/bash

# Check if two arguments are provided and the first argument is "--imageName"
if [ "$#" -ne 2 ] || [ "$1" != "--imageName" ]; then
    echo "Usage: $0 --imageName <image name>"
    exit 1
fi

IMAGE=$2
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker run -v"$SCRIPT_DIR"/..:/data -t "$IMAGE"
