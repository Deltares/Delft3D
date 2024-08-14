#!/bin/bash

# Check if two arguments are provided and the first argument is "--imageName"
if [ "$#" -ne 2 ] || [ "$1" != "--imageName" ]; then
    echo "Usage: $0 --imageName <image name>"
    exit 1
fi

IMAGE=$2

# Run pytest
docker run --rm -v="./logs:/data/logs" -t "$IMAGE"

# Run auto-formatting check.
docker run --rm "$IMAGE" ruff format -q .

# Run auto linter with basic pyflakes rules, warnings and import sort order
docker run --rm -v="./logs:/data/logs" -t "$IMAGE" ruff check -q \
    --output-format=junit \
    --output-file=logs/ruff_check_results.xml \
    --select F4,F5,F6,F7,W,I

