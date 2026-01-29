#!/bin/bash

# List all subfolders in the current directory
for dir in */; do
    echo "Checking folder: $dir"
    cd "$dir"
    if [ -f run.sh ]; then
        echo "##teamcity[testStarted name='$dir' captureStandardOutput='true']"
        ./run_docker.sh
        echo "##teamcity[testFinished name='$dir']"
    else
        echo "No run script in $dir"
    fi
    cd ..
done
