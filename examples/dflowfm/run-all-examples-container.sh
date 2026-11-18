#!/bin/bash

# Initialize variables
default_image="containers.deltares.nl/delft3d/delft3dfm:daily"
image="$default_image"
container_runtime="docker"

# Parse command-line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --image) image="$2"; shift ;;
        --apptainer) container_runtime="apptainer" ;;
        --docker) container_runtime="docker" ;;
        --help) 
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --image <IMAGE>    Specify container image (default: containers.deltares.nl/delft3d/delft3dfm:daily)"
            echo "  --docker           Use Docker runtime (default)"
            echo "  --apptainer        Use Apptainer runtime"
            echo "  --help             Show this help message"
            exit 0
            ;;
        *) echo "Unknown parameter passed: $1"; echo "Use --help for usage information."; exit 1 ;;
    esac
    shift
done

# Prepare image reference based on container runtime
if [ "$container_runtime" = "apptainer" ]; then
    # If the image is from containers.deltares.nl, prepend 'docker://' in case of Apptainer
    if [[ "$image" == containers.deltares.nl* ]]; then
        container_image="docker://$image"
    else
        container_image="$image"
    fi
    echo "Using Apptainer with Docker image: $container_image"
    
    # Check if apptainer is available in PATH
    if ! command -v apptainer &> /dev/null; then
        echo "Error: 'apptainer' command not found. Please install Apptainer and ensure it is in your PATH."
        exit 1
    fi
else
    container_image="$image"
    echo "Using Docker with image: $container_image"
    
    # Check if docker is available in PATH
    if ! command -v docker &> /dev/null; then
        echo "Error: 'docker' command not found. Please install Docker and ensure it is in your PATH."
        exit 1
    fi
fi

echo "Container runtime: $container_runtime"
echo "Looking for run_${container_runtime}.sh scripts in subdirectories..."

# Loop through each subdirectory
script_name="run_${container_runtime}.sh"
executed_scripts=0
failed_examples=()
for dir in */ ; do
    test_name="${dir%/}_${container_runtime}"
    echo "##teamcity[testStarted name='$test_name']"

    # Check if the appropriate script exists in the subdirectory
    if [ -f "$dir/$script_name" ]; then
        echo "Found $script_name in $dir. Executing..."
        executed_scripts=$((executed_scripts + 1))

        # Run the script with the appropriate image format
        "${dir}${script_name}" --image "$container_image"

        # Check exit status and report test result
        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            echo "##teamcity[testFailed name='$test_name' message='Script exited with code $exit_code']"
            failed_examples+=("$dir")
        fi
    else
        echo "Error: No $script_name found in $dir. Every example must provide $script_name."
        echo "##teamcity[testFailed name='$test_name' message='Example not run: $script_name is missing']"
        failed_examples+=("$dir")
    fi
    echo "##teamcity[testFinished name='$test_name']"
done

if [ ${#failed_examples[@]} -ne 0 ]; then
    echo "Executed $executed_scripts scripts; ${#failed_examples[@]} example(s) failed or were not run:"
    printf '  %s\n' "${failed_examples[@]}"
    exit 1
fi

if [ $executed_scripts -eq 0 ]; then
    echo "No $script_name scripts found in any subdirectory of $(pwd)."
    exit 1
fi

echo "Executed $executed_scripts scripts successfully."
