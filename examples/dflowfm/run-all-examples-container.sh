#!/bin/bash

# Initialize variables
image="containers.deltares.nl/delft3d/delft3dfm:daily"  # Default value
container_runtime="docker"  # Default to docker

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
    container_image="docker://$image"
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
found_scripts=0
for dir in */ ; do
    script_name="run_${container_runtime}.sh"
    
    # Check if the appropriate script exists in the subdirectory
    if [ -f "$dir/$script_name" ]; then
        echo "Found $script_name in $dir. Executing..."
        found_scripts=$((found_scripts + 1))
        
        # Run the script with the appropriate image format
        "${dir}${script_name}" --image "$container_image"
        
        # Check exit status
        if [ $? -ne 0 ]; then
            echo "Warning: $script_name in $dir exited with non-zero status"
        fi
    else
        echo "No $script_name found in $dir."
    fi
done

if [ $found_scripts -eq 0 ]; then
    echo "No run_${container_runtime}.sh scripts found in any subdirectory."
    echo "Make sure you have the appropriate scripts for the selected runtime ($container_runtime)."
    exit 1
else
    echo "Executed $found_scripts scripts successfully."
fi
