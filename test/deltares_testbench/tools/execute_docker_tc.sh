#!/bin/bash

# Usage:
# Executing a command inside a Docker container.
# This script is an interface used by the testbench that is comparable to execute_singularity_tc.sh

#
#
# --- You shouldn't need to change the lines below ------------------------

function print_usage_info {
    echo "Usage: ${0##*/} executable [OPTIONS]"
    echo "       ${0##*/} [-h | --help]"
    echo "Runs executable inside Docker container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    exit 1
}

# Variables
executable=
executable_opts=
container_bindir=/opt/delft3dfm_latest/lnx64/bin # The directory WITHIN the container that contains all the executables

# Main
if [[ $# -eq 0 ]]; then
    print_usage_info
fi

# Parse the first argument of the script
while [[ $# -ge 1 ]]
do
    key="$1"
    shift
    case $key in
         -h|--help)
        print_usage_info
        ;;
        *)
        executable=$key    # The first unknown argument is the executable
        executable_opts=$* # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
        break
        ;;
    esac
done


# Set container properties
# Note that the working directory is set to a custom mounting directory
# for the container runtime environment. This mounting is to prevent 
# clashes with the internal opt directory of the container
current_working_dir=$(pwd)
mount_dir=/data


echo ---------------------------------------------------------------------- 
echo $scriptdirname
echo "Executing docker container with:"
echo "Current   working directory               : $current_working_dir"
echo "Mounting  target  directory               : $mountdir"
echo "Executable                                : $executable"
echo "Executable options                        : $executable_opts"
echo
echo "Executing docker run $container_bindir/$executable $executable_opts"

#
#
# --- Execution part: modify if needed ------------------------------------ 

docker run \
            --rm \
            --mount type=bind,source="$current_working_dir",target="$mount_dir" \
            -u $(id -u):$(id -g) \
            -e I_MPI_FABRICS=shm \
            --shm-size 8G \
            --ulimit stack=-1 \
            deltares/delft3dfm:latest \
            $container_bindir/$executable $executable_opts
