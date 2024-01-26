#!/bin/sh

function print_usage_info {
    echo "Usage: ${0##*/} dimr_config_file [run_dimr.sh OPTIONS]..."
    echo "       ${0##*/} [-h | --help]"
    echo "Runs the run_dimr.sh command of a Docker container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    exit 1
}

# Variables
dimr_config_file=
executable_extraopts=

# Main
if [[ $# -eq 0 ]]; then
    print_usage_info
fi

# Parse the first argument of the script
if [[ $# -ge 1 ]]; then
    arg=$1
    case $arg in
        -h|--help)
        print_usage_info
        ;;
        *)
        dimr_config_file=$arg
        ;;
    esac
    shift

    # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
    executable_extraopts="$*"
fi

# Check if the dimr_config_file exists
if [ ! -z "$dimr_config_file" ] && [ ! -f "$dimr_config_file" ]; then
    echo "ERROR: Dimr config file must be set and existent."
    exit 2 # Exit code 2 to indicate that no such file is present
fi

# Write run_docker.sh file
run_docker_file_name=run_docker.sh
echo "Writing $run_docker_file_name..."
cat > $run_docker_file_name <<EOL
#!/bin/bash

ulimit -s unlimited
export OMP_NUM_THREADS=1
export I_MPI_FABRICS=shm

/opt/delft3dfm_latest/lnx64/bin/run_dimr.sh -m $dimr_config_file $executable_extraopts
EOL

# Check if the run_docker.sh exists exists
if [ ! -f "$run_docker_file_name" ]; then
    echo "ERROR: Could not write the $run_docker_file_name."
    exit 2 # Exit code 2 to indicate that no such file is present
fi

# Add permissions for the docker file to be executed
chmod a+x,a-s $run_docker_file_name

# Execute the docker container
working_dir=`pwd`
mount_dir=/data
echo "Executing latest delft3dfm Docker container with:"
echo "Dimr file                         :   $dimr_config_file"
echo "Working directory                 :   $working_dir"
echo "Mounting directory                :   $mount_dir"
echo "Extra executable flags            :   $executable_extraopts"
docker run --rm -v $working_dir:$mount_dir -u $(id -u):$(id -g) --ulimit stack=-1 -t deltares/delft3dfm:latest
