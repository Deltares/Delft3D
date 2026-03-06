#!/bin/bash

# Important: allow unlimited stack size. 
ulimit -s unlimited

# Note: Apptainer is the replacement for Singularity.
# This script assumes that the command "apptainer" can be found.

# Usage:
# Executing a command inside an Apptainer container. 
# This script is an interface between the Apptainer submit script
# and the Apptainer container itself.

# The following parameters are passed to the container via --env.
# Modify them according to your requirements:

# MPI_DIR: the path to the host's installation of IntelMPI.
MPI_DIR=$I_MPI_ROOT

# Control debug output.
export I_MPI_DEBUG=5

# Set the Hydra bootstrap server.
export I_MPI_HYDRA_BOOTSTRAP=slurm

# Make Intel MPI use slurm's PMI Library (optional).
export I_MPI_PMI_LIBRARY=/host/lib64/libpmi2.so

#
#
# --- You shouldn't need to change the lines below ------------------------

function print_usage_info {
    echo "Usage: ${0##*/} executable [OPTIONS]"
    echo "       ${0##*/} [-c | --containerfolder] folder executable [OPTIONS]"
    echo "       ${0##*/} [-m | --modelfolder] folder executable [OPTIONS]"
    echo "       ${0##*/} [-h | --help]"
    echo "Runs executable inside Apptainer container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-c, --containerfolder"
    echo "       The folder in which the apptainer container is located"
    echo "       (Default value: current working folder)"
    echo "-h, --help"
    echo "       Print this help message and exit"
    echo "-m, --modelfolder"
    echo "       The root folder of the model, i.e. the folder that contains ALL of the input files and sub-folders"
    echo "       (Default value: current working folder)"
    exit 1
}

# Variables. Will be overwritten.
container_folder=${PWD} # The directory that contains the apptainer container
model_folder=${PWD} # The directory that contains the model. This will be bound to the container.

executable=
executable_opts=

container_bindir=/opt/delft3dfm_latest/lnx64/bin # The directory WITHIN the container that contains the executables
container_libdir=/opt/delft3dfm_latest/lnx64/lib # The directory WITHIN the container that contains the libraries

container_PATH=$MPI_DIR/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:$container_bindir
container_LD_LIBRARY_PATH=$MPI_DIR/lib:$MPI_DIR/lib/release:$MPI_DIR/libfabric/lib:/usr/local/lib:/usr/lib:$container_libdir

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
        -c|--containerfolder)
        container_folder=$1
        shift
        ;;
        -h|--help)
        print_usage_info
        ;;
        -m|--modelfolder)
        model_folder=$1
        shift
        ;;
        -*|--*)
        echo "Unknown option: $key"
        echo
        print_usage_info
        ;;
        *)
        executable=$key    # The first unknown argument is the executable
        executable_opts=$* # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
        break
        ;;
    esac
done

# Scan container directory for sif containers
shopt -s nullglob
container_file_paths=($(ls $container_folder/*.sif))
container_file_path=

if [ ${#container_file_paths[@]} -eq 1 ]; then
    container_file_path=${container_file_paths[0]}
else
    echo "ERROR: Container directory must contain one and only one *.sif file."
    exit 2 # Exit code 2 to indicate that no such file is present
fi
shopt -u nullglob

# Set container properties
# Handle relative paths in the submit script.
currentFolder=${PWD}
cd $model_folder
model_folder=${PWD}
cd $currentFolder

current_working_dir=$(pwd)
mountdir=/mnt/data

# Note that the working directory is set to a custom mounting directory
# for the container runtime environment. This mounting is to prevent 
# clashes with the internal opt directory of the container
# Container working directory is "mount directory + (current working directory - model folder)"
container_working_dir=$mountdir${current_working_dir:${#model_folder}}

echo ---------------------------------------------------------------------- 
echo "Executing Apptainer container with:"
echo "Container file                            : $container_file_path"
echo "Current   working directory               : $current_working_dir"
echo "Mounting  source  directory               : $model_folder"
echo "Mounting  target  directory               : $mountdir"
echo "Container working directory               : $container_working_dir"
echo "Executable                                : $executable"
echo "Executable options                        : $executable_opts"
echo "env PATH                 inside container : $container_PATH"
echo "env LD_LIBRARY_PATH      inside container : $container_LD_LIBRARY_PATH"
echo "env HDF5_USE_FILE_LOCKING inside container: $HDF5_USE_FILE_LOCKING"
echo
echo "Executing apptainer exec $container_bindir/$executable $executable_opts"

#
#
# --- Execution part: modify if needed ------------------------------------ 

# Optionally use --cleanenv to prevent the user's environment from being passed to the container.
# Be careful with it: the IntelMPI setup uses multiple environment settings.
# --cleanenv will probably not work for multiple node computations.
# See also https://apptainer.org/docs/user/latest/environment_and_metadata.html
#

apptainer exec \
                 --bind $model_folder:$mountdir,$MPI_DIR:$MPI_DIR,/usr/:/host,/usr/lib64/:/host/lib64 \
                 --pwd $container_working_dir \
                 --no-home \
                 --env HDF5_USE_FILE_LOCKING=$HDF5_USE_FILE_LOCKING \
                 --env PATH=$container_PATH \
                 --env LD_LIBRARY_PATH=$container_LD_LIBRARY_PATH \
                 $container_file_path $container_bindir/$executable $executable_opts
