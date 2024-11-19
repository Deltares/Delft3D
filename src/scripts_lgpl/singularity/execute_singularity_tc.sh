#!/bin/bash

# Usage:
# Executing a command inside a Singularity container.
# This script is an interface between the scripts "run_singularity.sh" and 
# "submit_singularity" in the working folder and the Singularity container itself.

# This script "execute_singularity_tc.sh" must be located in the same folder
# as the Singularity sif-file.


# This script assumes that the command "singularity" can be found.
# For example, at Deltares:
# module load singularity



# The following parameters are passed to the container via --env.
# Modify them according to your requirements:

# MPI_DIR: the path to your own installation of IntelMPI
MPI_DIR=/opt/apps/intelmpi/2021.11.0/mpi/2021.11
container_PATH=$MPI_DIR/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin 
container_LD_LIBRARY_PATH=$MPI_DIR/lib:$MPI_DIR/lib/release


# Warning! For the SYSADMIN, it might be necessary to modify MPI parameters.
# Such parameters are very system specific and potentially fragile.
# For example, the following settings are required for the TeamCity build server at Deltares:
export I_MPI_FABRICS=shm
export FI_PROVIDER=tcp

#
#
# --- You shouldn't need to change the lines below ------------------------

function print_usage_info {
    echo "Usage: ${0##*/} executable [OPTIONS]"
    echo "       ${0##*/} [-p | --parentlevel] 3 executable [OPTIONS]"
    echo "       ${0##*/} [-h | --help]"
    echo "Runs executable inside Singularity container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    echo "-p, --parentlevel"
    echo "       A numeric value that specifies the amount of levels to navigate to the parent directory to mount"
    echo "       (Default value: 1)"
    exit 1
}

# Variables
parent_level=1
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
        -p|--parentlevel)
        parent_level=$1
        shift
        ;;
        *)
        executable=$key    # The first unknown argument is the executable
        executable_opts=$* # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
        break
        ;;
    esac
done

# Retrieve the script directory
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`

# Scan script directory for sif containers
shopt -s nullglob
container_file_paths=($(ls $scriptdir/*.sif))
container_file_path=

if [ ${#container_file_paths[@]} -eq 1 ]; then
    container_file_path=${container_file_paths[0]}
else
    echo "ERROR: Directory must contain one and only one *.sif file."
    echo "       scriptdir: $scriptdir"
    workdir=`pwd`
    echo "       workdir  : $workdir"
    exit 2 # Exit code 2 to indicate that no such file is present
fi
shopt -u nullglob


# Set container properties
# Note that the working directory is set to a custom mounting directory
# for the container runtime environment. This mounting is to prevent 
# clashes with the internal opt directory of the container
current_working_dir=$(pwd)
mountdir=/mnt/data

working_dir=
container_working_dir=

if [[ $parent_level -ge 1 ]]; then 
    let target_workingdir_level=$parent_level+1
    working_dir=$(echo $current_working_dir | rev | cut -d'/' -f$target_workingdir_level- | rev)          # Returns the desired mounting parent directory 
    container_working_dir=$mountdir/$(echo $current_working_dir | rev | cut -d'/' -f-$parent_level | rev) # Extract the directories that are traversed
elif [[ $parent_level -eq 0 ]]; then
    # Parent directory is equal to the current directory and as such there is no need to traverse the directory structure
    # while mounting
    working_dir=$current_working_dir
    container_working_dir=$mountdir
else 
    echo "Invalid parent level setting: value must be greater than or equal to 0"
    exit 1
fi

echo ---------------------------------------------------------------------- 
echo $scriptdirname
echo "Executing Singularity container with:"
echo "Container file                            : $container_file_path"
echo "Current   working directory               : $current_working_dir"
echo "Mounting  source  directory               : $working_dir"
echo "Mounting  target  directory               : $mountdir"
echo "Container working directory               : $container_working_dir"
echo "Executable                                : $executable"
echo "Executable options                        : $executable_opts"
echo "env PATH                 inside container : $container_PATH"
echo "env LD_LIBRARY_PATH      inside container : $container_LD_LIBRARY_PATH"
echo "env FI_PROVIDER                           : $FI_PROVIDER"
echo "env I_MPI_FABRICS                         : $I_MPI_FABRICS"
echo
echo "Executing singularity exec $container_bindir/$executable $executable_opts"

#
#
# --- Execution part: modify if needed ------------------------------------ 

# Optionally use --cleanenv to prevent the user's environment from being passed to the container.
# Be careful with it: the IntelMPI setup uses multiple environment settings.
# --cleanenv will probably not work for multiple node computations.
# See also https://sylabs.io/guides/3.8/user-guide/environment_and_metadata.html
#
# Warning! For the SYSADMIN, it might be necessary to extend the bindings.
# When using D-Waves/SWAN inside the container and SWAN aborts with a message like:
#     OMP: Error #179: Function Can't open SHM2 failed:
#     OMP: System error #20: Not a directory
#     forrtl: error (76): Abort trap signal
# Then try to add the binding /run/shm:/run/shm below. Example line:
#     --bind /run/shm:/run/shm,$working_dir:$mountdir,$MPI_DIR:$MPI_DIR,/usr/:/host \


singularity exec \
                 --bind $working_dir:$mountdir,$MPI_DIR:$MPI_DIR,/usr/:/host \
                 --pwd $container_working_dir \
                 --env PATH=$container_PATH \
                 --env LD_LIBRARY_PATH=$container_LD_LIBRARY_PATH \
                 $container_file_path $container_bindir/$executable $executable_opts
