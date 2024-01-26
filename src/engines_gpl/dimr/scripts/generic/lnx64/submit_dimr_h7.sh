#! /bin/bash  
# Specify Slurm SBATCH directives 
#SBATCH --nodes=1              # Number of nodes.
#SBATCH --ntasks-per-node=1     # The number of tasks to be invoked on each node.
                                # For sequential runs, the number of tasks should be '1'.
                                # Note: SLURM_NTASKS is equal to "--nodes" multiplied by "--ntasks-per-node".
#SBATCH --job-name=test_model   # Specify a name for the job allocation.
#SBATCH --time=00:15:00         # Set a limit on the total run time of the job allocation.
#SBATCH --partition=test        # Request a specific partition for the resource allocation.
                                # See: https://publicwiki.deltares.nl/display/Deltareken/Compute+nodes.
##SBATCH --exclusive            # The job allocation can not share nodes with other running jobs.
                                # In many cases this option can be omitted.
##SBATCH --contiguous           # The allocated nodes must form a contiguous set, i.e. next to each other.
                                # In many cases this option can be omitted.

set -eo pipefail

function print_usage_info {
    echo "Usage: sbatch [SLURM OPTIONS]... ${0##*/} [OPTION]..."
    echo "Run a dimr model on H7."
    echo
    echo "Options:"
    echo "-d, --debug <D>"
    echo "       0:ALL, 6:SILENT; ALL includes overall time output"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-m, --masterfile <filename>"
    echo "       dimr configuration filename, default dimr_config.xml"
}

if [[ ! -n $SLURM_JOB_ID ]]; then
    echo "This script must be submitted using 'sbatch'."
    print_usage_info
    exit 1
fi

# Load the intelmpi module.
module load intelmpi/2021.10.0

# Set MPI options. 
# Reference on intel MPI environment variables: 
# https://www.intel.com/content/www/us/en/docs/mpi-library/developer-reference-linux/2021-8/environment-variable-reference.html
# https://www.intel.com/content/www/us/en/developer/articles/technical/mpi-library-2019-over-libfabric.html 
export I_MPI_DEBUG=5
export I_MPI_FABRICS=ofi
export I_MPI_OFI_PROVIDER=tcp
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi2.so

# Get the path to the script submitted using `sbatch`. `sbatch` copies the script to
# a temporary directory before executing it, so we need `scontrol` to look up the original command.
SCRIPT_PATH=$(scontrol show job $SLURM_JOB_ID | awk '/Command=/{split($1,a,"="); print(a[2])}')
D3D_HOME="$(readlink -f $(dirname $SCRIPT_PATH)/..)"
BIN_DIR=${D3D_HOME}/bin
LIB_DIR=${D3D_HOME}/lib

# Parse command line arguments.
DEBUG_LEVEL=-1
CONFIG_FILE=dimr_config.xml
while [[ $# -ge 1 ]]; do
    key="$1"
    shift
    case $key in
        -d|--debug)
        DEBUG_LEVEL="$1"
        shift
        ;;
        -m|--masterfile)
        CONFIG_FILE="$1"
        shift
        ;;
        -h|--help)
        print_usage_info
        ;;
        *)
        echo "ERROR: Failed to parse command line arguments."
        print_usage_info
        exit 1
        ;;
    esac
done

# Check configfile
if [[ ! -f $CONFIG_FILE ]]; then
    echo "ERROR: configfile $CONFIG_FILE does not exist." 2>&1 
    print_usage_info
    exit 1
fi

# Check debuglevel, translate into argument for dimr
DEBUG_ARG=
if [[ $DEBUG_LEVEL != -1 ]]; then
    DEBUG_ARG="-d $DEBUG_LEVEL"
fi
  
# Configure environment variables and 'stacksize' limit.
ulimit -s unlimited
export PATH=$BIN_DIR:$PATH
export LD_LIBRARY_PATH=$LIB_DIR:$LD_LIBRARY_PATH
export PROC_DEF_DIR=${D3D_HOME}/share/delft3d

if [[ $DEBUG_LEVEL = 0 ]]; then
    echo === LD_LIBRARY_PATH =========================================
    echo $LD_LIBRARY_PATH
    echo =========================================================
    echo " "
    echo === ldd $LIB_DIR/libdflowfm.so =========================================
    ldd $LIB_DIR/libdflowfm.so
    echo =========================================================
    echo " "
    echo ===  $BIN_DIR/dflowfm -v =========================================
    $BIN_DIR/dflowfm -v
    echo =========================================================
    echo " "
    echo ===  ldd $BIN_DIR/dimr =========================================
    ldd $BIN_DIR/dimr
    echo ========================================================
    echo " "
    echo ===  ldd $LIB_DIR/libdimr.so =======================================
    ldd $LIB_DIR/libdimr.so
    echo =========================================================
fi

srun ${BIN_DIR}/dimr $CONFIG_FILE $DEBUG_ARG
