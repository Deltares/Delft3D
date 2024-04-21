#!/bin/bash

# Usage:
# This script is called by the script "submit_singularity_snellius.sh".
# This script executes a command inside a Singularity container.

# This script "execute_singularity_snellius.sh" must be located in the same folder
# as the Singularity sif-file to be used.

# This is a SNELLIUS specific script.

# This script assumes that the command "singularity" can be found.

# The following parameters are passed to the container via --env.


# =============================================================================
function execute_singularity_edited {
    executable=
    executable_opts=
    container_bindir=/opt/delft3dfm_latest/lnx64/bin # The directory WITHIN the container that contains all the executables
    container_libdir=/opt/delft3dfm_latest/lnx64/lib # The directory WITHIN the container that contains all the executables
    # MPI_DIR: the path to your own installation of IntelMPI
    export MPI_DIR=${I_MPI_ROOT}
    export container_PATH=$MPI_DIR/bin:$PWD:$container_bindir:$PATH:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin 
    export container_LD_LIBRARY_PATH=$MPI_DIR/lib:$MPI_DIR/lib/release:$container_libdir:$LD_LIBRARY_PATH

    if [ $debugLevel -ge 1 ]; then
        I_MPI_DEBUG_LEVEL=5
    else
        I_MPI_DEBUG_LEVEL=0
    fi

    if [[ "$@" == *"partition:ndomains"* ]]; then
        echo "Partitioning: 'Number of tasks' is forced to be 1"
        NSLOTS=1
    else
        NSLOTS=$SLURM_NTASKS
    fi


    # Parse the arguments of execute_singularity_edited
    while [[ $# -ge 1 ]]
    do
        key="$1"
        shift
        case $key in
            *)
            executable=$key    # The first unknown argument is the executable
            executable_opts=$* # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
            break
            ;;
        esac
    done

    # Retrieve the script directory
	# Get the full path for the file that is read into $0.  
    scriptdirname=`readlink \-f \$0`
	
	# Get directory path from the filepath (i.e. path including filename)
    scriptdir=`dirname $scriptdirname`

    # Scan script directory for sif containers
    shopt -s nullglob
    container_file_paths=($(ls $scriptdir/*.sif))
    containerName=

    if [ ${#container_file_paths[@]} -eq 1 ]; then
        containerName=${container_file_paths[0]}
    else
        echo "ERROR: Directory must contain one and only one *.sif file."
        echo "       scriptdir: $scriptdir"
        workdir=`pwd`
        echo "       workdir  : $workdir"
        exit 2 # Exit code 2 to indicate that no such file is present
    fi
    shopt -u nullglob


    export containerMountFolder=/mnt/data
    export containerWorkingFolder=$containerMountFolder${currentFolder:${#modelFolder}}
    full_cmd_call="$container_bindir/$executable $executable_opts"

    echo "Executing Singularity container with:"
    echo "    containerName                             : $containerName"
    echo "    Current   working folder                  : $currentFolder"
    echo "    Mounting  source  folder                  : $modelFolder"
    echo "    Mounting  target  folder                  : $containerMountFolder"
    echo "    Container working folder                  : $containerWorkingFolder"
    echo "    Executable                                : $executable"
    echo "    Executable options                        : $executable_opts"
    echo "    env PATH                 inside container : $container_PATH"
    echo "    env LD_LIBRARY_PATH      inside container : $container_LD_LIBRARY_PATH"
    echo "    env HDF5_USE_FILE_LOCKING inside container : $HDF5_USE_FILE_LOCKING"

    #
    #
    # --- Execution part ------------------------------------ 


    if [ $debugLevel -ge 2 ]; then
        echo "host fi_info:"
        fi_info
        echo "container fi_info:"
        singularity exec \
            --bind $modelFolder:$containerMountFolder,/usr:/host,/sw:/sw \
            --pwd $containerWorkingFolder \
            --env PATH=$container_PATH \
            --env LD_LIBRARY_PATH=$container_LD_LIBRARY_PATH \
            $containerName \
            fi_info
    fi

    mpirun -np $NSLOTS \
           -ppn $SLURM_TASKS_PER_NODE \
           -f hosts_uniq.txt \
           -genv I_MPI_DEBUG=$I_MPI_DEBUG_LEVEL \
           singularity exec \
               --bind $modelFolder:$containerMountFolder,/usr:/host,/usr/lib64:/host/lib64 \
               --pwd $containerWorkingFolder \
               --env HDF5_USE_FILE_LOCKING=$HDF5_USE_FILE_LOCKING \
               --env PATH=$container_PATH \
               --env LD_LIBRARY_PATH=$container_LD_LIBRARY_PATH \
               --env I_MPI_PMI_LIBRARY=/host/lib64/libpmi2.so \
               --env I_MPI_PMI2=yes \
               $containerName \
               $full_cmd_call
}



# =============================================================================
function run_dimr_orig {
    debuglevel=-1
    configfile=dimr_config.xml
    runscript_extraopts=
    ulimit -s unlimited

    #
    ## Start processing command line options:
    while [[ $# -ge 1 ]]
    do
    key="$1"
    shift

    case $key in
        -c|--corespernode)
        corespernode=$1
        shift
        ;;
        -d|--debug)
        debuglevel="$1"
        shift
        ;;
        -m|--masterfile)
        configfile="$1"
        shift
        ;;
        --)
        echo "-- sign detected, remained options are going to be passed to dimr"
        runscript_extraopts="$runscript_extraopts $*"
        break       # exit loop, stop shifting, all remaining arguments without dashes handled below
        ;;
        -*)
        echo "option ${key} seems dedicated for dimr, therefore passing it and the following ones to the dimr"
        runscript_extraopts="$key $*"
        break       # exit loop, $key+all remaining options to dflowfm executable
        ;;
    esac
    done

    # Check configfile    
    if [ ! -f $configfile ]; then
        echo "ERROR: configfile $configfile does not exist"
        echo "call print_usage_info"
    fi

    # Check debuglevel, translate into argument for dimr
    if [ $debuglevel -eq -1 ]; then
        debugarg=
    else
        debugarg="-d $debuglevel"
    fi


    if [ $debugLevel -ge 3 ]; then
        echo "    Configfile       : $configfile"
        echo "    debugarg         : $debugarg"

        echo "run_dimr_orig executing:"
        echo "execute_singularity_edited dimr $configfile $debugarg"
    fi
    execute_singularity_edited dimr $configfile $debugarg
}



# =============================================================================
# ==============================
# === MAIN
# ==============================
echo ---------------------------------------------------------------------- 
set -e

if [ -z $debugLevel ]; then
    export debugLevel=0
fi

currentFolder=${PWD}
modelFolder=$1
shift
cd $modelFolder
modelFolder=${PWD}
cd $currentFolder

echo
echo "---Creating hostfile..."
echo
srun hostname > hosts.txt
sort hosts.txt | uniq > hosts_uniq.txt
sed -i 's/.local.snellius.surf.nl//g' hosts_uniq.txt
rm hosts.txt

echo
echo "---Detected hosts:"
cat hosts_uniq.txt
echo

echo
echo "---- Setting up MPI environment..."
source $I_MPI_ROOT/env/vars.sh -ofi-internal=1
export I_MPI_PIN_MODE=pm
export I_MPI_FABRICS=shm:ofi
export I_MPI_HYDRA_BOOTSTRAP=slurm
export I_MPI_PIN_RESPECT_CPUSET=0

echo ""
echo "    script                    : " `readlink \-f \$0`
echo "    Number of nodes           : $SLURM_NNODES"
echo "    Number of tasks per node  : $SLURM_NTASKS_PER_NODE"
echo "    Number of tasks           : $SLURM_NTASKS"
echo "    currentFolder             : $currentFolder"
echo "    modelFolder               : $modelFolder"
echo "    dimrconfigFolder          : $dimrconfigFolder"
echo "    mdufileFolder             : $mdufileFolder"
echo "    singularityFolder         : $singularityFolder"
echo "    debugLevel                : $debugLevel"
echo "    I_MPI_ROOT                : $I_MPI_ROOT"
echo "    I_MPI_PIN_MODE            : $I_MPI_PIN_MODE"
echo "    I_MPI_FABRICS             : $I_MPI_FABRICS"
echo "    I_MPI_HYDRA_BOOTSTRAP     : $I_MPI_HYDRA_BOOTSTRAP"
echo "    I_MPI_PIN_RESPECT_CPUSET  : $I_MPI_PIN_RESPECT_CPUSET"
if [ $debugLevel -ge 2 ]; then
    echo "    I_MPI_TUNING_MODE                       : $I_MPI_TUNING_MODE"
    echo "    I_MPI_TUNING_AUTO_WARMUP_ITER_NUM       : $I_MPI_TUNING_AUTO_WARMUP_ITER_NUM"
    echo "    I_MPI_TUNING_AUTO_ITER_NUM              : $I_MPI_TUNING_AUTO_ITER_NUM"
    echo "    I_MPI_TUNING_AUTO_SYNC                  : $I_MPI_TUNING_AUTO_SYNC"
    echo "    I_MPI_TUNING_AUTO_ITER_POLICY_THRESHOLD : $I_MPI_TUNING_AUTO_ITER_POLICY_THRESHOLD"
    echo "    I_MPI_TUNING_AUTO_STORAGE_SIZE          : $I_MPI_TUNING_AUTO_STORAGE_SIZE"
    echo "    module list               :"
    module list
fi


if [[ "$@" == *"run_dflowfm"* ]]; then
    # run_dflowfm:
    # Directly call "execute_singularity_edited"
    execute_singularity_edited "$@"
fi
if [ $1 == "run_dimr.sh" ]; then
    # run_dimr:
    # - Use "shift" to remove the first argument
    # - call "execute_singularity_edited" via "run_dimr_orig" to parse the arguments.
    #   "run_dimr_orig" will pass "dimr dimr_config.xml" to "execute_singularity_edited" instead of "run_dimr.sh -m dimr_config.xml"
    shift
    run_dimr_orig "$@"
fi

echo ""
echo ""
echo ""
