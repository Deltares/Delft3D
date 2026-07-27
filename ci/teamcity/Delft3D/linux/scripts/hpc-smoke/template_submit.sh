#! /bin/bash

# Note: Apptainer is the replacement for Singularity.

# Usage: 
#   - Place this script in the same folder as the dimr config xml file
#   - Modify this script where needed (e.g. number of nodes, number of tasks per node, Apptainer version, model folder)
#   - Execute this script using:
#     sbatch ./{SUBMIT_SCRIPT_NAME}
#
# This is a {PLATFORM_NAME} specific script

#SBATCH --nodes={NUMBER_OF_NODES}                #-N, -n is total number of nodes. $SLURM_NTASKS = "--nodes" times "--ntasks-per-node"
#SBATCH --ntasks-per-node={TASKS_PER_NODE}     #You pay for a minimum of 1/4 of the cores on a node.
#SBATCH --job-name={JOB_NAME}         #-J
#SBATCH --time 00:29:00         #-t, reduce the expected time if possible to increase your priority.
#SBATCH --chdir=./              #chdir set as /path/to/runfolder is useful when calling this script from a different directory.
#SBATCH --partition={PARTITION_NAME}        #Type of partition. Choose the type appropriate for your job.
{SBATCH_MEMORY_RESERVATION}                        #(optional argument, depending on your HPC configuration) Memory per CPU core. Adjust based on your model requirements.
##SBATCH --exclusive            #To avoid any interference from other jobs running on the same node,
                                #or when a user wants to use all RAM available on the node. In many cases this option can be omitted.
                                #Extra costs might be associated with this option.
##SBATCH --contiguous           #To ensure that all nodes allocated for a job are allocated in a contiguous way, i.e. next to each other.
                                #Use of this option makes sense only for multi-node jobs. (See below for more information.)
                                #Extra costs might be associated with this option.

echo "---Loading modules..."
{MODULE_LOAD_SECTION}

# The root folder of the model, i.e. the folder that contains ALL of the input files and sub-folders:
modelFolder=${PWD} # You can use parent-directories relative to your current directory, e.g.: modelFolder=${PWD}/../../..

# The folder containing the dimr config file:
dimrconfigFolder=${PWD}

# The folder containing the mdu file (extracted from dimr config):
mdufileFolder={MDU_FILE_FOLDER}

# The name of the dimr config file. The default is dimr_config.xml:
dimrFile={DIMR_FILE}

# This setting might help to prevent errors due to temporary locking of NetCDF files. 
export HDF5_USE_FILE_LOCKING=FALSE

# Set the location of the Apptainer container.
apptainerFolder={APPTAINER_FOLDER}

# Check if process element is missing and add it
if ! grep -q "<process>" $dimrFile; then
    echo "Adding missing process and element to dimr config..."
    sed -i '/<library>dflowfm<\/library>/a\        <process>0</process>' $dimrFile
fi

# Check if mpiCommunicator element is missing and add it
if ! grep -q "<mpiCommunicator>" $dimrFile; then
    echo "Adding missing mpiCommunicator element to dimr config..."
    sed -i '/<process>.*<\/process>/a\        <mpiCommunicator>DFM_COMM_DFMWORLD</mpiCommunicator>' $dimrFile
fi

# Use SLURM_NTASKS to update the line "<process>" in the dimrFile.
PROCESSSTR="$(seq -s " " 0 $((SLURM_NTASKS-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile

# The name of the mdu file (extracted from dimr config):
mduFile={MDU_FILE}

# Set the execute script name.
execute_script={EXECUTE_SCRIPT}


#--- Partition by calling the dflowfm executable -------------------------------------------------------------
if [ "$SLURM_NTASKS" -gt 1 ]; then 
    echo ""
    echo "Partitioning parallel model..."
    cd "$mdufileFolder"
    echo "Partitioning in folder ${PWD}"
	{PARTITIONING_COMMAND}
else 
    #--- No partitioning ---
    echo ""
    echo "Sequential model..."
fi 
 
 
echo ""
echo "Simulation..."
cd $dimrconfigFolder
{SIMULATION_COMMAND}
