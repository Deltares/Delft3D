#! /bin/bash

# Note: Apptainer is the replacement for Singularity.

# Usage: 
#   - Place this script in the same folder as the dimr config xml file
#   - Modify this script where needed (e.g. number of nodes, number of tasks per node, Apptainer version, model folder)
#   - Execute this script using:
#     sbatch ./submit_singularity_snellius.sh
#
# This is a SNELLIUS specific script

#SBATCH --nodes=1               #-N, -n is total numer of nodes. $SLURM_NTASKS = "--nodes" times "--ntasks-per-node"
#SBATCH --ntasks-per-node=1     #You pay for a minimum of 1/4 of the cores on a node.
#SBATCH --job-name=tst          #-J
#SBATCH --time 01:00:00         #-t, reduce the expected time if possible to increase your priority.
#SBATCH --chdir=./              #chdir set as /path/to/runfolder is useful when calling this script from a different directory.
#SBATCH --partition=rome        #Type of partition. Choose the type appropriate for your job.
                                #See https://servicedesk.surf.nl/wiki/display/WIKI/Snellius+partitions+and+accounting
##SBATCH --exclusive            #To avoid any interference from other jobs running on the same node,
                                #or when a user wants to use all RAM available on the node. In many cases this option can be omitted.
                                #Extra costs might be associated with this option.
##SBATCH --contiguous           #To ensure that all nodes allocated for a job are allocated in a contiguous way, i.e. next to each other.
                                #Use of this option makes sense only for multi-node jobs. (See below for more information.)
                                #Extra costs might be associated with this option.

# Note on the 'contiguous' directive:
# Contiguous allocation is intended to improve MPI message routing and therefore improve the communication performance.
# If nodes are allocated contiguously, less routers are involved and, often, messages can be communicated using a local node-to-node network connection.
# Otherwise, messages will have to travel across the whole network tree to reach the destination.
# In practice, it's often better to omit it, because allocation of contiguous nodes may take more time than allocation of random nodes.



echo "---Load modules..."
module purge
module load 2023
module load intel/2023a


#---You will need to modify the input below this line---

# The root folder of the model, i.e. the folder that contains ALL of the input files and sub-folders:
modelFolder=${PWD} # You can use parent-directories relative to your current directory, e.g.: modelFolder=${PWD}/../../..

# Or, for large models that generate a lot of output, copying the model to your scratch file space '/scratch-shared/<username>' and running from there might be faster.
# See: https://servicedesk.surf.nl/wiki/display/WIKI/Snellius+filesystems#Snelliusfilesystems-Thescratchfilesystems
# Don't forget to copy your results back to a permanent location on Snellius since data on the scratch space is removed automatically!

# The folder containing the dimr config file:
dimrconfigFolder=${PWD}

# The folder containing the mdu file:
mdufileFolder=${PWD}

# The name of the dimr config file. The default is dimr_config.xml:
dimrFile=dimr_config.xml

# This setting might help to prevent errors due to temporary locking of NetCDF files. 
export HDF5_USE_FILE_LOCKING=FALSE


#---You do not need to modify anything below this line---

# Set the location of the Apptainer container.
singularityFolder=/path/to/your/apptainer/sif

# Use SLURM_NTASKS to update the line "<process>" in the dimrFile.
PROCESSSTR="$(seq -s " " 0 $((SLURM_NTASKS-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile
# Retrieve the name of the mduFile from dimrFile.
mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

echo ""
echo "Partitioning..."
cd $mdufileFolder
$singularityFolder/execute_singularity_snellius.sh $modelFolder run_dflowfm.sh --partition:ndomains=$SLURM_NTASKS:icgsolver=6 $mduFile

echo ""
echo "Simulation..."
cd $dimrconfigFolder
$singularityFolder/execute_singularity_snellius.sh $modelFolder run_dimr.sh -m $dimrFile
