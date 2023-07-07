#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

# Usage:
# To submit and execute a job on an SGE cluster, for
# D-Flow FM computations using a Singularity container.
# The script "run_singularity.sh" is much simpler, but can only be used for
# either sequential, or parallel computations using one node.
# For parallel using multiple nodes: use submit_singularity.sh or write your own variant.
#
# To start:
# 1. Be sure that a Singularity container is available, 
#    together with an execute_singularity.sh script in the same folder.
# 2. Copy the submit_singularity.sh script into your working folder, i.e. the folder containing the dimr config file.
# 3. Modify the submit_singularity.sh script, see remarks below.
# 4. Execute the script from the command line.
#    This script assumes a SGE queueing system is present and will execute qsub.
#    Example:
#    > ./submit_singularity.sh
#
#
# Submit a job in the queue:
#     Execute this script from the command line without arguments.
#     The "STEP 1" part below will be entered, finishing with placing this script,
#     "submit_singularity.sh", with a set of arguments, in the queue.
#
# Execute a job from the queue:
#     When the queueing system has allocated resources, this script,
#     "submit_singularity.sh", with a set of arguments, will be executed.
#     The "STEP 2" part below will be entered, finishing with:
#     - executing script "execute_singularity.sh"
#       In case of a single node run. The mpirun/mpiexec inside will refer to
#       the external IntelMPI installation.
#     - executing "mpirun ... execute_singularity.sh"
#       In case of a multi node run. It's essential that "mpirun" is outside Singularity.


# "execute_singularity.sh -p 2": Parent level to mount:
# If your working folder does not contain all of the input files, then you must set the -p flag.
# Let's define the "top level" as the folder containing all of the input files.
# The value of -p must be the number of folder levels between the dimr config file and the top level.
# A higher value will not cause any harm, provided that folders exist at the higher levels.
# 


#
#
# --- You will need to change the lines below -----------------------------

# Set number of nodes and partitions
nNodes=2
nProc=3

# Set the path to the folder containing the singularity image and the execute_singularity.sh script. For example: 
singularitydir=/p/d-hydro/delft3dfm_containers/delft3dfm_2022.03

# select queue; one of : normal-e3-c7, normal-e3-c7-v2, normal-e5-c7, test-c7
queue=test-c7

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml


#
#
# --- You shouldn't need to change the lines below ------------------------

# stop after an error occurred:
set -e

nPart=$((nNodes * nProc))

if (( $# < 4 )); then
    #
    # This script is called with 3 arguments or less:
    # STEP 1:
    # Prepare computation and execute "qsub", executing this script, "submit_singularity.sh" (with >=4 arguments), when the nodes are allocated

    # Replace number of processes in DIMR file
    PROCESSSTR="$(seq -s " " 0 $((nPart-1)))"
    sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile

    # Read MDU file from DIMR file
    mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

    # jobName: dimrsif_$FOLDERNAME_nodesxprocs
    export jobName="dimrsif_${PWD##*/}_${nNodes}x${nProc}"


    if [ "$nPart" == "1" ]; then
        # Sequential computation
        # "-p": See above. Arguments after "run_dimr.sh" are explained in run_dimr.sh
        echo Adding a job to the queue for one node, one partition, based on this script "submit_singularity.sh" ...
        qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh -p 2 run_dimr.sh -m $dimrFile
    else
        # Parallel computation on 1 or more nodes
        #

        # First: partitioning 
        # (You can re-use a partition if the input files and the number of partitions haven't changed)
        # Partitioning is executed by dflowfm, in the folder containing the mdu file
        # Partitioning is executed immediately on the current system, not in the queue
        cd path/to/directory/containing/the/mdu/file
        echo partitioning...
        # "-p": See above. Arguments after "run_dflowfm.sh" are explained in run_dflowfm.sh
        $singularitydir/execute_singularity.sh -p 2 run_dflowfm.sh --partition:ndomains=$nPart:icgsolver=6 $mduFile

        # Jump back to the dimr config file folder to execute dimr
        cd path/to/directory/containing/the/dimr_config/file
        # Second: computation
        echo computation...
        if [ "$nNodes" == "1" ]; then
            # One Node
            # "-p": See above. Arguments after "run_dimr.sh" are explained in run_dimr.sh
            echo Adding a job to the queue for one node, multi partitions, based on this script "submit_singularity.sh" ...
            qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh -p 2 run_dimr.sh -m $dimrFile
        else
            # Multi Node
            # "-p": See above. Arguments after "run_dimr.sh" are explained in run_dimr.sh
            echo Adding a job to the queue for multi nodes, based on this script "submit_singularity.sh" ...
            qsub -q $queue -pe distrib $nNodes -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh -p 2 run_dimr.sh -m $dimrFile
        fi
    fi

else
    #
    # This script is called with 4 arguments or more:
    # STEP 2:
    # The queueing system allocated nodes, execute "execute_singularity.sh",
    # possibly with "mpirun" outside (multi node)

    # This script assumes that the command "singularity" can be found.
    # For example, at Deltares:
    module load intelmpi/21.2.0
    
    pwd=`pwd`
    nNodes=${1}
    nProc=${2}
    exec_sif=${3}
    minp=${4}
    pcount=${5}
    run_dimr=${6}
    
    scriptdirname=`readlink \-f \$0`
    echo ---------------------------------------------------------------------- 
    echo $scriptdirname
    echo submit_singularity.sh executing "execute_singularity.sh"
    echo pwd          : $pwd
    echo nNodes       : $nNodes
    echo nProc        : $nProc
    echo exec_sif     : $exec_sif
    echo -p           : $minp
    echo pcount       : $pcount
    echo run_dimr     : $run_dimr
    echo run_dimr arg 1 : ${7}
    echo run_dimr arg 2 : ${8}
    echo run_dimr arg 3 : ${9}
    echo run_dimr arg 4 : ${10}
    echo run_dimr arg 5 : ${11}
    echo run_dimr arg 6 : ${12}
    echo
    #
    # Create machinefile using $PE_HOSTFILE
    if [ "$nNodes" == "1" ]; then
        echo " ">$(pwd)/machinefile
    else
        if [ -n $nProc ]; then
            if [ -e $(pwd)/machinefile ]; then
                rm -f machinefile
            fi
            for (( i = 1 ; i <= `expr $nProc`; i++ )); do
                awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
            done
        else
           awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
        fi
    fi
    echo Contents of machinefile:
    cat $(pwd)/machinefile
    echo ---------------------------------------------------------------------- 

    if [ "$nNodes" == "1" ]; then
        # One Node: mpirun is executed inside run_dimr.sh, triggered by "-c nProc"
        echo One node execution: execute_singularity.sh -p 2 run_dimr.sh -c nProc
        $exec_sif $minp $pcount $run_dimr -c $nProc ${7} ${8} ${9} ${10} ${11} ${12} 
    else
        # Multi Node: mpirun must be outside execute_singularity.sh.
        # A Singularity container will be started for each partition. Note that run_dimr.sh does not contain a "-c nProc" specification, defaulting to "-c 1"
        echo Multi node execution: mpirun -n nPart -ppn nProc -f machinefile execute_singularity.sh -p 2 run_dimr.sh -c 1
        mpirun -n $nPart -ppn $nProc -f machinefile $exec_sif $minp $pcount $run_dimr ${7} ${8} ${9} ${10} ${11} ${12} 
    fi
fi
