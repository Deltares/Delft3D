#!/bin/bash
#$ -V
#$ -j yes

set NHOSTS manually here:
export NHOSTS=3
NPART=$NHOSTS
    #
    # This script starts a single-domain Delft3D-FLOW computation on Linux in parallel mode
    # asuming nodes are allocated manually
    #
    # Usage example:
    # Create a file named machinefile as described below
    # Execute this script
    #
    # In case the error "unable to find mpd.conf" occurs:
    # Your home directory MUST contain file .mpd.conf with contents:
    # secretword=bla
    # and with file access permissions:
    # -r--------
    # 


    #
    # Specify the config file to be used here
    # 
argfile=config_d_hydro.xml





    #
    # Set the directory containing delftflow.exe here
    #
export ARCH=lnx64
export D3D_HOME=../../../bin
exedir=$D3D_HOME/$ARCH/flow2d3d/bin



cp corinp_lnx.dat corinp.dat
rm -f trim*.*
rm -f tri-diag.*
rm -f TMP_*.*
rm -f tstprt.*
rm -f COSUMO/FF2NF/FF2NF*.txt

    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 
module load intel/14.0.3
module load mpich2/3.1.4_intel_14.0.3



 
    ### The file "machinefile" is assumed to be created manually and should
    ### look like:
    ### machinename1:2
    ### machinename2:2
    ### machinename3:2

echo Contents of machinefile:
cat $(pwd)/machinefile
echo ----------------------------------------------------------------------





    # Run

    ### command="d3d.run -nproc "$NHOSTS" -input "$inputfile" -back no
    ### eval $command

    ### General for MPICH2, startup your MPICH2 communication network (you
    ### can check if it is already there with mpdtrace).
    ###
    ### The use of hydra instead of mpd is adviced. hydra is the default for mpich2 version 1.4.1
    ### From the README:
    ### hydra
    ### -----
    ### Hydra is the default process management framework that uses existing 
    ### daemons on nodes (e.g., ssh, pbs, slurm, sge) to start MPI processes. 
    ### More information on Hydra can be found at 
    ### http://wiki.mcs.anl.gov/mpich2/index.php/Using_the_Hydra_Process_Manager
    ### 
    ### mpd
    ### ---
    ### MPD was the traditional process manager in MPICH2. The file 
    ### mpich2-1.4.1/src/pm/mpd/README has more information about interactive commands 
    ### for managing the ring of MPDs. The MPD process manager is now deprecated.
#mpd &


    ### Optionally:
    ### Add option --rsh=/usr/bin/rsh to mpdboot
    ### This is needed when the following error appears:
    ### mpdboot_xh5000 (handle_mpd_output 420): from mpd on xh5001, invalid port info: no_port
#mpdboot -n $NHOSTS -f $(pwd)/machinefile --ncpus=2

# link mpich debug rubbish to /dev/null
node_number=$NPART
while test $node_number -ge 1
do
   node_number=`expr $node_number - 1`
   ln -s /dev/null log$node_number.irlog
done

    ### For debug purpose:
echo " "
echo "ldd libflow2d3d.so: start"
ldd $exedir/libflow2d3d.so
echo "ldd libflow2d3d.so: end"
echo " "

    ### General, start delftflow in parallel by means of mpirun.
    ### The machines in the h4 cluster are dual core; start 2*NHOSTS parallel processes
mpirun -np $NHOSTS $exedir/d_hydro.exe $argfile
    ### alternatives:
    ### mpiexec -n $DELTAQ_NumNodes delftflow_91.exe -r $inputfile.mdf
    ### mpiexec -n `expr $DELTAQ_NumNodes \* 2` $exedir/deltares_hydro.exe $argfile


rm -f log*.irlog

    ### General for MPICH2, finish your MPICH2 communication network.
#mpdallexit 

