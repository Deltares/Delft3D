#!/bin/bash
#$ -V
#$ -j yes

MPIRUNEXEC=/opt/mpich2-1.0.8-intel64/bin/mpirun

OPTS=-autostartstop

if [ -z "$1" ]; then
	echo "usage: run.sh <MDU-file> [number of processes per node]"
	exit 1
else
	MDUfile=$1
fi

if [ -z "$2" ]; then
	export processes_per_node=2
else
	export processes_per_node=$2
fi
echo "processes per node: $processes_per_node"

exedir=/u/pijl/bin 
#libdir=$D3D_HOME/flow2d3d/lib 
libdir=''


if [ -n $processes_per_node ]; then
   export NSLOTS=`expr $NHOSTS \* $processes_per_node`
fi
echo "number of slots: $NSLOTS"


#kill processes on nodes
awk '{print "ssh " $1 " pkill python"}' $PE_HOSTFILE



   ### General for MPICH2, create needed machinefile for mpdboot and
   ### mpiexec from $PE_HOSTFILE.
if [ -n $processes_per_node ]; then
   if [ -e $(pwd)/machinefile ]
   then
      rm machinefile
   fi
   for (( i = 1 ; i <= $processes_per_node; i++ ))
   do
      awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
   done
else
   awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
fi



    ### Specific setting for H3/H4 linuxclusters, needed for MPICH2
    ### commands (mpdboot, mpirun, mpiexed, mpdallexit etc.).
export PATH="/opt/mpich2-1.4.1-gcc-4.6.2/bin:${PATH}"
#export LD_LIBRARY_PATH=$exedir:$libdir:$LD_LIBRARY_PATH
#export LD_PRELOAD="$libdir/libgfortran.so.3" 
 
    ### Some general information available via SGE. Note that NHOSTS can be
    ### smaller than NSLOTS (for instance on multicore nodes).
echo ----------------------------------------------------------------------
echo Parallel run of Delft3D-FLOW with MPICH2 on H4 linuxcluster.
echo SGE_O_WORKDIR: $SGE_O_WORKDIR
echo HOSTNAME     : $HOSTNAME
echo NHOSTS       : $NHOSTS
echo NQUEUES      : $NQUEUES
echo NSLOTS       : $NSLOTS
echo PE_HOSTFILE  : $PE_HOSTFILE
echo MPI_VER      : $MPI_VER

echo Contents of machinefile:
cat $(pwd)/machinefile
echo ----------------------------------------------------------------------


pkill python
mpdboot -n $NHOSTS -f $(pwd)/machinefile
   ### --ncpus can be used to start the first "$processes_per_node" processes on
   ### the first node, the second "$processes_per_node" processes on the second node
   ### and so on. This may minimize the communication between the hosts.
   ### This will only work if the machinefile looks like this:
   ###     x907.deltares.nl:2
   ###     x903.deltares.nl:2
   ### instead of:
   ###     x907.deltares.nl:1
   ###     x903.deltares.nl:1
   ###     x907.deltares.nl:1
   ###     x903.deltares.nl:1
   # mpdboot -n $NHOSTS -f $(pwd)/machinefile --ncpus=$processes_per_node

   ### link mpich debug rubbish to /dev/null
node_number=$NSLOTS
while [ $node_number -ge 1 ]; do
   node_number=`expr $node_number - 1`
   ln -s /dev/null log$node_number.irlog
done

   ### General, start delftflow in parallel by means of mpirun.
if [ ${NSLOTS} -eq 1 ]; then
	echo "$exedir/dflowfm ${OPTS} ${MDUfile} > out.txt 2> err.txt"
	$exedir/dflowfm ${OPTS} ${MDUfile} > out.txt 2> err.txt
else
	echo "${MPIRUNEXEC} -np $NSLOTS $exedir/dflowfm ${OPTS} ${MDUfile}"
	${MPIRUNEXEC} -np $NSLOTS $exedir/dflowfm ${OPTS} ${MDUfile} > out.txt 2> err.txt
fi

   ### When finished:
rm -f log*.irlog

   ### General for MPICH2, finish your MPICH2 communication network.
mpdallexit 

   ### Local run: copy back
   # StageOut
