#!/usr/bin/env bash

#
# Use this script to start SWAN in Delft3D
# Assumptions: 
# - The SWAN binary is located in the same directory as this script
# - The name of the SWAN binary is hard-coded in this script
# - All needed so-files are in directory ./../lib
# - OMP version:
#   - Use OMP_NUM_THREADS_SWAN if defined: OMP_NUM_THREADS is stored at the start and reset at the end
#   - If OMP_NUM_THREADS_SWAN is not defined: unset OMP_NUM_THREADS
#   - To overrule this: see comments in this script
#

if [[ -f swan_sh.log ]]; then
  rm -rf swan_sh.log
fi
echo screen output of swan.sh is written to this file >swan_sh.log
echo and will be overwritten everytime that swan.sh is executed >>swan_sh.log
echo >>swan_sh.log

# For NHOSTS=1 the OpenMP version of SWAN will be started.

#
# Local options
#
debug=0
OMP_NUM_THREADS_BACKUP=$OMP_NUM_THREADS

# When using mpi to run FLOW in parallel, it is not possible to use mpi
# to run SWAN in parallel. By using "testpar=1", SWAN will not use mpi.
testpar=1
# testpar=$NHOSTS
# testpar=$NSLOTS
if [[ ! -z $testpar ]]; then
  if [[ $testpar -gt 1 ]]; then
    mpirun=1
  else
    mpirun=0
  fi
else
  mpirun=0
fi

scriptdirname=$(readlink \-f "$0")
scriptdir=${scriptdirname%/*}
D3D_HOME=$scriptdir/..

module load intelmpi/21.2.0 &>/dev/null
export FI_PROVIDER=tcp

#
# INITIALIZATION
#
MACHINE_TYPE=`uname -m`
 
if [[ $mpirun -eq 1 ]]; then
  SWANEXEC="$D3D_HOME/bin/swan_mpi"
else
  SWANEXEC="$D3D_HOME/bin/swan_omp"
  #
  # swan40.72AB and newer runs parallel using OpenMP, using the total number of cores on the machine by default
  # Two ways to force the number of parallel processes:
  # 1. Define environment parameter OMP_NUM_THREADS_SWAN with the correct number of processes
  # 2. Below: replace "unset OMP_NUM_THREADS" by "export OMP_NUM_THREADS=4" (with a self choosen value, 4 is choosen as an example)
  if [[ -z $OMP_NUM_THREADS_SWAN ]]; then
    unset OMP_NUM_THREADS
  else
    export OMP_NUM_THREADS=$OMP_NUM_THREADS_SWAN
  fi
fi

#
# DEBUG
#
if [[ $debug -eq 1 ]]; then
  echo "=== debug information (start) ===" >>swan_sh.log
  echo SGE_O_WORKDIR: $SGE_O_WORKDIR >>swan_sh.log
  echo HOSTNAME     : $HOSTNAME >>swan_sh.log
  echo NHOSTS       : $NHOSTS >>swan_sh.log
  echo NQUEUES      : $NQUEUES >>swan_sh.log
  echo NSLOTS       : $NSLOTS >>swan_sh.log
  echo PE_HOSTFILE  : $PE_HOSTFILE >>swan_sh.log
  echo D3D_HOME     : $D3D_HOME >>swan_sh.log
  echo PATH         : $PATH >>swan_sh.log
  echo "=== debug information (end) ===" >>swan_sh.log
fi

#
# RUN
#
type swan.sh >>swan_sh.log
echo "Using swan executable $SWANEXEC" >>swan_sh.log
echo " " >>swan_sh.log
echo "SWAN batchfile executed for Delft3D" >>swan_sh.log

echo "Performing computation for: INPUT" >>swan_sh.log
#
# Check whether SWAN executable exists
#
if [[ -x ${SWANEXEC} ]]; then
  #
  # Check whether inputfile INPUT exists
  #
  if [[ -f INPUT ]]; then
    #
    #echo press enter to continue
    #read dummy
    #
    if [[ $mpirun -eq 1 ]]; then
       echo "Start of parallel computation using $NSLOTS slots" >>swan_sh.log
       #
       # General.
       #
       mpirun -np $NSLOTS "${SWANEXEC}" >>swan_sh.log

       #
       # Move PRINT file to output file
       #
       slot_number=$NSLOTS
       while [[ $slot_number -ge 1 ]]
       do
          if [[ $slot_number -lt 10 ]]; then
             print_filename=PRINT-00$slot_number
          elif [[ $slot_number -lt 100 ]]; then
             print_filename=PRINT-0$slot_number
          elif [[ $slot_number -lt 1000 ]]; then
             print_filename=PRINT-$slot_number
          else
             echo Warning: for all slot numbers larger than 999, print files will be moved to PRINT-1000. >>swan_sh.log
             print_filename=PRINT-1000
          fi
          slot_number=`expr $slot_number - 1`
       done 
       echo "End of parallel computation using $NSLOTS slots." >>swan_sh.log
       
    else
       #
       # SWAN run on 1 node.
       #
       "${SWANEXEC}" >>swan_sh.log
       
    fi
    
  else
    echo " " >>swan_sh.log
    echo "*** Error: SWAN input file INPUT does not exist" >>swan_sh.log
    echo " " >>swan_sh.log
    # read dummy
  fi
else
  echo " " >>swan_sh.log
  echo "*** ERROR: SWAN executable does not exist" >>swan_sh.log
  echo "           ${SWANEXEC}" >>swan_sh.log
  # read dummy
fi

export OMP_NUM_THREADS=$OMP_NUM_THREADS_BACKUP

exit
