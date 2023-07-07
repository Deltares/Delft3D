#!/bin/bash

ulimit -s unlimited
export OMP_NUM_THREADS=1
export I_MPI_FABRICS=shm
export PATH=/opt/intel/oneapi/mpi/2021.4.0/bin:$PATH

/opt/delft3dfm_latest/lnx64/bin/run_dimr.sh
