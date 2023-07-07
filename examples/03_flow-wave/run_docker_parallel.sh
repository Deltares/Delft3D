#!/bin/bash

ulimit -s unlimited

/opt/delft3d_latest/lnx64/bin/run_dflow2d3d_parallel_dwaves.sh 3 --dockerparallel -w r17.mdw
