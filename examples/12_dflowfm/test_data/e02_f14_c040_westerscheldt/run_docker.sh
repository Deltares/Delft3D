#!/bin/bash

ulimit -s unlimited

/opt/delft3dfm_latest/lnx64/bin/run_dimr.sh -c 3 --D3D_HOME /opt/delft3dfm_latest/lnx64
