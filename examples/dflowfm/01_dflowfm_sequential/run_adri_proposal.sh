#!/bin/bash

source /p/delft3d/users/mourits/gitlab/delft3d/install_fm-suite/bin/activate

run_delft3d --set:HPC=deltares_h7 --set:nodetype=v16cpu
run_delft3d dflowfm dimr_config.xml --partition:ndomains=20
run_delft3d dimr dimr_config.xml -log-level DEBUG --SBATCH:--time=00:15:00
