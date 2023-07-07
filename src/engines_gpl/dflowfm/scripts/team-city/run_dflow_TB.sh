#/bin/bash
. /usr/share/Modules/init/sh

module load intel/14.0.3
module load netcdf/v4.3.2_v4.4.0_intel_14.0.3
module load mpich2/1.4.1_intel14.0.3
module load metis/5.1.0_intel14.0.3
module load petsc/3.4.0_intel14.0.3

SCRIPT_DIR=$( cd ${0%/*} && pwd -P )

$SCRIPT_DIR/dflowfm "$@"

