#!/bin/bash

# This script starts both D-Flow FM and preC-SUMO. It is a temporary solution for the testbench.

scriptdir="$(dirname "$(readlink -f "${0}")")"
installdir="${scriptdir}/../../data/engines/teamcity_artifacts/lnx64"
bindir="$(readlink -f "${installdir}/bin")"
libdir="$(readlink -f "${installdir}/lib")"
export PATH="${bindir}:${PATH}"
export LD_LIBRARY_PATH="${libdir}:${LD_LIBRARY_PATH}"

rm -rf fm/DFM_OUTPUT_FlowFM
rm -rf fm/precice-exports
rm -f fm/precice-profiling/*.txt
rm -f cosumo/FF2NF/*.xml
rm -f cosumo/csumo_bmi.dia
rm -rf cosumo/precice-exports
rm -f cosumo/precice-profiling/*.txt
rm -f cosumo/precice_debug_output.txt
rm -f csumo_to_dflowfm.nc
rm -f precice_debug_output.txt
rm -f precice-profiling/*.txt
rm -rf precice-run

cd cosumo
"${bindir}/preC-SUMO" -c csumo_settings.xml -p ../precice_config.xml &
cd ..
cd fm
"${bindir}/dflowfm" FlowFM.mdu --precice
cd ..
