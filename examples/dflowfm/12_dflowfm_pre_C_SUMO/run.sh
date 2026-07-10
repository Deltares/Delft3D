#!/bin/bash

usePreCICE=1
startFM=1
startPreCSUMO=1

bindir=$(readlink -f ../../../install_fm-suite/bin)
export PATH=$bindir:$PATH
export LD_LIBRARY_PATH=$bindir:$LD_LIBRARY_PATH

rm -rf fm/DFM_OUTPUT_FlowFM
rm -rf fm/precice-exports
rm -f fm/precice-profiling/*.txt
rm -f cosumo/FF2NF/*.xml
rm -f cosumo/csumo_bmi.dia
rm -rf cosumo/precice-exports
rm -rf cosumo/precice-profiling
rm -f cosumo/precice_debug_output.txt
rm -f csumo_to_dflowfm.nc
rm -f precice_debug_output.txt
rm -r precice-profiling/*.txt
rm -rf precice-run

if [ "$usePreCICE" = "1" ] ; then
    if [ "$startPreCSUMO" = "1" ] ; then
        cd cosumo
        $bindir/preC-SUMO -c csumo_settings.xml -p ../precice_config.xml &
        cd ..
    else
        echo "Please start preC-SUMO"
    fi
    if [ "$startFM" = "1" ] ; then
        cd fm
        $bindir/dflowfm FlowFM.mdu --precice
        cd ..
    else
        echo "Please start FlowFM"
    fi
else
    $bindir/dimr dimr_config.xml
fi
