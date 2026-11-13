#!/bin/bash

# Run the coupled model from a directory containing the fm and cosumo subdirectories.

bindir="$(dirname "$(readlink -f "${0}")")"
export D3D_HOME="$(readlink -f "${bindir}/..")"
for program in preC-SUMO dflowfm; do
    if [[ ! -x "${bindir}/${program}" ]]; then
        printf 'Required executable not found or not executable: %s\n' "${bindir}/${program}" >&2
        exit 127
    fi
done
libdir="${D3D_HOME}/lib"
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

(cd cosumo && exec "${bindir}/preC-SUMO" -c csumo_settings.xml -p ../precice_config.xml) &
precsumo_pid=$!

(cd fm && exec "${bindir}/dflowfm" FlowFM.mdu --precice)
dflowfm_status=$?

if [[ ${dflowfm_status} -ne 0 ]]; then
    printf 'dflowfm failed with exit code %d\n' "${dflowfm_status}" >&2
    # preC-SUMO may still be waiting for a preCICE connection to dflowfm.
    kill "${precsumo_pid}" 2>/dev/null
    wait "${precsumo_pid}" 2>/dev/null
    exit "${dflowfm_status}"
fi

wait "${precsumo_pid}"
precsumo_status=$?
if [[ ${precsumo_status} -ne 0 ]]; then
    printf 'preC-SUMO failed with exit code %d\n' "${precsumo_status}" >&2
    exit "${precsumo_status}"
fi
