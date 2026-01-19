#!/usr/bin/env bash
# To start DIMR, execute this script

# stop after an error occured:
set -e

# Set numbers of hosts and cores per host
nNodes=1
nProc=4

nPart=$((nNodes * nProc))

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml

# Folder with the MDU file, relative to the location of this script
mduFolder=dflowfm

# Replace number of processes in DIMR file
PROCESSSTR="$(seq -s " " 0 $((nPart-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1${PROCESSSTR}\2/" ${dimrFile}

# Read MDU file from DIMR-file
mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' ${dimrFile})".mdu

if [ "${nPart}" == "1" ]; then
    run_dimr.sh -m ${dimrFile}
else
    pushd ${mduFolder}
        echo "Starting partitioning..."
        run_dflowfm.sh --partition:ndomains=${nPart}:icgsolver=6 ${mduFile}
    popd
    echo "Starting computation..."
    run_dimr.sh -c ${nProc} -m ${dimrFile}
fi
