#!/bin/bash
# To start Dimr, execute this script

# stop after an error occured:
set -e

# Set numbers of hosts and cores per host
nNodes=1
nProc=1

# set DIMR version to be used inside DOCKER: 
dimrdir=/opt/delft3dfm_latest
export PROC_DEF_DIR=$dimrdir/lnx64/share/delft3d

# DOCKER: no queue selection
#

nPart=$((nNodes * nProc))

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml

# Replace number of processes in DIMR file
PROCESSSTR="$(seq -s " " 0 $((nPart-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile

# Read MDU file from DIMR-file
mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

# jobName: $FOLDERNAME
export jobName="${PWD##*/}"


if [ "$nPart" == "1" ]; then
    $dimrdir/lnx64/bin/run_dimr.sh -m $dimrFile
else
    cd dflowfm
    $dimrdir/lnx64/bin/run_dflowfm.sh --partition:ndomains=$nPart:icgsolver=6 $mduFile
    cd ..
    $dimrdir/lnx64/bin/run_dimr.sh -c $nProc -m $dimrFile
fi
