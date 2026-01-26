#!/bin/bash

# Usage:
#     Either:
#         Call this script with one argument being the path to a Dimrset-bin folder containing a matching run script
#     Or:
#         Build the source code
#         In this script: Set dimrset_bin to point to the appropriate "install-folder\bin"
#         Execute this script
# 

if [ -z "$1" ]; then
    dimrset_bin=../../../build_all/lnx64/bin
else
    dimrset_bin=$1
fi

$dimrset_bin/run_dflow2d3d.sh

