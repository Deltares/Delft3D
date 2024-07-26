#!/bin/env bash

dir="${1:-./}"
if [[ ! $dir =~ ^"./" ]]; then
    dir="./$dir"
fi

echo "Look for Fortran files in directory: $dir"

cores_available=$(nproc)
cores="${2:-${cores_available}}"

echo "Using $cores cores"

find "$dir" -type f -iname "*.f90" -not \( -path "./src/third_party/*" -o -path "./src/third_party_open/*" -o -path "./src/engines_gpl/dflowfm/packages/dflowfm_lib/templates/*" -o -path "./src/engines_gpl/flow2d3d/*" \) \
    | xargs --max-procs $(nproc) --replace bash -c 'echo Formatting {}; (timeout --foreground --verbose 10 fprettify --config-file .fprettify.rc {}); if [ $? != 0 ]; then echo Timeout when formatting file {}; else echo Done formatting {}; fi'
