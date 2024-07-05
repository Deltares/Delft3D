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
    | xargs --max-args 1 --max-procs $(nproc) timeout --foreground -v 10 fprettify --config-file .fprettify.rc
