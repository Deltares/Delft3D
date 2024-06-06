#!/bin/env bash

fortran_files=$(find "${1:-.}" -type f -iname "*.f90")

for fortran_file in ${fortran_files}; do
    echo "Converting ${fortran_file}"
    fprettify --config-file ./tools/fprettify/.fprettify.rc ${fortran_file}
done
