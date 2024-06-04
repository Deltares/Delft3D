#!/bin/env bash

fortran_files=$(find ./ -type f -iname "*.f90")

for fortran_file in ${fortran_files}; do
    echo "Converting ${fortran_file}"
    fprettify --config-file .fprettify.rc ${fortran_file}
done
