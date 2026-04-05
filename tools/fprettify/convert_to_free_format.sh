#!/usr/bin/env bash

# You can use this script to convert all .f/.F files in the repository to .f90/.F90 files
# Note, make sure to also update references to the .f/.F files in CMakeLists.txt files

set -eu

# use -iname to also find .F files
f_files=$(find ./ -type f -iname "*.f" -not -path "./src/third_party_open/*")	

for f_file in $f_files; do
	echo "Converting $f_file to free format"
	f90_file_temp="${f_file}tmp"
	# write formatted output to a temporary file, or we'll have very strange output
	findent -ofree < "${f_file}" > "${f90_file_temp}"
	# overwrite the old file with the temporary, correctly formatted file
	mv "${f90_file_temp}" "${f_file}"
	git add "${f_file}"
done

git commit -m "Convert .f/.F files to free format"

for f_file in $f_files; do
	f90_file="${f_file}90"
	echo "Renaming $f_file to ${f90_file}"
	git mv "${f_file}" "${f90_file}"
done

git commit -m "Rename .f/.F files to .f90/.F90"
