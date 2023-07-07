# Script to edit vfproj files to use intel IFX compiler instead of IFORT
# Currently, cmake + visual studio (2022) does not automatically allow changing fortran compiler

# Author: Maarten Klapwijk
# Date 09-06-2023

# Arguments of the script: 
#   - ifx/ifort: changes to ifx compiler/does nothing
#   - build_directory

 
import sys
import re
from pathlib import Path

build_dir = sys.argv[2]

folder_path = Path(build_dir).absolute()

regex = r"(?P<config_tag><Configuration[\w\s]+Name=\".*\|x64\")"
subst = "\\g<config_tag> UseCompiler=\"ifxCompiler\""

def convert_vfproj_files(file_name):
    with open(file_name, 'r') as file :
        filedata = file.read()
        filedata = re.sub(regex, subst, filedata, 0, re.MULTILINE)

        with open(file_name, 'w') as file:
            file.write(filedata)
            file.close()

if sys.argv[1] == "ifx":
    print("Converting *.vfproj files to use IFX instead of IFORT")
    for file_name in folder_path.rglob("*.vfproj"):
        convert_vfproj_files(file_name)
    print("Using IFX")
elif sys.argv[1] == "ifort":
    print("Using IFORT")
else:
    print("Argument given to convert_vfproj_to_use_ifx.py script not recognised. Exiting")