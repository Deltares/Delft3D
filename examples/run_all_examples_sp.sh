#!/bin/bash


echo "Running testcase 01_standard (sp) ..."
cd 01_standard
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
./run.sh >screen.log 2>&1


echo "Running testcase 01_standard parallel (sp) ..."
./run_parallel.sh >screen_parallel.log 2>&1
   # Undo changes
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 02_domaindecomposition (sp) ..."
cd 02_domaindecomposition
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
./run.sh >screen.log 2>&1
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 03_flow-wave (sp) ..."
cd 03_flow-wave
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
./run.sh >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel (sp) ..."
./run_parallel.sh >screen_parallel.log 2>&1
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 04_fluidmud (sp) ..."
cd 04_fluidmud
../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d" "<library>flow2d3d_sp"
../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d" "<library>flow2d3d_sp"
./run.sh >screen.log 2>&1
../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d_sp" "<library>flow2d3d"
../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 05_mormerge (sp) ..."
cd 05_mormerge/input
../../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
cd ../merge
./run.sh >screen.log 2>&1
cd ../input
../../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ../..


echo "Skipping testcases 06_delwaq, 07_wave, 08_part-tracer, 09_part-oil, and 10_delwaq-part-tracer (no sp variant)"


echo "Running testcase 11_standard_netcdf (sp) ..."
cd 11_standard_netcdf
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
./run.sh >screen.log 2>&1
../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo ...finished

