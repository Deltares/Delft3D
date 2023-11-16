@ echo off

   rem Relative from INSIDE a testcase:
set TCL_EXE=..\..\build_all\x64\share\bin\tclkitsh852.exe


echo "Running testcase 01_standard (sp) ..."
cd 01_standard
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run.bat >screen.log 2>&1


echo "Running testcase 01_standard parallel (sp) ..."
call run_parallel.bat >screen_parallel.log 2>&1
rem Undo changes
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 02_domaindecomposition (sp) ..."
cd 02_domaindecomposition
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run.bat >screen.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 03_flow-wave (sp) ..."
cd 03_flow-wave
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run.bat >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel (sp) ..."
call run_parallel.bat >screen_parallel.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 04_fluidmud (sp) ..."
cd 04_fluidmud
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d" "<library>flow2d3d_sp"
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run.bat >screen.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d_sp" "<library>flow2d3d"
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 05_mormerge (sp) ..."
cd 05_mormerge\input
"..\%TCL_EXE%" ..\..\sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
cd ..\merge
rem call run.bat >screen.log 2>&1
cd ..\input
"..\%TCL_EXE%" ..\..\sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..\..
dir

echo "Skipping testcases 06_delwaq, 07_wave, 08_part-tracer, 09_part-oil, and 10_delwaq-part-tracer (no sp variant)"


echo "Running testcase 11_standard_netcdf (sp) ..."
cd 11_standard_netcdf
%TCL_EXE% ..\sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run.bat >screen.log 2>&1
%TCL_EXE% ..\sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo ...finished
pause
