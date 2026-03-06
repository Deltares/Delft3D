@ echo off

   rem Relative from INSIDE a testcase:
set TCL_EXE=..\..\build_all\x64\share\bin\tclkitsh852.exe


echo "Running testcase 01_standard ..."
cd 01_standard
call run.bat >screen.log 2>&1


echo "Running testcase 01_standard parallel ..."
call run_parallel.bat >screen_parallel.log 2>&1
cd ..


echo "Running testcase 02_domaindecomposition ..."
cd 02_domaindecomposition
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 03_flow-wave ..."
cd 03_flow-wave
call run.bat >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel ..."
call run_parallel.bat >screen_parallel.log 2>&1
cd ..


echo "Running testcase 04_fluidmud ..."
cd 04_fluidmud
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 05_mormerge ..."
cd 05_mormerge\merge
call run.bat >screen.log 2>&1

echo "Running testcase 05_mormerge (alternative( ..."
call run_mormerge_alternative.bat >screen_mormerge_alternative.log 2>&1
cd ..\..


echo "Running testcase 06_delwaq ..."
cd 06_delwaq
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 07_wave ..."
cd 07_wave
call run.bat >screen.log 2>&1

echo "Running testcase 07_wave (via dimr) ..."
call run_wave_via_dimr.bat >screen_wave_via_dimr.log 2>&1
cd ..


echo "Running testcase 08_part-tracer ..."
cd 08_part-tracer
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 09_part-oil ..."
cd 09_part-oil
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 10_delwaq-part-tracer ..."
cd 10_delwaq-part-tracer
call run.bat >screen.log 2>&1
cd ..


echo "Running testcase 11_standard_netcdf ..."
cd 11_standard_netcdf
call run.bat >screen.log 2>&1
cd ..

echo ...finished
pause
