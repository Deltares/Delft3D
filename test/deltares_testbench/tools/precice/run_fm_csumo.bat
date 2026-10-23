@ echo off

rem This script starts both D-Flow FM and preC-SUMO. It is a temporary solution for the testbench.

setlocal
set bindir=%~pd0..\..\data\engines\teamcity_artifacts\x64\bin

dir %bindir%

echo Cleaning ...
del /f /q fm\DFM_OUTPUT_FlowFM\*.* >del.log 2>&1
del /f /q fm\2dis_*_net.nc >del.log 2>&1
del /f /q fm\DFM_interpreted_idomain_2dis_net.nc >del.log 2>&1
del /f /q fm\FlowFM_*.mdu >del.log 2>&1
del /f /q fm\*.dia >del.log 2>&1
del /f /q fm\fort.* >del.log 2>&1
del /f /q fm\precice-exports\*.vtu >del.log 2>&1
del /f /q fm\precice-exports\*.series >del.log 2>&1
del /f /q fm\precice-profiling\*.txt >del.log 2>&1
del /f /q cosumo\FF2NF\*.xml >del.log 2>&1
del /f /q cosumo\csumo_bmi.dia >del.log 2>&1
del /f /q cosumo\precice-exports\*.vtu >del.log 2>&1
del /f /q cosumo\precice-exports\*.series >del.log 2>&1
del /f /q cosumo\precice-profiling\*.txt >del.log 2>&1
del /f /q cosumo\precice_debug_output.txt >del.log 2>&1
del /f /q csumo_to_dflowfm.nc >del.log 2>&1
del /f /q precice_debug_output.txt >del.log 2>&1
del /f /q precice-profiling\*.* >del.log 2>&1
rmdir /s /q precice-run\preC-SUMO-fm >del.log 2>&1
del /f /q del.log
echo ... Cleaning done
echo Start computation using preCICE ...
cd cosumo
set PATH=%bindir%
start "" /B cmd /c "%bindir%\preC-SUMO.exe -c csumo_settings.xml -p ..\precice_config.xml > precsumolog.txt 2>&1"
cd ..
cd fm
set PATH=%bindir%
call %bindir%\run_dflowfm.bat FlowFM.mdu --precice
cd ..
endlocal

