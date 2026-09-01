@ echo off

setlocal
set cleanupOnly=0
set usePreCICE=1
set startFM=1
set startPreCSUMO=1
set installDir=install_fm-suite

if defined CLEANUP_ONLY_OVERRIDE set cleanupOnly=%CLEANUP_ONLY_OVERRIDE%
if defined USE_PRECICE_OVERRIDE set usePreCICE=%USE_PRECICE_OVERRIDE%
if defined START_FM_OVERRIDE set startFM=%START_FM_OVERRIDE%
if defined START_PRECSUMO_OVERRIDE set startPreCSUMO=%START_PRECSUMO_OVERRIDE%

set bindir=..\..\..\%installDir%\bin

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
rmdir /s /q precice-run/* >del.log 2>&1
del /f /q del.log
echo ... Cleaning done
if "%cleanupOnly%"=="1" goto :eof

if %usePreCICE% EQU 1 (
    echo Start computation using preCICE ...
    if %startPreCSUMO% EQU 1 (
        cd cosumo
        set PATH=..\%bindir%
        start ..\%bindir%\preC-SUMO.exe -c csumo_settings.xml -p ..\precice_config.xml
        cd ..
    ) else (
        echo Please start preC-SUMO
    )
    
    if %startFM% EQU 1 (
        cd fm
        set PATH=..\%bindir%
        call ..\%bindir%\run_dflowfm.bat FlowFM.mdu --precice
        cd ..
    ) else (
        echo Please start D-Flow FM
    )
    echo ... Computation using preCICE done
) else (
    echo Start computation using DIMR ...
    call %bindir%\run_dimr.bat
    echo ... Computation using DIMR done
)
endlocal

if "%NO_PAUSE%"=="1" goto :eof

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
