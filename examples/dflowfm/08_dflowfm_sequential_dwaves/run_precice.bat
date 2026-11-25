@ echo off

setlocal
set cleanupOnly=0
set usePreCICE=1
set startFM=1
set startDWaves=1
set installDir=install_fm-suite

if defined CLEANUP_ONLY_OVERRIDE set cleanupOnly=%CLEANUP_ONLY_OVERRIDE%
if defined USE_PRECICE_OVERRIDE set usePreCICE=%USE_PRECICE_OVERRIDE%
if defined START_FM_OVERRIDE set startFM=%START_FM_OVERRIDE%
if defined START_DWAVES_OVERRIDE set startDWaves=%START_DWAVES_OVERRIDE%

set bindir=..\..\..\%installDir%\bin

echo Cleaning ...
del /f /q dflowfm\DFM_OUTPUT_FlowFM\*.* >del.log 2>&1
del /f /q dflowfm\FlowFM_*.mdu >del.log 2>&1
del /f /q dflowfm\*.dia >del.log 2>&1
del /f /q dflowfm\fort.* >del.log 2>&1
del /f /q dflowfm\precice-exports\*.vtu >del.log 2>&1
del /f /q dflowfm\precice-exports\*.series >del.log 2>&1
del /f /q dflowfm\precice-profiling\*.txt >del.log 2>&1
del /f /q dwaves\*.dia >del.log 2>&1
del /f /q dwaves\precice-exports\*.vtu >del.log 2>&1
del /f /q dwaves\precice-exports\*.series >del.log 2>&1
del /f /q dwaves\precice-profiling\*.txt >del.log 2>&1
del /f /q dwaves\precice_debug_output.txt >del.log 2>&1
del /f /q precice_debug_output.txt >del.log 2>&1
del /f /q precice-profiling\*.* >del.log 2>&1
rmdir /s /q precice-run >del.log 2>&1
del /f /q del.log
echo ... Cleaning done
if "%cleanupOnly%"=="1" goto :eof

if %usePreCICE% EQU 1 (
    echo Start computation using preCICE ...
    if %startDWaves% EQU 1 (
        cd dwaves
        set PATH=..\%bindir%
	rem TODO: Update the next line once wave.exe actually supports preCICE.
        rem start ..\%bindir%\wave f34.mdw --online=precice ..\precice_config.xml
        cd ..
    ) else (
        echo Please start D-Waves
    )
    
    if %startFM% EQU 1 (
        cd dflowfm
        set PATH=..\%bindir%
	rem TODO: Uncomment the next line when 2D-only works.
        rem call ..\%bindir%\run_dflowfm.bat f34.mdu --precice
        cd ..
    ) else (
        echo Please start D-Flow FM
    )
    echo ... Computation using D-Waves done
) else (
    echo Start computation using DIMR ...
    call %bindir%\run_dimr.bat
    echo ... Computation using DIMR done
)
endlocal

if "%NO_PAUSE%"=="1" goto :eof
pause
:eof
