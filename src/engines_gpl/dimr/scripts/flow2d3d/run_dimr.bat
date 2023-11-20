@ echo off
title run_dimr

 rem set OMP_NUM_THREADS=1
    rem
    rem This script is an example for running d_hydro on Windows
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 24 june 2015
    rem 
    rem
setlocal enabledelayedexpansion


    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set argfile=%1
)
echo configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

    rem
    rem Set the directories containing the binaries
    rem
set ARCH=win64
set currentdir=%~dp0%
set D3D_HOME=%currentdir%..\..\bin


set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set flowexedir=%D3D_HOME%\%ARCH%\flow2d3d\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dimrexedir%;%flowexedir%
    rem With debug info: "%dimrexedir%\dimr.exe" -d 0xFFFFFFFF %argfile%
echo "Computation start   : %TIME%"
"%dimrexedir%\dimr.exe" %argfile%
echo "Computation finished: %TIME%"

goto end

:usage
echo Usage:
echo run_dimr.bat [--help] [config_dimr.xml]
echo     --help         : (Optional) show this usage
echo     config_dimr.xml: (Optional) default: config_dimr.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
