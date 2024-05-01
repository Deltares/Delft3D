@ echo off
title run_agrhyd
    rem
    rem This script runs Agrhyd on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the mdu file
    rem
set argfile= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set argfile=%1
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    if not exist %argfile%.inp (
        echo ERROR: input ini file "%argfile%" does not exist
        goto usage
    )
)

set workdir=%CD%
set argfile=%workdir%\%argfile%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..

set waqdir=%D3D_HOME%\bin

set sharedir=%D3D_HOME%\share\delft3d
set libdir=%D3D_HOME%\lib
set PATH=%sharedir%;%libdir%;%waqdir%


    rem go to directory, run agrhyd, and return
For %%A in ("%argfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: %waqdir%\"agrhyd.exe" "%argfile%"
%waqdir%\"agrhyd.exe" "%argName%"
cd /d "%workdir%"



goto end

:usage
echo Usage:
echo run_agrhyd.bat [--help] agrhyd.ini
echo     --help             : (Optional) show this usage
echo     agrhyd.ini         : (Mandatory) agrhyd input file.
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
