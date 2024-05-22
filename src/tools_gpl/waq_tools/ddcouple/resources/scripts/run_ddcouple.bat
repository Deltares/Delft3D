@ echo off
title run_ddcouple
    rem
    rem This script runs Ddcouple on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the hyd/ddb file
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
        echo ERROR: input hyd/ddb "%argfile%" does not exist
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


    rem go to directory, run ddcouple, and return
For %%A in ("%argfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%waqdir%\ddcouple.exe" "%argfile%" %2 %3 %4
"%waqdir%\ddcouple.exe" "%argName%" %2 %3 %4
cd /d "%workdir%"



goto end

:usage
echo Usage:
echo run_ddcouple.bat [--help] input.ddb
echo     --help             : (Optional) show this usage
echo     input.ddb          : (Mandatory) Ddcouple input file.
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
