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
set D3D_HOME=%~dp0..\..\..

rem Remove "\dwaq\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set waqdir=%D3D_HOME%\%ARCH%\dwaq\bin

set sharedir=%D3D_HOME%\%ARCH%\share\bin
set PATH=%waqdir%;%sharedir%

    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%waqdir%;%sharedir%;%~dp0

    rem go to directory, run agrhyd, and return
For %%A in ("%argfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%waqdir%\agrhyd.exe" "%argfile%"
"%waqdir%\agrhyd.exe" "%argName%"
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
