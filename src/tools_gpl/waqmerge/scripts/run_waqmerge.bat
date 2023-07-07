@ echo off
title run_waqmerge
    rem
    rem This script runs Waqmerge on Windows
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
        echo ERROR: input mdu file "%argfile%" does not exist
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

    rem go to directory, run waqmerge, and return
For %%A in ("%argfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%waqdir%\waqmerge.exe" "%argfile%"
"%waqdir%\waqmerge.exe" "%argName%"
cd /d "%workdir%"



goto end

:usage
echo Usage:
echo run_waqmerge.bat [--help] input.mdu
echo     --help             : (Optional) show this usage
echo     input.mdu          : (Mandatory) Waqmerge input file.
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
