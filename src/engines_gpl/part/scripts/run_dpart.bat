@ echo off
title run_dpart
    rem
    rem This script runs Delft3D-FLOW on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the config file
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
        echo ERROR: configfile "%argfile%" does not exist
        goto usage
    )
)


set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

rem Remove "\dpart\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-23%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set sharedir=%D3D_HOME%\%ARCH%\share\bin
set partdir=%D3D_HOME%\%ARCH%\dpart\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%partdir%;%sharedir%;%~dp0
echo executing in this window: "%partdir%\delpar.exe" "%argfile%"
"%partdir%\delpar.exe" "%argfile%"

goto end

:usage
echo Usage:
echo run_dpart.bat [--help] part.inp
echo     --help   : (Optional) show this usage
echo     part.inp : (Mandatory) Delpar input file

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
