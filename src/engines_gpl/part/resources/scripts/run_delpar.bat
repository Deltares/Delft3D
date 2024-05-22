@ echo off
title run_delpar

    rem This script runs Delft3D-FLOW on Windows
    rem Adapt and use it for your own purpose

setlocal enabledelayedexpansion


    rem Set the config file

set inputfile=
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set inputfile=%1
    )
)
echo Inputfile: %inputfile%
if not exist %inputfile% (
    if not exist %inputfile%.inp (
        echo ERROR: inputfile "%inputfile%" does not exist
        goto usage
    )
)
if "%inputfile%" == "runid.par" (
    echo input file is set to "runid.par".
    set inputfile=
    rem inputfile is made empty on purpose, to maintain backwards compatibility with Delft3d4
)


set workdir=%CD%
echo Working directory: %workdir%
set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%
set partdir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib


set PATH=%partdir%;%sharedir%;%libdir%
echo executing in this window: "%partdir%\delpar.exe" %inputfile%
"%partdir%\delpar.exe" %inputfile%

goto end

:usage
echo Usage:
echo run_delpar.bat [--help] *.inp/runid.par
echo     --help   : (Optional) show this usage
echo        *.inp : (Optional) Delpar input file
echo    rundi.par : (Optional) Delpar configuration file, containing name of *.inp and *.mdu file

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
