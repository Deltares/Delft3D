@ echo off
title run_flow2d3d_wave_mormerge
    rem
    rem This script runs Delft3D-FLOW, Delft3D-WAVE and Delft3D-mormerge on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the mm-config file
    rem
if [%1] EQU [] (
    set argfile=config.mm
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set argfile=%1
    )
)
echo Mormerge Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: Mormerge configfile "%argfile%" does not exist
    goto usage
)


set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem

rem Remove "\dmor\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%

set bindir=%~dp0
set dmorscripts=%bindir%
set TCL_EXE=%bindir%\tclkitsh852.exe
set scriptname=%dmorscripts%/mormerge.tcl


    rem
    rem No adaptions needed below
    rem

    rem Run
echo executing: %TCL_EXE% %scriptname% -i %argfile% -s %scriptname%
"%TCL_EXE%" "%scriptname%" -i %argfile% -s "%scriptname%"


goto end

:usage
echo Usage:
echo run_flow2d3d_wave_mormerge.bat [--help] [config.mm]
echo     --help    : (Optional) show this usage
echo     config.mm : (Optional) default: config.mm

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
