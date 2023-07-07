@ echo off
title run_waqpb_export
    rem
    rem This script runs waqpb_export on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the input arguments
    rem
set version= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set version=%1
    )
)
set serial= 
if [%2] EQU [] (
    goto usage
) else (
    if [%2] EQU [--help] (
        goto usage
    ) else (
        set serial=%2
    )
)
set procDefLoc= 
if [%3] EQU [] (
    goto usage
) else (
    if [%3] EQU [--help] (
        goto usage
    ) else (
        set procDefLoc=%3
    )
)
set csvFilesLoc=%procDefLoc%\csvFiles

echo Version: %version%
echo Serial: %serial%
echo Proc_def location: %procDefLoc%
echo CSV files location: %csvFilesLoc%


set workdir=%CD%
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

    rem go to csv files directory, run waqpb_export, and return
cd /d %csvFilesLoc%

echo executing in this window: "%waqdir%\waqpb_export.exe" "%version%" "%serial%"
"%waqdir%\waqpb_export.exe" "%version%" "%serial%"
move proc_def.dat ../
move proc_def.def ../
cd /d "%workdir%"


goto end

:usage
echo Usage:
echo run_waqpb_export.bat [--help] version serial
echo     --help             : (Optional) show this usage
echo     version            : (Mandatory) delwaq version
echo     serial             : (Mandatory) proc_def serial number
echo     procDefLoc         : (Mandatory) proc_def and csv files location
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
