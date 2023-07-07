@ echo off
title run_waqpb_import
    rem
    rem This script runs waqpb_import on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion



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


set procDefLoc= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set procDefLoc=%1
    )
)
set csvFilesLoc=%procDefLoc%\csvFiles

echo Proc_def location: %procDefLoc%
echo CSV files location: %csvFilesLoc%


    rem go to directory, run waqpb_import, and return
cd /d %csvFilesLoc%

echo executing in this window: "%waqdir%\waqpb_import.exe"
"%waqdir%\waqpb_import.exe"

cd /d "%workdir%"


goto end

:usage
echo Usage:
echo run_waqpb_import.bat [--help] version serial
echo     --help             : (Optional) show this usage
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
