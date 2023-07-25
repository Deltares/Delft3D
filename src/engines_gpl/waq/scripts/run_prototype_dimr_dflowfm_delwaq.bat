@ echo off
title run_prototype_dflowfm_delwaq_dimr
    rem
    rem This script runs prototype_dflowfm_delwaq_dimr on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the input arguments
    rem
set version= 
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



echo executing in this window: "%waqdir%\prototype_dflowfm_delwaq_dimr.exe"
"%waqdir%\prototype_dflowfm_delwaq_dimr.exe"



goto end

:usage
echo Usage:
echo run_prototype_dflowfm_delwaq_dimr.bat [--help] version serial
echo     --help             : (Optional) show this usage
echo     procDefLoc         : (Mandatory) proc_def location
:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
