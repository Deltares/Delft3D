@ echo off
title run_dflowfm
    rem
    rem This script runs dfm_volume_tool on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dflowfm\scripts\run_dfm_volume_tool.bat

setlocal enabledelayedexpansion

    rem
    rem Read arguments

    rem --help:
rem if [%1] EQU [--help] ( goto usage )


    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else (
   rem Getting and setting the number of physical cores
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%
set exedir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib

    rem Run
set PATH=%sharedir%;%libdir%
echo executing: "%exedir%\dfm_volume_tool.exe"  %1 %2 %3 %4 %5 %6 %7 %8 %9
"%exedir%\dfm_volume_tool.exe" %1 %2 %3 %4 %5 %6 %7 %8 %9

goto end

:usage
echo Usage:
echo run_dfm_volume_tool.bat [--help] [dfm_volume_tool options]
echo     --help            : (Optional) show this usage
echo     dfm_volume_tool options : (Optional) arguments to pass through to dfm_volume_tool

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
