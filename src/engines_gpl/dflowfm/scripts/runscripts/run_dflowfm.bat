@ echo off
title run_dflowfm
    rem
    rem This script runs dflowfm on Windows
    rem Adapt and use it for your own purpose
    rem
    rem For local parallel execution on Windows. No special setup is required. The -localonly flag allows MPI to run
    rem without the hydra service.
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\bin\run_dimr_parallel.bat
    rem More examples: check run scripts in https://github.com/Deltares/Delft3D/tree/main/examples/*






    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dflowfm\scripts\run_dflowfm.bat

setlocal enabledelayedexpansion

    rem
    rem Read arguments

    rem --help:
if [%1] EQU [--help] ( goto usage )


    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else (
   rem Getting and setting the number of physical cores
   for /F "tokens=*" %%C in ('powershell -Command "Get-CimInstance -ClassName Win32_Processor | Select-Object -ExpandProperty NumberOfCores | Measure-Object -Sum | Select-Object -ExpandProperty Sum"') do set NumberOfPhysicalCores=%%C
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
set exedir=%~dp0
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib

    rem Run
set PATH=%sharedir%;%libdir%
echo executing: "%exedir%dflowfm-cli.exe" %*
                "%exedir%dflowfm-cli.exe" %*

goto end

:usage
echo Usage:
echo run_dflowfm.bat [--help] [dflowfm options]
echo     --help          : (Optional) show this usage
echo     dflowfm options : (Optional) arguments to pass through to dflowfm

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
