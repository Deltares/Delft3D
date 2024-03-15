@ echo off
title run_dflowfm
    rem
    rem This script runs dflowfm on Windows
    rem Adapt and use it for your own purpose
    rem
    rem When using intelMPI for the first time on a machine:
    rem Execute "hydra_service.exe -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\hydra_service.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Windows Start button" -> type "cmd", right-click "Command Prompt" App, "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin (or your local copy)
    rem         hydra_service.exe -install
    rem         mpiexec.exe -register -username <user> -password <password> -noprompt
    rem     When there is an hydra_service/smpd already running on the machine, it must be ended first, using the Microsoft Task Manager,
    rem     or in the command  box: hydra_service.exe -uninstall (smpd -uninstall)
    rem
    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dimr\scripts\run_dimr_parallel.bat
    rem More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*






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
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)


    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%
set exedir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib

set workdir=sharedir\ec_module_test\
cd %D3D_HOME%\share\ec_module_test\
echo Working directory: %workdir%

    rem Run
set PATH=%sharedir%;%libdir%
echo executing: "%exedir%\ec_module_test.exe" 
		"%exedir%\ec_module_test.exe" -c internal
		python run_all_tests.py
		

goto end

:usage
echo Usage:
echo run_dflowfm_tests.bat [--help] [dflowfm options]
echo     --help          : (Optional) show this usage
echo     dflowfm options : (Optional) arguments to pass through to dflowfm

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
