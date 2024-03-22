@ echo off
title run_dimr_parallel
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
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dimr\scripts\run_dimr_parallel.bat
    rem More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

setlocal enabledelayedexpansion
set debuglevel=-1

    rem
    rem Read arguments

    rem No arguments:
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=dimr_config.xml
    goto readyreading
)

    rem --help:
if [%1] EQU [--help] ( goto usage )

    rem number of partitions:
set numpar=%1

    rem debuglevel and or configfile
if [%2] EQU [-d] (
    set debuglevel=%3
    if [%4] EQU [] (
        set argfile=dimr_config.xml
        goto readyreading
    ) else (
        set argfile=%4
        goto readyreading
    )
) else (
    set argfile=%2
)
if [%3] EQU [-d] (
    set debuglevel=%4
    goto readyreading
)

:readyreading

    rem Check configfile
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

    rem Check debuglevel, translate into argument for dimr
if  %debuglevel% EQU -1 (
    set debugarg=
) else (
    set debugarg=-d !debuglevel!
)

    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else (
   rem Getting and setting the number of physical cores
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)

echo number of partitions: %numpar%

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

    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%sharedir%;%libdir%;%exedir%
if exist %exedir%\vars.bat (
    echo executing: "%exedir%\vars.bat"
        call "%exedir%\vars.bat"
) else (
    echo "WARNING: File not found: %exedir%\vars.bat"
    echo "         Problems may occur when using IntelMPI"
)
echo executing: "%exedir%\mpiexec.exe" -n %numpar% -localonly "%exedir%\dimr.exe" %debugarg% %argfile%
                "%exedir%\mpiexec.exe" -n %numpar% -localonly "%exedir%\dimr.exe" %debugarg% %argfile%

goto end

:usage
echo Usage:
echo run_dimr_parallel.bat [--help] [n] [-d debuglevel] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions. Must match with the prepared D-Flow FM calculation.
echo                      Default value: NUMBER_OF_PROCESSORS
echo     -d debuglevel  : (Optional) debuglevel=0:ALL, 6:SILENT
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
