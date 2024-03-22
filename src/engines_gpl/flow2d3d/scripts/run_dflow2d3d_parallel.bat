@ echo off
title run_dflow2d3d_parallel
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
    rem This script runs Delft3D-FLOW in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Leave this script where it is.
    rem Call this script from within the working directory:
    rem path\to\delft3d\installation\x64\dflow2d3d\scripts\run_dflow2d3d_parallel.bat
    rem More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

setlocal enabledelayedexpansion

set numpar=%NUMBER_OF_PROCESSORS%
set flowConfigFile=config_d_hydro.xml
set debugLevel=-1
set forceExit=0
set goToUsage=0
set numparFound=0
set minDFound=0
    rem WARNING: execute the following line before handling arguments, otherwise it will be screwed up
set scriptDir=%~dp0


:HANDLEARGUMENTS
    if "%1"=="" goto HANDLEARGUMENTSFINISHED
    if [%1]         EQU [--help]      ( set goToUsage=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [-d]          ( set minDFound=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if  %minDFound% EQU 1             ( set debugLevel=%1 & set minDFound=0  & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [--forceExit] ( set forceExit=1                      & goto CONTINUEWITHNEXTARGUMENT )
    rem When reaching this point, the current argument is not a recognized option.
    rem Assumption: the first not-recognized argument is the number of partitions, \
    rem             the second not-recognized argument is the name of the dimr config file
    if  %numparFound% EQU 0 (
        set numpar=%1
        set numparFound=1
    ) else (
        set flowConfigFile=%1
    )
    :CONTINUEWITHNEXTARGUMENT
    shift
goto HANDLEARGUMENTS
:HANDLEARGUMENTSFINISHED

if  %goToUsage% EQU 1 (
    goto USAGE
)

if not exist %flowConfigFile% (
    echo ERROR: configfile "%flowConfigFile%" does not exist
    goto USAGE
)


set workdir=%CD%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%scriptDir%..

rem Remove "\dflow2d3d\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-27%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set dflow2d3ddir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib

if  %debugLevel% EQU 0 (
    echo.
    echo "run_dflow2d3d_dwaves.bat arguments:"
    echo "    Number of partitions : %numpar%"
    echo "    Configfile           : %flowConfigFile%"
    echo "    debugLevel       : %debugLevel%"
    echo "    forceExit        : %forceExit%"
    echo "    Working directory: %workdir%"
    echo "    D3D_HOME         : %D3D_HOME%"
    echo "    ARCH             : %ARCH%"
    echo.
)


    rem
    rem No adaptions needed below
    rem

    rem Start FLOW
set PATH=%dflow2d3ddir%;%sharedir%;%libdir%
echo executing: "mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %flowConfigFile%
                "mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %flowConfigFile%

goto end

:USAGE
echo Usage:
echo run_dflow2d3d_parallel.bat [Options] [n] [config_d_hydro.xml]
echo     n                 : (Optional) integer, number of partitions.
echo                                    Default value: %NUMBER_OF_PROCESSORS%
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml
echo     Options:
echo         --help        : (Optional) show this usage
echo         --forceExit   : (Optional) When this script is finished, execute the "exit" statement (needed by mormerge)
echo         -d 0          : (Optional) Maximum debug level is zero

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
if  %forceExit% EQU 1 (
    echo Forcing exit
    exit
)
