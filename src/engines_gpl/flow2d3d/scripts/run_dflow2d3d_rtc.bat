@ echo off
title run_flow2d3d_rtc
    rem
    rem This script runs Delft3D-FLOW online with RTC on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Leave this script where it is.
    rem Call this script from within the working directory:
    rem path\to\delft3d\installation\x64\dflow2d3d\scripts\run_dflow2d3d_rtc.bat
    rem More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

setlocal enabledelayedexpansion

set flowConfigFile=config_d_hydro.xml
set debugLevel=-1
set skipWait=0
set forceExit=0
set goToUsage=0
set minDFound=0
    rem WARNING: execute the following line before handling arguments, otherwise it will be screwed up
set scriptDir=%~dp0


:HANDLEARGUMENTS
    if "%1"=="" goto HANDLEARGUMENTSFINISHED
    if [%1]         EQU [--help]      ( set goToUsage=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [-d]          ( set minDFound=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if  %minDFound% EQU 1             ( set debugLevel=%1 & set minDFound=0  & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [--skipWait]  ( set skipWait=1                       & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [--forceExit] ( set forceExit=1                      & goto CONTINUEWITHNEXTARGUMENT )
    rem When reaching this point, the current argument is not a recognized option.
    rem Assumption: this argument is the name of the dimr config file
    set flowConfigFile=%1
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
set rtcexedir=%D3D_HOME%\bin
set rtcdefaultdir=%sharedir%\rtc\
set libdir=%D3D_HOME%\lib


if  %debugLevel% EQU 0 (
    echo.
    echo "run_dflow2d3d_dwaves.bat arguments:"
    echo "    Configfile       : %flowConfigFile%"
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
echo executing in separate window: "%dflow2d3ddir%\d_hydro.exe" %flowConfigFile%
              start "Delft3D-FLOW" "%dflow2d3ddir%\d_hydro.exe" %flowConfigFile%





    rem Start RTC
title RTC simulation
    echo "    FLOW is started in a separate second command box"
    echo "    Wait until the following lines appears in the FLOW command box:"
    echo "        Part IV   - Reading complete MD-file..."
    echo "        Part V    - Initialisation and checking input..."
    echo "        Part VI   - Initialisation and checking second part..."
    echo "        Part VII  - Initialisation output..."
    echo "    Then press a key in the first command box to start RTC"
if  %skipWait% EQU 0 (
    C:\Windows\System32\timeout.exe 10
)

echo executing: "%rtcexedir%\rtc.exe" "%rtcdefaultdir%\RTC.FNM" "RTC.RTN"
                "%rtcexedir%\rtc.exe" "%rtcdefaultdir%\RTC.FNM" "RTC.RTN"



goto end

:USAGE
echo Usage:
echo run_dflow2d3d_rtc.bat [Options] [config_d_hydro.xml]
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml
echo     Options:
echo         --help        : (Optional) show this usage
echo         --skipWait    : (Optional) Skip waiting until FLOW is up and running (needed by the testbench)
echo         --forceExit   : (Optional) When this script is finished, execute the "exit" statement (needed by mormerge)
echo         -d 0          : (Optional) Maximum debug level is zero

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
if  %forceExit% EQU 1 (
    echo Forcing exit
    exit
)
