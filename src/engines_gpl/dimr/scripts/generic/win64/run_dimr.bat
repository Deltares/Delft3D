@ echo off
title run_dimr
    rem
    rem This script runs dimr on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dimr\scripts\run_dimr.bat
    rem More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

setlocal enabledelayedexpansion

    rem
    rem Read arguments

set dimrConfigFile=dimr_config.xml
set debugLevel=-1
set forceExit=0
set minDFound=0
set goToUsage=0
    rem WARNING: execute the following line before handling arguments, otherwise it will be screwed up
set scriptDir=%~dp0


:HANDLEARGUMENTS
    if "%~1"=="" goto HANDLEARGUMENTSFINISHED
    if [%1]         EQU [--help]      ( set goToUsage=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [-d]          ( set minDFound=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if  %minDFound% EQU 1             ( set debugLevel=%1 & set minDFound=0  & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [--forceExit] ( set forceExit=1                      & goto CONTINUEWITHNEXTARGUMENT )
    rem When reaching this point, the current argument is not a recognized option.
    rem Assumption: this argument is the name of the dimr config file
    set dimrConfigFile=%~1
    :CONTINUEWITHNEXTARGUMENT
    shift
goto HANDLEARGUMENTS
:HANDLEARGUMENTSFINISHED

if  %goToUsage% EQU 1 (
    goto USAGE
)

if  %debugLevel% EQU 0 (
    echo.
    echo run_dimr.bat arguments:
    echo     debugLevel     : %debugLevel%
    echo     dimrConfigFile : %dimrConfigFile%
    echo     forceExit      : %forceExit%
    echo.
)

    rem Check configfile
echo Configfile:%dimrConfigFile%
if not exist "%dimrConfigFile%" (
    echo ERROR: configfile "%dimrConfigFile%" does not exist
    goto USAGE
)

    rem Check debugLevel, translate into argument for dimr
if  %debugLevel% EQU -1 (
    set debugarg=
) else (
    set debugarg=-d !debugLevel!
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
echo OMP_NUM_THREADS is %OMP_NUM_THREADS%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%scriptDir%..
echo D3D_HOME         : %D3D_HOME%
set exedir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib
set proc_def_dir=%sharedir%\delft3d

    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%sharedir%;%libdir%;%exedir%
echo executing: "%exedir%\dimr.exe" %debugarg% %dimrConfigFile%
"%exedir%\dimr.exe" %debugarg% %dimrConfigFile%

goto END

:USAGE
    echo Usage:
    echo run_dimr.bat [--help] [-d debugLevel] [--forceExit] [dimr_config.xml]
    echo     --help         : (Optional) show this usage
    echo     -d debugLevel  : (Optional) debugLevel=0:ALL, 6:SILENT
    echo     --forceExit    : (Optional) execute "exit" at the end of this script. Needed by mormerge.
    echo     dimr_config.xml: (Optional) default: dimr_config.xml

:END
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
if  %forceExit% EQU 1 (
    echo Forcing exit
    exit
)

:ENDPROC
