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
    if "%1"=="" goto HANDLEARGUMENTSFINISHED
    if [%1]         EQU [--help]      ( set goToUsage=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [-d]          ( set minDFound=1                      & goto CONTINUEWITHNEXTARGUMENT )
    if  %minDFound% EQU 1             ( set debugLevel=%1 & set minDFound=0  & goto CONTINUEWITHNEXTARGUMENT )
    if [%1]         EQU [--forceExit] ( set forceExit=1                      & goto CONTINUEWITHNEXTARGUMENT )
    rem When reaching this point, the current argument is not a recognized option.
    rem Assumption: this argument is the name of the dimr config file
    set dimrConfigFile=%1
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
if not exist %dimrConfigFile% (
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
set D3D_HOME=%scriptdir%..\..\..

    rem Remove "\dimr\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%
    rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set cosumobmidir=%D3D_HOME%\%ARCH%\cosumo_bmi\bin
set delwaqexedir=%D3D_HOME%\%ARCH%\dwaq\bin
set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set proc_def_dir=%D3D_HOME%\%ARCH%\dflowfm\default
set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set esmfexedir=%D3D_HOME%\%ARCH%\esmf\bin
set esmfbatdir=%D3D_HOME%\%ARCH%\esmf\scripts
set flow1dexedir=%D3D_HOME%\%ARCH%\dflow1d\bin
set flow1d2dexedir=%D3D_HOME%\%ARCH%\dflow1d2d\bin
set rrexedir=%D3D_HOME%\%ARCH%\drr\bin
set rtctoolsexedir=%D3D_HOME%\%ARCH%\drtc\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set sharedir=%D3D_HOME%\%ARCH%\share\bin
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin
set wandaexedir=%D3D_HOME%\%ARCH%\wanda\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%cosumobmidir%;%dimrexedir%;%delwaqexedir%;%dflowfmexedir%;%flow1dexedir%;%flow1d2dexedir%;%rtctoolsexedir%;%rrexedir%;%waveexedir%;%swanbatdir%;%swanexedir%;%esmfbatdir%;%esmfexedir%;%wandaexedir%;%sharedir%
echo executing: "%dimrexedir%\dimr.exe" %debugarg% %dimrConfigFile%
"%dimrexedir%\dimr.exe" %debugarg% %dimrConfigFile%

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
