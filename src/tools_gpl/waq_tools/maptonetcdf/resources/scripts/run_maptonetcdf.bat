@ echo off
title run_maptonetcdf
rem
rem this script runs maptonetcdf on Windows
rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU []        goto usage
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage
rem we need to see three arguments
if [%3] EQU [] (
    echo ERROR: not enough arguments given!
    echo.
    goto usage
    )

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set libdir=%bindir%\..\lib
set PATH=%libdir%;%bindir%;%PATH%

rem set the map-file, hyd-file and number of layers
set mapfile=%1
set ncfile=%2
set numLayers=%3
echo     mapFile          : %mapfile%
echo     ncFile           : %ncfile%
echo     numLayers        : %numLayers%

rem run
echo executing in this window: %bindir%\maptonetcdf.exe %mapfile% %ncfile% %numLayers%
%bindir%\maptonetcdf.exe  %mapfile% %ncfile% %numLayers%
goto end

:usage
echo Purpose: Sets PATH and runs maptonetcdf on Windows.
echo.
echo Purpose: Sets PATH and runs maptonetcdf on Windows with all given command line arguments.
echo.
echo Usage: run_maptonetcdf ^<mapFile.map^> ^<ncFile.nc^> ^<numLayers^> [OPTIONS]
echo.
echo Command line arguments:
echo ^<mapFile.map^>       maptonetcdf .map input file (mandatory).
echo ^<ncFile.nc^>         maptonetcdf .nc output file (mandatory).
echo ^<numLayers^>         number of layers (mandatory).
echo -h, --help, --usage print this help message and exit
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
