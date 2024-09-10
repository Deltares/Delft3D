@ echo off
title run_waqpb_export
rem
rem this script runs waqpb_export on Windows
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

rem process arguments
set version=%1
set serial=%2
set procDefLoc=%3
set csvFilesLoc=%procDefLoc%\csvFiles

echo version            : %version%
echo serial             : %serial%
echo proc_def location  : %procDefLoc%
echo csv-files location : %csvFilesLoc%

rem go to csv files directory, run waqpb_export, and return
set currentdir=%CD%
echo Working directory: %csvFilesLoc%
cd /d %csvFilesLoc%
echo executing in this window: "%bindir%\waqpb_export.exe" "%version%" "%serial%"
"%bindir%\waqpb_export.exe" "%version%" "%serial%"
move proc_def.dat ../
move proc_def.def ../
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs waqpb_export on Windows with all given command line arguments.
echo.
echo Usage:
echo run_waqpb_export.bat -version*.* -serial?????????? ^<proc_def folder^> [OPTIONS]...
echo.
echo -version*.*         delwaq version number (a real number, e.g. -version7.0, mandatory).
echo -serial??????????   proc_def serial number (10 digits, e.g. -serial2024071099, mandatory).
echo ^<proc_def folder^>   location of proc_def and csv files subfolder (a folder named csvFiles is assumed,
echo                     e.g. . (for the current work dir), mandatory).
echo -h, --help, --usage print this help message and exit

:end
pause
