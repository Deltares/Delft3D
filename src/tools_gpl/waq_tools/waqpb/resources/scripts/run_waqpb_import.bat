@ echo off
title run_waqpb_import
rem
rem this script runs waqpb_import on Windows
rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU []        goto usage
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set libdir=%bindir%\..\lib
set PATH=%libdir%;%bindir%;%PATH%

rem process arguments
set procDefLoc=%1
set csvFilesLoc=%procDefLoc%\csvFiles

echo proc_def location  : %procDefLoc%
echo csv-files location : %csvFilesLoc%

rem go to csv files directory, run waqpb_import, and return
set currentdir=%CD%
echo Working directory: %csvFilesLoc%
cd /d %csvFilesLoc%
echo executing in this window: "%bindir%\waqpb_import.exe"
"%bindir%\waqpb_import.exe"
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs waqpb_import on Windows.
echo.
echo Usage:
echo run_waqpb_import.bat ^<proc_def folder^> [OPTIONS]...
echo.
echo ^<proc_def folder^>   location of proc_def and csv files subfolder (a folder named csvFiles is assumed,
echo                     e.g. . (for the current work dir), mandatory).
echo -h, --help, --usage print this help message and exit

:end
pause
