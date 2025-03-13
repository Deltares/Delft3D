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
set newtabArg =
if [%2] EQU [-newtab] set newtabArg=-newtab
if [%2] EQU [--newtab] set newtabArg=-newtab

echo proc_def location  : %procDefLoc%
echo csv-files location : %csvFilesLoc%
echo newtab option : %newtabArg%

rem go to csv files directory, run waqpb_import, and return
set currentdir=%CD%
echo Working directory: %csvFilesLoc%
cd /d %csvFilesLoc%
echo executing in this window: "%bindir%\waqpb_import.exe" %newtabArg%
"%bindir%\waqpb_import.exe" %newtabArg%
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs waqpb_import on Windows.
echo.
echo Usage:
echo run_waqpb_import.bat ^<proc_def folder^> [-newtab] || [--newtab]
echo.
echo ^<proc_def folder^>         Location of proc_def and csv files subfolder (mandatory).
echo                           Use the character . for default. Then a subfolder with the name csvFiles is assumed.
echo.
echo -h, --help, --usage       Print this help message and exit
echo.
echo -newtab, --newtab         This optional argument will disregard the content of any existing *.csv files.
echo                           If any are found, they will be overwritten.

:end
pause
