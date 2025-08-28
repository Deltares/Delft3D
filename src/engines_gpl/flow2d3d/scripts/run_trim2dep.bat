@ echo off
title run_trim2dep
rem
rem this script runs waqmerge on Windows
rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set libdir=%bindir%\..\lib
set PATH=%libdir%;%bindir%;%PATH%

rem run trim2dep

echo executing in this window: "%bindir%\trim2dep.exe" 
"%bindir%\trim2dep.exe" 
goto end

:usage
echo Purpose: Sets PATH and runs trim2dep on Windows.
echo.
echo Usage:
echo run_trim2dep.bat [ -h ^| --help ^| --usage]
echo.
echo     -h ^| --help ^| --usage show this usage (optional)
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
