@ echo off
title run_agrhyd
rem
rem this script runs Agrhyd on Windows
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

rem set the config file
set configfile=%1
echo agrhyd config file:%configfile%

rem go to directory, run agrhyd, and return
set currentdir=%CD%
For %%A in ("%configfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
echo Working directory: %argPath%
echo.
cd /d "%argPath%"
echo executing in this window: %bindir%agrhyd.exe "%argName%"
echo.
%bindir%\"agrhyd.exe" "%argName%"
cd /d "%currentdir%"

goto end

:usage
echo Purpose: Sets PATH and runs agrhyd on Windows.
echo.
echo Usage:
echo run_agrhyd.bat [^<ini-file^> ^| -h ^| --help ^| --usage] 
echo.
echo     ^<ini-file^>              agrhyd ini-input file (mandatory)
echo     -h ^| --help ^| --usage   show this usage (optional)
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
