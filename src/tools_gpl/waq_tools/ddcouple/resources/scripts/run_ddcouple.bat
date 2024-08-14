@ echo off
title run_ddcouple
rem
rem this script runs ddcouple on Windows
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

rem set the hyd/ddb file
set hydddbfile=%1
echo ddcouple hyd/ddb-file:%hydddbfile%

rem go to directory, run ddcouple, and return
set currentdir=%CD%
For %%A in ("%hydddbfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%bindir%\ddcouple.exe" "%argName%" %2 %3 %4
"%bindir%\ddcouple.exe" "%argName%" %2 %3 %4
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs ddcouple on Windows.
echo.
echo Usage:
echo run_ddcouple.bat [^<hyd/ddb-file^> ^| -h ^| --help ^| --usage]
echo.
echo     ^<hyd/ddb-file^>          ddcouple input file (mandatory)
echo     -h ^| --help ^| --usage   show this usage (optional)
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
