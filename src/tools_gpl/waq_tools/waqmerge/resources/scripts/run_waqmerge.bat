@ echo off
title run_waqmerge
rem
rem this script runs waqmerge on Windows
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
set mdufile=%1
echo waqmerge mdu-file:%mdufile%

rem go to directory, run waqmerge, and return
set currentdir=%CD%
For %%A in ("%mdufile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%bindir%\waqmerge.exe" "%argName%" %2 %3 %4
"%bindir%\waqmerge.exe" "%argName%" %2 %3 %4
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs waqmerge on Windows.
echo.
echo Usage:
echo run_waqmerge.bat [^<mdu-file^> ^| -h ^| --help ^| --usage]
echo.
echo     ^<mdu-file^>              waqmerge input file (mandatory)
echo     -h ^| --help ^| --usage   show this usage (optional)
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
