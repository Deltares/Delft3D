@ echo off
title run_delpar
rem
rem this script runs delpar on Windows
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

rem set the input file
set inputfile=%1
echo delpar inp-file:%inputfile%

rem go to directory, run delpar, and return
set currentdir=%CD%
For %%A in ("%inputfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
if "%argName%" == "runid.par" (
    set argName=
    rem argName is made empty on purpose, delpar looks for runid.par by default if no argument is given.
)
cd /d "%argPath%"
echo executing in this window: "%bindir%\delpar.exe" %argName%
"%bindir%\delpar.exe" %argName%
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs delpar on Windows.
echo.
echo Usage:
echo run_delpar.bat [^<inp-file^> ^| -h ^| --help ^| --usage]
echo.
echo     ^<inp-file^>              delpar input file (mandatory)
echo     -h ^| --help ^| --usage   show this usage (optional)
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
