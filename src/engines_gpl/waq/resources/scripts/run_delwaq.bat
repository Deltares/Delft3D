@echo off
title run_delwaq
    rem
    rem This script runs Delwaq on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU []        goto usage
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage

rem set the directories containing the binaries and set PATH
set bindir=%~dp0
set libdir=%bindir%..\lib
set PATH=%libdir%;%bindir%;%PATH%

rem process the arguments (name of the input file is always the first argument)
set inputfile=%1
shift

rem go to input file directory, run delwaq, and return
set currentdir=%CD%
for %%A in ("%inputfile%") do (
    set argName=%%~nxA
    set argPath=%%~dpA
)
cd /d "%argPath%"
echo executing in this window: "%bindir%delwaq.exe" %argName%  %*
"%bindir%delwaq.exe" %argName% %*
cd /d "%currentdir%"
goto end

:usage
echo Purpose: Sets PATH and runs delwaq on Windows.
echo.
echo Usage:
echo run_delwaq.bat ^<inp-file^> [-p ^<proc_def^>] [-eco [^<spe-file^>]] [...]
echo     --help             : (Optional) show this usage
echo     ^<inp-file^>         : Name of the Delwaq input file (mandatory) 
echo     -p ^<proc_def^>      : use an alternative process library file instead of ../share/delft3d/proc_def
echo     -openbp ^<dll-file^> : provide a dll with extra subroutines for user defined processes
echo     -np                : do not use any Delwaq processes (all substances will be seen as tracers)
echo     -eco [^<spe-file^>]  : use BLOOM, optionally using an alternative algea database for the default
echo                          ../share/delft3d/bloom.spe
echo     ...                : any other options are passed through to Delwaq
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
