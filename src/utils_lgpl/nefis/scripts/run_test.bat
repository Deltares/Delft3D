@ echo off
title run_nefis_test
rem
rem this script runs a nefis test on Windows
rem
setlocal enabledelayedexpansion

rem show usage?
if [%1] EQU [-h]      goto usage
if [%1] EQU [--help]  goto usage
if [%1] EQU [--usage] goto usage

rem Set the directories containing the binaries and set PATH
set testdir=%~dp0
set libdir=%testdir%\..\..\lib
set PATH=%libdir%;%testdir%;%PATH%

rem run nefis test
"%testdir%\test_%1.exe" 
rem check on error 
if %ERRORLEVEL% NEQ 0 (
  rem echo Task failed with error level %ERRORLEVEL%
  exit /b %ERRORLEVEL%
) else (
  rem echo Normal end.
  exit /b 0
)
goto end

:usage
echo Purpose: 
echo Runs Nefis tests
echo.
echo Usage:
echo run_test.bat [test number] [-h ^| --help ^| --usage]
echo.
echo     test number
echo     -h ^| --help ^| --usage show this usage (optional)
echo.
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause