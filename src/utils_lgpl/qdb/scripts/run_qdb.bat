@ echo off
title run_trim2dep
rem
rem this script runs trim2dep on Windows
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

echo executing in this window: "%bindir%\qdb.exe" 
"%bindir%\qdb.exe" 
goto end

:usage
echo Purpose: 
echo Sets PATH and runs qdb on Windows. QDB is an application which converts stores 
echo and obtains fields from a NEFIS discharge database to and from NEFIS files for 
echo further processing by the Simulation Management Tool (SMT), which is a quasi-
echo steady hydrograph runner echo for Delft3D 4 simulations.  
echo.
echo Usage:
echo run_qdb.bat [ -h ^| --help ^| --usage]
echo.
echo     -h ^| --help ^| --usage show this usage (optional)
echo.
echo run_qdb.bat uses a text file "qdb.cmd" as input
echo.
echo The input file "qdb.cmd" should be in the location where the run_qdb.bat 
echo script is called from and should look like the example below where the comment 
echo statements (# ...) are removed.
echo.
echo -------------------------contents qdb.cmd------------------------------
echo STORE DON'T OVERWRITE  # Command for database - allowed options (RETRIEVE, STORE1, STORE, COPY or LIST) followed by 
echo 2500                   # Discharge level
echo trim-yac3c             # Simulation file 
echo qdb-yac3c              # Database file
echo -----------------------------------------------------------------------
echo.
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
