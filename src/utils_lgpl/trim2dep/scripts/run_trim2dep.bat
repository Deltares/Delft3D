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

echo executing in this window: "%bindir%\trim2dep.exe" 
"%bindir%\trim2dep.exe" 
goto end

:usage
echo Purpose: 
echo Sets PATH and runs trim2dep on Windows. Trim2dep is an application which converts 
echo a field from a NEFIS trim-file to a .dep file for further processing. This is used 
echo by the Simulation Management Tool (SMT), which is a quasi-steady hydrograph runner 
echo for Delft3D 4 simulations.  
echo.
echo Usage:
echo run_trim2dep.bat [ -h ^| --help ^| --usage]
echo.
echo     -h ^| --help ^| --usage show this usage (optional)
echo.
echo run_trim2dep.bat uses a text file "trim2dep.cmd" as input
echo.
echo The input file "trim2dep.cmd" should be in the location where the run_trim2dep.bat 
echo script is called from and should look like the example below where the comment 
echo statements (# ...) are removed.
echo.
echo -------------------------contents trim2dep.cmd------------------------------
echo trimfile/trim-br1    # TRIM or COM file
echo refplane.dep         # depth-file
echo map-const            # group name (see Delft3D\VSI.exe)
echo DPS0                 # element name
echo 1                    # Which record from trim/com file
echo -999                 # Inactive cell value (mapconst:KCS) (Only for Reals)
echo ----------------------------------------------------------------------------
echo.
:end

rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
