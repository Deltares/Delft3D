@ echo off
rem Usage:
rem     Either:
rem         Call this script with one argument being the path to a Dimrset-bin folder containing a matching run script
rem     Or:
rem         Build the source code
rem         In this script: Set dimrset_bin to point to the appropriate "install-folder\bin"
rem         Execute this script
rem 

if "%~1" == "" (
    set "dimrset_bin=%~dp0..\..\..\install_all\bin"
) else (
    set "dimrset_bin=%~1"
)
for %%I in ("%dimrset_bin%") do set "dimrset_bin=%%~fI"

call "%dimrset_bin%\run_dimr.bat" dimr_config.xml


rem pause
