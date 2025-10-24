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
    set dimrset_bin="..\..\..\install_all\bin"
) else (
    set dimrset_bin=%1
)

call "%dimrset_bin:"=%\run_dflow2d3d_fluidmud.bat" -wconfig config_d_hydro_sed.xml -mconfig config_d_hydro_mud.xml


rem pause
