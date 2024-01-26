@ echo off
title %cd%
    rem
    rem This script is an example for running DFlowFM on Windows
    rem Adapt and use it for your own purpose
    rem

    rem
    rem Set the directories
    rem
set D3D_HOME=%~dp0..
echo D3D_HOME         : %D3D_HOME%
set exedir=%D3D_HOME%\bin
set sharedir=%D3D_HOME%\share
set libdir=%D3D_HOME%\lib

    rem Run

set path=%exedir%;%libdir%;%path%

    rem
    rem Run
    rem
"%exedir%\dflowfm-cli.exe" --nodisplay --autostartstop %1 %2 %3 %4 %5 %6 %7 %8  --processlibrary "%sharedir%\proc_def.dat" --bloomspecies "%sharedir%\bloom.spe"

