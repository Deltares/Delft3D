@ echo off
title %cd%
    rem
    rem This script is an example for running DFlowFM on Windows
    rem Adapt and use it for your own purpose
    rem

    rem
    rem Set the directories
    rem
set scriptdir=%~dp0
set dflowfmexedir=%scriptdir%\..\bin
set libdir=%scriptdir%\..\..\share\bin
set procdefbloomspedir=%scriptdir%\..\default
set path=%dflowfmexedir%;%libdir%;%path%

    rem
    rem Run
    rem
"%dflowfmexedir%\dflowfm-cli.exe" --nodisplay --autostartstop %1 %2 %3 %4 %5 %6 %7 %8  --processlibrary "%procdefbloomspedir%\proc_def.dat" --bloomspecies "%procdefbloomspedir%\bloom.spe"

