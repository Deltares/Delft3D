@ echo off
title %cd%
    rem
    rem This script is an example for running DFlowFM on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Deprecated! Calling run_dflowfm instead
    rem

    rem
    rem Set the directories
    rem
set D3D_HOME=%~dp0..
set exedir=%D3D_HOME%\bin
call "%exedir%\run_dflowfm.bat" %*

