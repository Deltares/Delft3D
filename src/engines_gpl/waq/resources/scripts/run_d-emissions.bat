@ echo off
title run_d-emissions
    rem
    rem This script runs D-Emissions on Windows
    rem
setlocal enabledelayedexpansion

set D3D_HOME=%~dp0..
set delwaq_script=%D3D_HOME%\bin\run_delwaq.bat
set d_emissionsdir=%D3D_HOME%\share\d-emissions
call "%delwaq_script%" %1 %2 %3 %4 %5 %6 %7 %8 %9 -p "%d_emissionsdir%\em_proc_def.dat"
