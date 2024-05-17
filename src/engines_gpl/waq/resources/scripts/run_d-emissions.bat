@ echo off
title run_d-emissions
    rem
    rem This script runs D-Emissions on Windows
    rem
setlocal enabledelayedexpansion

set D3D_HOME=%~dp0..
set em_procfile=%D3D_HOME%\share\d-emissions\em_proc_def
set delwaq_script=%D3D_HOME%\bin\run_delwaq.bat
call %delwaq_script% %1 %2 %3 %4 %5 %6 %7 %8 %9 -p %em_procfile%
