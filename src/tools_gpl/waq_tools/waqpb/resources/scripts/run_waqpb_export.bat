@ echo off
title run_waqpb_export
rem
rem this script runs waqpb_export on Windows
rem
setlocal enabledelayedexpansion

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set libdir=%bindir%\..\lib
set PATH=%libdir%;%bindir%;%PATH%

echo "    bin dir           : %bindir%"
echo "    lib dir           : %libdir%"

echo executing in this window: "%bindir%\waqpb_export.exe"
"%bindir%\waqpb_export.exe" %*

pause
