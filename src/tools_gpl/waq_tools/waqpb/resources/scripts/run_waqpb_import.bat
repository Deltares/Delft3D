@ echo off
title run_waqpb_import
rem
rem this script runs waqpb_import on Windows
rem
setlocal enabledelayedexpansion

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set PATH=%bindir%;%PATH%

echo "    bin dir           : %bindir%"

echo executing in this window: "%bindir%\waqpb_import.exe %*"
"%bindir%\waqpb_import.exe" %*

rem pause
