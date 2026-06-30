@ echo off
title run_waqpb_import
rem
rem this script runs checkhydbal on Windows
rem
setlocal enabledelayedexpansion

rem Set the directories containing the binaries and set PATH
set bindir=%~dp0
set PATH=%bindir%;%PATH%

echo "    bin dir           : %bindir%"

echo executing in this window: "%bindir%\checkhydbal.exe %*"
"%bindir%\checkhydbal.exe" %*

rem pause
