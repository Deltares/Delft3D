@ echo off

rem set bindir=..\..\..\..\install_fm-suite\bin
set bindir=c:\adri\work\delft3d\2026.01\x64\bin
set libdir=%bindirNF%\..\lib

del /f /q DFM_OUTPUT_FlowFM\*.*

call %bindir%\run_dimr.bat

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
