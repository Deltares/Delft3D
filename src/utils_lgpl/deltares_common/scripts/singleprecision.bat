@echo off

set TCL_EXE=..\..\..\third_party_open\tcl\bin\win64\tclkitsh863.exe

echo singleprecision.bat:
echo executing: %TCL_EXE% changeprecision.tcl single
%TCL_EXE% changeprecision.tcl single

if NOT %ErrorLevel% EQU 0 (
    exit /B %ErrorLevel%
)
