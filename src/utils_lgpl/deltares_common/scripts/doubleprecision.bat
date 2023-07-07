@echo off

set TCL_EXE=..\..\..\third_party_open\tcl\bin\win64\tclkitsh852.exe

echo doubleprecision.bat:
echo executing: %TCL_EXE% changeprecision.tcl double
%TCL_EXE% changeprecision.tcl double

if NOT %ErrorLevel% EQU 0 (
    exit /B %ErrorLevel%
)
