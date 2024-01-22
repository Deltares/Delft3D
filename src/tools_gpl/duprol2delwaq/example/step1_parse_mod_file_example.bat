@echo off
set model=Eutrof1a
set modfile=%model%.mod
set ascfile=%model%.asc

..\bin\Parse.exe %modfile%
..\bin\waqpb_import.exe %ascfile% OPL newtab newfrm duprol
..\bin\waqpb_export.exe < updateNEF.ini
pause
