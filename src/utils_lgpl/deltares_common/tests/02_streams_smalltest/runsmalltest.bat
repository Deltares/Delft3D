@ echo off

set exedir=bin\release

start %exedir%\01-smalltest.exe receive

%exedir%\01-smalltest.exe send
