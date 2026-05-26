@echo off
set VSROOT=C:\Program Files\Microsoft Visual Studio\17\Community
call "%VSROOT%\Common7\Tools\VsDevCmd.bat" -arch=amd64 -host_arch=amd64
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" --force
cmd