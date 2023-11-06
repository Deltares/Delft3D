@ echo off

echo "testing release version"
..\..\..\bin\win32\01_initest.exe >initest_win32_new.log 2>&1
comp initest_win32_new.log initest_win32.log

echo "testing debug version"
..\..\..\bin\win32_debug\01_initest.exe >initest_win32_new.log 2>&1
comp initest_win32_new.log initest_win32.log /L

pause
