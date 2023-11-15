@ echo off

echo "testing release version"
..\..\..\bin\win32\test_01.exe >01test_win32_new.log 2>&1
comp 01test_win32_new.log 01test_win32.log

echo "testing debug version"
..\..\..\bin\win32_debug\test_01.exe >01test_win32_new.log 2>&1
comp 01test_win32_new.log 01test_win32.log /L

pause
