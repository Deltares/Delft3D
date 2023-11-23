@ echo off

echo "testing release version"
..\..\..\bin\win32\03_dredgedumptest.exe >dredgedumptest_win32_new.log 2>&1
comp dredgedumptest_win32_new.log dredgedumptest_win32.log

echo "testing debug version"
..\..\..\bin\win32_debug\03_dredgedumptest.exe >dredgedumptest_win32_new.log 2>&1
comp dredgedumptest_win32_new.log dredgedumptest_win32.log /L

pause
