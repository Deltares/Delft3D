@ echo off

echo "testing release version"
..\..\..\bin\win32\02_treetest.exe >treetest_win32_new.log 2>&1
comp treetest_win32_new.log treetest_win32.log

echo "testing debug version"
..\..\..\bin\win32_debug\02_treetest.exe >treetest_win32_new.log 2>&1
comp treetest_win32_new.log treetest_win32.log /L

pause
