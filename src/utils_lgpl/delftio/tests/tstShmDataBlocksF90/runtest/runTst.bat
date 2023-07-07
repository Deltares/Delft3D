@echo off

del TEST*.* >del.log 2>&1
del del.log

start ..\x64\%1\tstShmDatablock-put.exe
..\x64\%1\tstShmDatablock-get.exe


rem fc TEST2DFSerial-res.txt ..\resultsApproved\w32\TEST2DFSerial-res.txt
