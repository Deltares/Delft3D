@echo off

del TEST*.* >del.log 2>&1
del del.log

start ..\x64\%1\tstall2d-put.exe
..\x64\%1\tstall2d-get.exe


fc TEST2DFSerial-res.txt ..\resultsApproved\w32\TEST2DFSerial-res.txt
fc TEST2DFSynch-res.txt ..\resultsApproved\w32\TEST2DFSynch-res.txt
fc TEST2DFAutoFiles-res.txt ..\resultsApproved\w32\TEST2DFAutoFiles-res.txt
fc TEST2DFAutoShm-res.txt ..\resultsApproved\w32\TEST2DFAutoShm-res.txt

