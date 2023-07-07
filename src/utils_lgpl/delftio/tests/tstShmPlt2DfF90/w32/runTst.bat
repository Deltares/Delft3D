rem @echo off

del TEST*.*

start ..\bin\%1\tstshm-plt-2df-put.exe
..\bin\%1\tstshm-plt-2df-get.exe

..\bin\%1\tstshm-plt-2df-inmem.exe


fc TESTShmPlt2df-res.txt ..\resultsApproved\w32\TESTShmPlt2df-res.txt
fc TESTShmPlt2dfInmem-res.txt ..\resultsApproved\w32\TESTShmPlt2df-res.txt

exit /B 0
