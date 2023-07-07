rem @echo off

del TEST*.*

start ..\bin\%1\tstallf77-put.exe
..\bin\%1\tstallf77-get.exe


fc TESTF77Serial-res.txt ..\resultsApproved\w32\TESTF77Serial-res.txt
fc TESTF77Synch-res.txt ..\resultsApproved\w32\TESTF77Synch-res.txt

exit /B 0
