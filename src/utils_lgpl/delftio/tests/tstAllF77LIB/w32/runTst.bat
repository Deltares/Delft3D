rem @echo off

del TEST*.*

..\bin\%1\tstallf77-put.exe
..\bin\%1\tstallf77-get.exe
..\bin\%1\tstallf77-map.exe
..\bin\%1\tstallf77-endTime.exe


fc TESTF77PUTLIB-res.txt ..\resultsApproved\w32\TESTF77PUTLIB-res.txt
fc TESTF77LIB-res.txt ..\resultsApproved\w32\TESTF77LIB-res.txt
fc TESTF77Map-res.txt ..\resultsApproved\w32\TESTF77Map-res.txt
fc TESTEndTime-res.txt ..\resultsApproved\w32\TESTEndTime-res.txt

exit /B 0
