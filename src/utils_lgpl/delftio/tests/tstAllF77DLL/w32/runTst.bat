rem @echo off

del TEST*.*

..\bin\%1\AllF77Put.exe
..\bin\%1\AllF77Get.exe
..\bin\%1\tstF77Map.exe
..\bin\%1\tstF77EndTime.exe


fc TESTF77PUTLIB-res.txt ..\resultsApproved\w32\TESTF77PUTLIB-res.txt
fc TESTF77LIB-res.txt ..\resultsApproved\w32\TESTF77LIB-res.txt
fc TESTF77Map-res.txt ..\resultsApproved\w32\TESTF77Map-res.txt
fc TESTEndTime-res.txt ..\resultsApproved\w32\TESTEndTime-res.txt

exit /B 0
