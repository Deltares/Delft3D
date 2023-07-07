rem @echo off

del TEST*.*

..\bin\%1\tstmany-put.exe
..\bin\%1\tstmany-get.exe

fc TESTMany-res.txt ..\resultsApproved\w32\TESTMany-res.txt

pause

exit /B 0
