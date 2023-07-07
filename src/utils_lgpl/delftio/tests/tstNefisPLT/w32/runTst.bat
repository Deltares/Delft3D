rem @echo off

del TEST*.*

..\bin\%1\tstNefisPLTPut.exe
..\bin\%1\tstNefisPLTGet.exe


fc TESTNefisPLT-res.txt ..\resultsApproved\w32\TESTNefisPLT-res.txt
fc TESTNefis.dat ..\resultsApproved\w32\TESTNefis.dat
fc TESTNefis.def ..\resultsApproved\w32\TESTNefis.def

exit /B 0
