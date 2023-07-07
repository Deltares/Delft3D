rem @echo off

del TEST*.*

..\bin\%1\tstNefis2DPut.exe
..\bin\%1\tstNefis2DGet.exe


fc TESTNefis2D-res.txt ..\resultsApproved\w32\TESTNefis2D-res.txt
fc TESTNefis.dat ..\resultsApproved\w32\TESTNefis.dat
fc TESTNefis.def ..\resultsApproved\w32\TESTNefis.def

exit /B 0
