rem @echo off

del TEST*.* dio-*-errors.txt

..\bin\%1\tstRestartPut.exe
..\bin\%1\tstRestartGet.exe
..\bin\%1\tstTriton.exe


fc TESTNefisRestart-res.txt ..\resultsApproved\w32\TESTNefisRestart-res.txt
fc TESTRestart.dat ..\resultsApproved\w32\TESTRestart.dat
fc TESTRestart.def ..\resultsApproved\w32\TESTRestart.def

fc TESTTriton-res.txt ..\resultsApproved\w32\TESTTriton-res.txt
fc TESTTritonRestart.def ..\resultsApproved\w32\TESTTritonRestart.def
fc TESTTritonRestart.dat ..\resultsApproved\w32\TESTTritonRestart.dat

exit /B 0
