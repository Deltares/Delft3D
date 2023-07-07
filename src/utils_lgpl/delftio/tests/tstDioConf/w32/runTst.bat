rem @echo off

del TEST*.*

..\bin\%1\tstdio-conf.exe


fc TESTDioConf-res.txt ..\resultsApproved\w32\TESTDioConf-res.txt

exit /B 0
