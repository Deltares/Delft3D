rem @echo off

del TEST*.*

..\bin\%1\tstrewind.exe

type TESTRewind1.his > TESTRewindTot.his
type TESTRewind2.his >> TESTRewindTot.his

..\bin\%1\tstrewind-append-get.exe

fc TESTRewindTot.his ..\resultsApproved\w32\TESTRewindTot.his
fc TESTRewind-res.txt ..\..\..\resultsApproved\w32\TESTRewind-res.txt

exit /B 0
