rem @echo off

del TEST*.*

..\bin\%1\tstmap-get.exe
..\bin\%1\tstmap-long-get.exe

fc TESTMap-res.txt ..\resultsApproved\w32\TESTMap-res.txt
fc TESTMapLong-res.txt ..\resultsApproved\w32\TESTMapLong-res.txt
fc TESTMapLong.map ..\resultsApproved\w32\TESTMapLong.map
fc TESTMapLong.maa ..\resultsApproved\w32\TESTMapLong.maa

exit /B 0
