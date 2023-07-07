rem @echo off

del TEST*.*

..\bin\%1\tsthia.exe
..\bin\%1\tsthis-long.exe
..\bin\%1\tstdescr.exe

fc TESTcalcpnt1-res.txt ..\resultsApproved\w32\TESTcalcpnt1-res.txt
fc TESTcalcpnt2-res.txt ..\resultsApproved\w32\TESTcalcpnt2-res.txt
fc TESTcalcpnt3-res.txt ..\resultsApproved\w32\TESTcalcpnt3-res.txt

fc TESTHisLong-res.txt ..\resultsApproved\w32\TESTHisLong-res.txt
fc TESTHisLong.his ..\resultsApproved\w32\TESTHisLong.his
fc TESTHisLong.hia ..\resultsApproved\w32\TESTHisLong.hia

fc TESTDescr-res.txt ..\resultsApproved\w32\TESTDescr-res.txt
fc TESTLocDesc.hia ..\resultsApproved\w32\TESTLocDesc.hia
fc TESTLocDesc.his ..\resultsApproved\w32\TESTLocDesc.his
fc TESTParAndLocWithIntIds.hia ..\resultsApproved\w32\TESTParAndLocWithIntIds.hia
fc TESTParAndLocWithIntIds.his ..\resultsApproved\w32\TESTParAndLocWithIntIds.his

exit /B 0
