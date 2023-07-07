rem @echo off

del TEST*.*

start ..\bin\%1\tstHis-Put.exe
..\bin\%1\tstHis-Get.exe

..\bin\%1\tstHis-Copy.exe

..\bin\%1\tstHis-Put-Time.exe
..\bin\%1\tstHis-Get-Time.exe

..\bin\%1\tstHis-Put-Double.exe

..\bin\%1\tstHis-Put-1D.exe
..\bin\%1\tstHis-Get-HisStep.exe
ren TESTHisHisstep-res.txt TESTHis1D-res.txt

..\bin\%1\tstHis-Put-HisStep.exe
..\bin\%1\tstHis-Get-HisStep.exe

..\bin\%1\tstHis-Get-Selection.exe

fc TESTHisData.asc ..\resultsApproved\w32\TESTHisData.asc
fc TESTAscii-res.txt ..\resultsApproved\w32\TESTAscii-res.txt
fc TESTHisData.his ..\resultsApproved\w32\TESTHisData.his
fc TESTHis-res.txt ..\resultsApproved\w32\TESTHis-res.txt
fc TESTShm-res.txt ..\resultsApproved\w32\TESTShm-res.txt

fc TESTHis_Copy.his ..\resultsApproved\w32\TESTHis_Copy.his
fc TESTHisCopy-res.txt ..\resultsApproved\w32\TESTHisCopy-res.txt

fc TESTHisTimestep.his ..\resultsApproved\w32\TESTHisTimestep.his
fc TESTHisTimestep-res.txt ..\resultsApproved\w32\TESTHisTimestep-res.txt

fc TESTHisDouble.his ..\resultsApproved\w32\TESTHisDouble.his

fc TESTHisHisstep-res.txt ..\resultsApproved\w32\TESTHisHisstep-res.txt
fc TESTHis1D-res.txt ..\resultsApproved\w32\TESTHisHisstep-res.txt

fc TESTHisSelection-res.txt ..\resultsApproved\w32\TESTHisSelection-res.txt

pause

exit /B 0
