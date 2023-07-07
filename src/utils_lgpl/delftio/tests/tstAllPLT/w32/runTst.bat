rem @echo off

del TEST*.*

start ..\bin\%1\tstallplt-put.exe
..\bin\%1\tstallplt-get.exe

fc TESTPltSerial-res.txt ..\resultsApproved\w32\TESTPltSerial-res.txt
fc TESTPltSynch-res.txt ..\resultsApproved\w32\TESTPltSynch-res.txt
fc TESTPltAutoFiles-res.txt ..\resultsApproved\w32\TESTPltAutoFiles-res.txt
fc TESTPltAutoShm-res.txt ..\resultsApproved\w32\TESTPltAutoShm-res.txt

exit /B 0
