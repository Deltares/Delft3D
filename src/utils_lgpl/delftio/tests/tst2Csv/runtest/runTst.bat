@echo off

del *.csv >del.log 2>&1
del del.log

..\x64\%1\tstHis2csv.exe tstHis2Csv.his all


rem fc TESTDioConf-res.txt ..\resultsApproved\w32\tstHis2Csv.csv

