@echo off
echo "extracting values from test data files"
..\release\his2csv.exe qlat.his
..\release\his2csv.exe qlat.his Lateral l_1, l_3_1
..\release\his2csv.exe calcpnt.his "Total area (m2)" 3_4 1_2
..\release\his2csv.exe reachseg.his "Discharge"

echo === comparing results with validated expected values ===
fc "calcpnt-Total area (m2).csv" ".\expected\calcpnt-Total area (m2).csv"
fc qlat.csv .\expected\qlat.csv
fc qlat-Lateral.csv .\expected\qlat-Lateral.csv
fc reachseg-Discharge.csv .\expected\reachseg-Discharge.csv
echo === comparing results with validated expected values done ===
