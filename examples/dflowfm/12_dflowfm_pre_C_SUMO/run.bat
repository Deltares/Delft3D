@ echo off

del /f /q fm\DFM_OUTPUT_FlowFM\*.*
del /f /q cosumo\FF2NF\*.*
del /f /q cosumo\csumo_bmi.dia
del /f /q csumo_to_dflowfm.nc

call c:\adri\work\delft3d\2026.01\x64\bin\run_dimr.bat

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
