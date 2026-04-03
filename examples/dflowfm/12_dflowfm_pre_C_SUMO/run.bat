@ echo off
set bindir=c:\checkouts\github\Delft3D\install_fm-suite\bin
set libdir=%bindir%\..\lib
set PATH=%bindir%;%libdir%;%PATH%

del /f /q fm\DFM_OUTPUT_FlowFM\*.*
del /f /q cosumo\FF2NF\*.xml
del /f /q cosumo\csumo_bmi.dia
del /f /q csumo_to_dflowfm.nc

rem call c:\adri\work\delft3d\2026.01\x64\bin\run_dimr.bat
%bindir%\preC-SUMO.exe -c cosumo\CSUMOsettings.xml -a precice_config.xml


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
