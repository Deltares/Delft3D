@ echo off

set usePreCICE=1
set startFM=1
set startPreCSUMO=1

set bindirNF=..\..\..\..\install_fm-suite\bin
set libdirNF=%bindirNF%\..\lib
set bindirFF=..\..\..\..\install_fm-suite\bin
set libdirFF=%bindirFF%\..\lib

del /f /q fm\DFM_OUTPUT_FlowFM\*.*
del /f /q cosumo\FF2NF\*.xml
del /f /q cosumo\csumo_bmi.dia
del /f /q csumo_to_dflowfm.nc
del /f /q precice_debug_output.txt
del /f /q precice-profiling\*.*
rmdir /s /q precice-run



if %usePreCICE% EQU 1 (
    if %startPreCSUMO% EQU 1 (
        cd cosumo
        set PATH=%bindirNF%;%libdirNF%
        start %bindirNF%\preC-SUMO.exe -c CSUMOsettings.xml -p ..\precice_config.xml
        cd ..
    ) else (
        echo Please start preC-SUMO
    )
    
    if %startFM% EQU 1 (
        cd fm
        set PATH=%bindirFF%;%libdirFF%
        call %bindirFF%\run_dflowfm.bat FlowFM.mdu --precice
        cd ..
    ) else (
        echo Please start D-Flow FM
    )
) else (
    call %bindirFF%\run_dimr.bat
)

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
