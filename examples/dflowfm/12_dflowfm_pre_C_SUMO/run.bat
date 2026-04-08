@ echo off

set usePreCICE=1
set startFM=0

set bindirNF=c:\checkouts\github\Delft3D\install_fm-suite\bin
set libdirNF=%bindirNF%\..\lib
set bindirFF=c:\adri\work\git_readonly\Delft3D\install_fm-suite\bin
set libdirFF=%bindirFF%\..\lib

del /f /q fm\DFM_OUTPUT_FlowFM\*.*
del /f /q cosumo\FF2NF\*.xml
del /f /q cosumo\csumo_bmi.dia
del /f /q csumo_to_dflowfm.nc
del /f /q precice_debug_output.txt
del /f /q precice-profiling\*.*
rmdir /s /q precice-run



if %usePreCICE% EQU 1 (

    rem call c:\adri\work\delft3d\2026.01\x64\bin\run_dimr.bat
    set PATH=%bindirNF%;%libdirNF%
    start %bindirNF%\preC-SUMO.exe -c cosumo\CSUMOsettings.xml -p precice_config.xml
    if %startFM% EQU 1 (
        cd fm
        set PATH=%bindirFF%;%libdirFF%
        call %bindirFF%\run_dflowfm.bat FlowFM.mdu --precice
    ) else (
        echo Please start D-Flow FM
    )

) else (
    call %bindirFF%\run_dimr.bat
)

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
