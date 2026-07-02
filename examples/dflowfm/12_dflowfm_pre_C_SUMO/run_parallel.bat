@ echo off

set usePreCICE=1
set startFM=1
set startPreCSUMO=1
set NPROC=2

set bindir=..\..\..\..\install_fm-suite\bin
set libdir=%bindir%\..\lib

del /f /q fm\DFM_OUTPUT_FlowFM\*.*
del /f /q fm\2dis_*_net.nc
del /f /q fm\DFM_interpreted_idomain_2dis_net.nc
del /f /q fm\FlowFM_*.mdu
del /f /q fm\*.dia
del /f /q fm\precice-exports\*.*
del /f /q fm\precice-profiling\*.txt
del /f /q cosumo\FF2NF\*.xml
del /f /q cosumo\csumo_bmi.dia
del /f /q cosumo\precice-exports\*.*
del /f /q cosumo\precice-profiling\*.txt
del /f /q cosumo\precice_debug_output.txt
del /f /q csumo_to_dflowfm.nc
del /f /q precice_debug_output.txt
del /f /q precice-profiling\*.*
rmdir /s /q precice-run



if %usePreCICE% EQU 1 (
    if %startPreCSUMO% EQU 1 (
        cd cosumo
        set PATH=%bindir%;%libdir%
        start %bindir%\preC-SUMO.exe -c csumo_settings.xml -p ..\precice_config.xml
        cd ..
    ) else (
        echo Please start preC-SUMO
    )
    
    if %startFM% EQU 1 (
        cd fm
        set PATH=%bindir%;%libdir%
        call %bindir%\dflowfm-cli.exe --partition:ndomains=%NPROC%:icgsolver=6 FlowFM.mdu
        call mpiexec -n %NPROC% %bindir%\dflowfm-cli.exe FlowFM.mdu --precice
        cd ..
    ) else (
        echo Please start D-Flow FM
    )
) else (
    call %bindir%\run_dimr.bat
)

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
