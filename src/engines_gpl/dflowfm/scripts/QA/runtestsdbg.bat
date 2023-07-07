@echo off
SETLOCAL ENABLEDELAYEDEXPANSION

set origdir=%CD%

cd %~dp0..\..\

set fm_home=%CD%

rem extend path for D-Flow (FM)
set path=%fm_home%\third_party_open\netcdf\lib\win32\x64\Release\;%path%

rem Run the testbench.
set caselist=^
f01_1D\c081_EINDHOVEN_1D                                    EINDH.mdu ^
f02_1D2D\c080_Kanalen                                       kanaal1D2D.mdu ^
f02_1D2D\c087_Manholes                                      manholes_1d2d.mdu ^
f03_advection\c024_Thacker_2D                               thacker2d.mdu ^
f03_advection\c029_Tidalbore_1D                             bore4.mdu ^
f04_bottomfriction\c010_Chezy                               chezy.mdu ^
f04_bottomfriction\c017_QZbnd_simple_channel_flow           10000.mdu ^
f05_boundary_conditions\c052_Supercritical_inflow_boundary  supercritinflow.mdu ^
f08_network\c021_Curvi_griddistortion                       curvidistortion.mdu ^
f08_network\c020_Channel_with_refinement                    simplechannel.mdu ^
f09_structures\c070_Flow_2D_over_a_weir                     weir.mdu ^
f09_structures\c085_Pump_1D2D                               pump.mdu ^
f09_structures\c087_Weir_1D                                 weir1.mdu ^
f09_structures\c088_Gate_1D                                 gate1.mdu ^
f09_structures\c089_ships_2D                                ship.mdu ^
f09_structures\c090_damclosure_2D                           damclosure.mdu ^
f10_transport\c060_Channel1D                                transport1d.mdu

set casedir=
set casefile=
set skipcase=
for %%c in (%caselist%) do (
    if "!skipcase!" == "1" (
rem     skipcase was set during previous 'casedir' iteration step. Reset current 'casefile' step and proceed with next tuple in caselist.
        set casedir=
        set skipcase=
    ) else if "!casedir!" == "" (
        cd %fm_home%
        set casedir=testcases\e00_unstruc\%%c\input
        if EXIST !casedir!\nul (
            echo *
            echo * Entering case dir !casedir!...
            cd "!casedir!"
        ) else (
            echo ERROR: non-existent case dir !casedir!...
            echo Skipping this case!
            set skipcase=1
        )
    ) else (
        set casefile=%%c
        if NOT EXIST !casefile! (
            echo ** ERROR: Case file !casefile! not found. Skipping this case.
        ) else (
            echo ** Start running !casefile!...
            copy /Y %fm_home%\res\*.* . >> copy.log
            echo ** %fm_home%\bin\x64\Debug\unstruc-x64.exe !casefile! -autostartstop
            call %fm_home%\bin\x64\Debug\unstruc-x64.exe !casefile! -autostartstop
        )
        set casedir=
    )
)

cd %fm_home%
