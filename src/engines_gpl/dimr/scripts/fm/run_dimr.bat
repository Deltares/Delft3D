@ echo off
title run_dhydro
    rem
    rem This script is an example for running dimr on Windows
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 24 june 2015
    rem 
    rem
setlocal enabledelayedexpansion


    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set argfile=%1
)
echo configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

    rem
    rem Set the directories containing the binaries
    rem
set ARCH=win64
set D3D_HOME=p:\h6\opt\delft3d\research\fmtrunk47986_wavebranch6681_dimrtrunk6639
set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set waveexedir=%D3D_HOME%\%ARCH%\wave\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set esmfexedir=%D3D_HOME%\%ARCH%\esmf\bin
set esmfbatdir=%D3D_HOME%\%ARCH%\esmf\scripts


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dimrexedir%;%dflowfmexedir%;%waveexedir%;%swanbatdir%;%swanexedir%;%esmfbatdir%;%esmfexedir%
    rem With debug info: "%dhydroexedir%\d_hydro.exe" -d 0xFFFFFFFF %argfile%
"%dimrexedir%\dimr.exe" %argfile%

goto end

:usage
echo Usage:
echo run_dimr.bat [--help] [config.xml]
echo     --help    : (Optional) show this usage
echo     config.xml: (Optional) default: d_hydro_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
