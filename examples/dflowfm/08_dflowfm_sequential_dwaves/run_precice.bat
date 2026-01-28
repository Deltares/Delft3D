@ echo off

setlocal EnableExtensions EnableDelayedExpansion
set startFM=1
set startWave=1

rem set bindir=c:\checkouts\gitlab\delft3d\test\deltares_testbench\data\engines\teamcity_artifacts\x64\bin
set bindir=c:\checkouts\github\delft3d\install_fm-suite\bin
rem set bindir=c:\adri\work\delft3d\2.28.28_2025.02\x64\bin

set libdir=%bindir%\..\lib
set PATH=%bindir%;%PATH%;%libdir%

if %startFM% EQU 1 (
   cd dflowfm
   start %bindir%\dflowfm-cli.exe f34.mdu
   cd ..
)

if %startWave% EQU 1 (
   cd dwaves
   start %bindir%\wave.exe f34.mdw 1
   cd ..
)

rem pause
