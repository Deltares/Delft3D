@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW 6.00 (Windows)
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 30 oct 2013
    rem 


    rem
    rem Set version numbers here
    rem example:
    
    rem
    rem Set the config file here
    rem 
set argfile=config_d_hydro.xml





    rem
    rem Set the directory containing d_hydro.exe here
    rem
echo When error message "The system cannot find the path specified." appears below:
echo   Check "ARCH" in the run-script:
echo     Version 6.01.17.5275 and older: default ARCH=win32
echo     Version 6.01.18.5368 and newer: default ARCH=win64
set ARCH=win64
set D3D_HOME=..\..\..\bin
set exedir=%D3D_HOME%\%ARCH%\flow2d3d\bin



copy corinp_win.dat corinp.dat
del /f trim*.* >del.log 2>&1
del /f tri-diag.* >del.log 2>&1
del /f TMP_*.* >del.log 2>&1
del /f tstprt.* >del.log 2>&1
del /f COSUMO\FF2NF\FF2NF*.txt >del.log 2>&1
del /f del.log


    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set PATH=%exedir%;%PATH%

    rem Run
%exedir%\d_hydro.exe %argfile%


:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
