@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW 6.00 (Windows) parallel
    rem Adapt and use it for your own purpose
    rem
    rem edwin.spee@deltares.nl
    rem adri.mourits@deltares.nl
    rem 30 oct 2013
    rem 
    rem
    rem !!!!! IMPORTANT !!!!!
    rem When using mpich2 for the first time on a machine:
    rem Execute AS ADMINISTRATOR "smpd -install"
    rem smpd.exe is in directory "...\win32\flow2d3d\bin"
    rem 




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

    rem
    rem No adaptions needed below
    rem



copy corinp_win.dat corinp.dat
del /f trim*.* >del.log 2>&1
del /f trih*.* >del.log 2>&1
del /f tri-diag.* >del.log 2>&1
del /f TMP_*.* >del.log 2>&1
del /f tstprt.* >del.log 2>&1
del /f COSUMO\FF2NF\FF2NF*.txt >del.log 2>&1
del /f debug.txt >del.log 2>&1
del /f *.ddb >del.log 2>&1
del /f del.log


    rem Set some (environment) parameters
set PATH=%exedir%;%PATH%
    rem mpiexec is in %exedir%
    rem For some users, it is necessary to use the locally installed mpiexec:
set MPIPATH=%exedir%
    rem set MPIPATH="C:\Program Files (x86)\Common Files\Intel\Shared Libraries\redist\intel64\mpirt"


    rem Run
    rem start computation on all local cores (2 for dual core; 4 for quad core etc.)
    rem note the flag "-localonly" which may be needed to avoid "Aborting: unable to connect to machinename.local"
%MPIPATH%\mpiexec -n 2 -localonly %exedir%\d_hydro.exe %argfile%


:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
