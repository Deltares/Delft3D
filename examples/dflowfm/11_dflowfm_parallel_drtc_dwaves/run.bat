@ echo off

    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\bin\smpd.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\bin
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: smpd -uninstall

set dimrdir=\\directory.intra\PROJECT\d-hydro\dimrset\latest

cd dflowfm
set PATH=%dimrdir%\x64\bin;%dimrdir%\x64\lib;%PATH%
call %dimrdir%\x64\bin\dflowfm-cli.exe --partition:ndomains=3:icgsolver=6 weirtimeseries.mdu
cd ..

call %dimrdir%\x64\bin\run_dimr_parallel.bat 3 dimr_config.xml


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
