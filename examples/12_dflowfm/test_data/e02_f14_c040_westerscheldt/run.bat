@ echo off

    rem When using intelMPI for the first time on a machine:
    rem Execute "hydra_service.exe -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\hydra_service.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Windows Start button" -> type "cmd", right-click "Command Prompt" App, "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin (or your local copy)
    rem         hydra_service.exe -install
    rem         mpiexec.exe -register -username <user> -password <password> -noprompt
    rem     When there is an hydra_service/smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: hydra_service.exe -uninstall (smpd -uninstall)

rem call ..\..\..\..\src\bin\x64\dflowfm\scripts\run_dflowfm.bat "--partition:ndomains=3:icgsolver=6" westerscheldt.mdu


rem call ..\..\..\..\src\bin\x64\dimr\scripts\run_dimr_parallel.bat 3 dimr_config.xml

rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
rem build.bat all
rem See README.md there for more information

set build_configuration=build_all

rem The following path should match the folder where the dimr scripts have been placed after your compilation step
rem In case you built using the build.bat (configure AND build step), then the scripts will be placed in the following folder:
set script_path=..\..\..\..\%build_configuration%\x64\dimr\scripts
rem In case you only configured using the build.bat and built yourself using Visual Studio, then the scripts will be placed in the following folder:
rem set script_path=..\..\..\..\%build_configuration%\x64\Release\dimr\scripts

call %script_path%\run_dimr_parallel.bat 3 dimr_config.xml

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
