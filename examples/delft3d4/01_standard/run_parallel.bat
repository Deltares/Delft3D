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




    rem This example testcase is that small, that it can run with a maximum of 5 partitions
set NPROC=%NUMBER_OF_PROCESSORS%
if %NPROC% gtr 5 set NPROC=5

    rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
    rem build.bat all
    rem See README.md there for more information

set build_configuration=build_delft3d4
set script_path=..\..\..\%build_configuration%\x64\dflow2d3d\scripts
call %script_path%\run_dflow2d3d_parallel.bat %NPROC%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
