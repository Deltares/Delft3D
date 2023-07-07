@ echo off

rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
rem build.bat all
rem See README.md there for more information

set build_configuration=build_all

rem The following path should match the folder where the dimr scripts have been placed after your compilation step
rem In case you built using the build.bat (configure AND build step), then the scripts will be placed in the following folder:
set script_path=..\..\..\..\..\%build_configuration%\x64\dimr\scripts
rem In case you only configured using the build.bat and built yourself using Visual Studio, then the scripts will be placed in the following folder:
rem set script_path=..\..\..\..\..\%build_configuration%\x64\Release\dimr\scripts

call %script_path%\run_dimr.bat

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
