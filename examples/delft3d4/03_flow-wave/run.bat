@ echo off

rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
rem build.bat all
rem See README.md there for more information

set build_configuration=build_delft3d4
set script_path=..\..\..\%build_configuration%\x64\dflow2d3d\scripts
call %script_path%\run_dflow2d3d_dwaves.bat -w r17.mdw


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
