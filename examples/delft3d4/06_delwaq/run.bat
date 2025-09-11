@ echo off

rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
rem build.bat all
rem See README.md there for more information

set build_configuration=install_waq
set script_path=..\..\..\%build_configuration%\bin
call %script_path%\run_delwaq.bat com-tut_fti_waq.inp


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
