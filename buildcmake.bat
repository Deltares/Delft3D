cmake ./src/cmake -D CMAKE_BUILD_TYPE=Debug -T fortran=ifort -D CONFIGURATION_TYPE:STRING=all -B build_all -D CMAKE_INSTALL_PREFIX=D:\checkouts\Trunk_git\test\deltares_testbench\data\engines\teamcity_artifacts\x64\
pause
rem python .\src\convert_vfproj_to_use_ifx.py ifx build_delft3d4

rem cd build_all
rem cmake --build . -j --target install --config Debug  