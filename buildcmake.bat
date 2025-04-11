cmake -S .\src\cmake -B build_all -A x64 -D CONFIGURATION_TYPE="all"
rem -D CMAKE_INSTALL_PREFIX=c:\Program Files\Deltares\Delft3D FM Suite 2025.02 1D2D (b3ba8af-7211)\plugins\DeltaShell.Dimr\kernels\x64\
pause
rem python .\src\convert_vfproj_to_use_ifx.py ifx build_delft3d4

rem cd build_all
rem cmake --build . -j --target install --config Debug  