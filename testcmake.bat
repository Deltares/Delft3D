"C:\Program Files\CMake\bin\cmake.exe" ./src/cmake -D CMAKE_BUILD_TYPE=Debug -D CONFIGURATION_TYPE:STRING=delft3d4 -B build_test -D CMAKE_INSTALL_PREFIX=.\build_test\x64\Debug\
 
cd build_test
"C:\Program Files\CMake\bin\cmake.exe" --build . --target install -j --config Debug > out.txt
cd ..