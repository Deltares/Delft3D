Source files can be found at
preCICE: https://github.com/precice/precice
Fortran module: https://github.com/precice/fortran-module

The source code was built for windows using msys2 (ucrt64) and mingw:
1) Install preCICE's dependencies using msys2 pacman
2) Patch preCICE's CMakeLists.txt install rules to add the dll by specifying the RUNTIME DESTINATION:
    install(TARGETS precice
            EXPORT preciceTargets
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/precice
3) Build and install using CMake:
    cmake -S . -B build -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=install -D PRECICE_FEATURE_MPI_COMMUNICATION=OFF -D PRECICE_FEATURE_PYTHON_ACTIONS=OFF -D PRECICE_FEATURE_GINKGO_MAPPING=ON -D PRECICE_FEATURE_PETSC_MAPPING=OFF -D BUILD_TESTING=OFF -D Boost_USE_STATIC_LIBS=OFF -D BUILD_SHARED_LIBS=ON
    cmake --build build --config Release
    cmake --install build --config Release
4) Copy preCICE's dependencies to a folder in msys2:
    ntldd --recursive install/bin/libprecice.dll | grep ucrt64 | awk '{print $3}' | sed 's|C:\\msys64\\|/|; s|\\|/|g' | xargs -I {} cp {} ./install/dependencies/bin
5) Copy libprecice.dll and all its dependencies to the src/third_party_open/precice/bin folder in the Delft3D repo
6) Copy libprecice.dll.a to the src/third_party_open/precice/lib folder in the Delft3D repo

