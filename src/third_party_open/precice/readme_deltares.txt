# Build
Clone the sources from the preCICE GitHub repository. Currently, we use version 3.3.1. For windows, one change needs to be made to the cmake of preCICE:
the install rules of preCICE in its main CMakeLists.txt should contain "RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}":
    install(TARGETS precice
            EXPORT preciceTargets
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ...
This change was already made in later development versions of preCICE.

Build preCICE within an intel oneAPI command prompt for Intel 64 and Visual Studio 2022 (for impi support):

    set CXXFLAGS=/permissive-
    set FFLAGS=/names:lowercase /assume:underscore
    cmake -S . -B build -G "Ninja" -D CMAKE_CXX_COMPILER=cl -D CMAKE_PREFIX_PATH=<THIRD_PARTY_PATH>\eigen\eigen-5.0.1\share\eigen3\cmake;<THIRD_PARTY_PATH>\libxml2\libxml2-v2.15.1\lib\cmake\libxml2;<THIRD_PARTY_PATH>\boost\boost_1_90_0\lib\cmake\Boost-1.90.0;<THIRD_PARTY_PATH>\petsc\petsc-3.21.3 -D CMAKE_INSTALL_PREFIX=install -D CMAKE_BUILD_TYPE=Release -D BUILD_SHARED_LIBS=ON -D BUILD_TESTING=OFF -D PRECICE_FEATURE_MPI_COMMUNICATION=ON -D PRECICE_FEATURE_PYTHON_ACTIONS=OFF -D PRECICE_FEATURE_GINKGO_MAPPING=OFF -D PRECICE_FEATURE_PETSC_MAPPING=OFF -D CMAKE_FIND_PACKAGE_PREFER_CONFIG=ON
    cmake --build build --config Release --parallel
    cmake --install build --config Release

Copy the artifacts from the install folder to the appropriate third-party folder in the Delft3D repository.
