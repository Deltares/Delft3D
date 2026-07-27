# Build
Download sources (this version: 5.0.1).

Build steps:
    cmake -S . -B build -G "Ninja" -D CMAKE_C_COMPILER=cl -D CMAKE_CXX_COMPILER=cl -D CMAKE_INSTALL_PREFIX=<PREFIX>\src\third_party_open\eigen\eigen-5.0.1\ -D EIGEN_BUILD_TESTING=OFF -D EIGEN_BUILD_BLAS=OFF -D EIGEN_BUILD_LAPACK=OFF -D EIGEN_BUILD_DOC=OFF -D EIGEN_BUILD_DEMOS=OFF
    cmake --install build

There is no cmake --build step, since it is a header only library.
