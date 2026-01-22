# Build:
Download sources (this version: v2.15.1)

Build steps from source folder:
    cmake -S . -B build -G "Ninja" -D CMAKE_C_COMPILER=cl -D CMAKE_CXX_COMPILER=cl -D CMAKE_INSTALL_PREFIX=<PREFIX>\src\third_party_open\libxml2\libxml2-v2.15.1 -D LIBXML2_WITH_ICONV=OFF -D LIBXML2_WITH_LZMA=OFF -D LIBXML2_WITH_PYTHON=OFF
    cmake --build build --config Release --parallel
    cmake --install build --config Release

# Use:
Point CMake to src\third_party_open\libxml2\libxml2-v2.15.1\lib\cmake\libxml2, where the libxml2-config.cmake lives. Then find_package(libxml2 CONFIG) should work.
