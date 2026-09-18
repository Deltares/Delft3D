This is the pugixml library for reading and writing XML files in C++.
# How to build:
- Download the sources (version 1.15) from https://pugixml.org/.
- cmake -S . -B build -D CMAKE_INSTALL_PREFIX=.\install\ -D BUILD_SHARED_LIBS=ON
- cmake --build build --config Release --parallel
- cmake --install build
- Copy the contents of the install folder here
