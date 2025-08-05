# Sets the location of unix based script files
set(CMAKE_INSTALL_RPATH "$ORIGIN:$ORIGIN/../lib")

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH  FALSE)

# When building, don't use the "install" RPATH yet. During the install phase,
# the binaries will be updated to use the "install" RPATH.
# Setting this option to "FALSE" ensures that, for example, the unit test
# executables that depend on libraries in the build folder can run without
# requiring an install and/or changing LD_LIBRARY_PATH.
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath='$ORIGIN:$ORIGIN/../lib' -Wl,--enable-new-dtags")

