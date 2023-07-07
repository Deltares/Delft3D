# Sets the location of unix based script files
set(update_version_script scripts_lgpl/linux/update_version.sh)
set(postbuild_event_script scripts_lgpl/linux/cmake_postbuild.sh)
set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install)
set(CMAKE_INSTALL_RPATH "$ORIGIN:$ORIGIN/../lib")

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath='$ORIGIN:$ORIGIN/../lib' -Wl,--enable-new-dtags")

