# Configure proj build options.
set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
set(ENABLE_TIFF OFF)
set(ENABLE_CURL OFF)
set(BUILD_APPS OFF)
if(WIN32)
    set(SQLITE3_INCLUDE_DIR ${checkout_src_root}/third_party_open/sqlite3/sqlite-3.44.0/include)
    set(SQLITE3_LIBRARY ${checkout_src_root}/third_party_open/sqlite3/sqlite-3.44.0/lib/sqlite3.lib)
    set(EXE_SQLITE3 ${checkout_src_root}/third_party_open/sqlite3/sqlite3-3.44.0/bin/sqlite3.exe)
endif(WIN32)

# Disable BUILD_SHARED_LIBS and BUILD_TESTING before proj configuration. Restore them after.
set(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT OFF)
set(SAVED_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})
set(BUILD_SHARED_LIBS OFF)
set(SAVED_BUILD_TESTING ${BUILD_TESTING})
set(BUILD_TESTING OFF)

set(CMAKE_FOLDER "third_party_open/proj") # Adds proj related projects under the 'proj' directory in VS sln file.
add_subdirectory(${proj_module} EXCLUDE_FROM_ALL proj)
unset(CMAKE_FOLDER)

# Restore BUILD_TESTING and BUILD_SHARED_LIBS
set(BUILD_TESTING ${SAVED_BUILD_TESTING})
unset(SAVED_BUILD_TESTING)
set(BUILD_SHARED_LIBS ${SAVED_BUILD_SHARED_LIBS})
unset(SAVED_BUILD_SHARED_LIBS)