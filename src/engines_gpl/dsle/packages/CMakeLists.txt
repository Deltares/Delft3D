if(WIN32)

cmake_minimum_required(VERSION 3.15)

project(libdsle)
enable_language(C)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_BINARY_DIR})

##############################################################################
################################## Version ###################################
##############################################################################
# For systems with git installed, find out revision and description.
execute_process(COMMAND git rev-parse HEAD
  OUTPUT_VARIABLE git_revision
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)
execute_process(COMMAND git describe --tags --first-parent HEAD
  OUTPUT_VARIABLE git_describe
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)

string(REPLACE "-g" ".g" git_describe "${git_describe}")
string(REPLACE "-dirty" ".dirty" git_describe "${git_describe}")
string(REPLACE "-" "+" git_describe "${git_describe}")
set(PACKAGE_VERSION_FULL "${git_describe}")

configure_file(config.h.in config.h ESCAPE_QUOTES)
#file(WRITE "wrappers/git_describe.txt" "${git_describe}")

##############################################################################
################################## Options ###################################
##############################################################################
# Default build type is Release
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type (default Release)" FORCE)
endif()

option(USE_FAST_MATH "Enable fast math optimizations" OFF)
if(USE_FAST_MATH)
    if (MSVC)
        add_compile_options(/fp:fast)
    elseif((CMAKE_C_COMPILER_ID MATCHES "Clang") OR (CMAKE_C_COMPILER_ID MATCHES "GNU"))
        add_compile_options(-ffast-math)
    endif()
else()
    if (MSVC)
        add_compile_options(/fp:precise)
    endif()
endif()

option(USE_FAST_TANH "Enable fast tanh approximation" OFF)
if(USE_FAST_TANH)
    add_definitions(-DDSLE_USE_FAST_TANH)
endif()

set(DSLE_SOURCE_FILES
    include/dsle.h
    src/dsle.c
    src/dimr_bmi.c
    src/dimr_bmi.h
    src/io_layer_distribution.c
    src/io_layer_distribution.h
    src/csv/load_csv.c
    src/csv/load_csv.h
    src/load_time_averaged.h
    src/load_time_averaged.c
    src/load_phase_wise.h
    src/load_phase_wise.c
    src/sealock.h
    src/sealock.c
    src/timestamp.h
    src/timestamp.c
    src/ini/ini_read.h
    src/ini/ini_read.c
    src/dsle_config.h
    src/dsle_config.c
    src/log/log.h
    src/log/log.c
)

##############################################################################
################################## Targets ###################################
##############################################################################
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
#Version includes
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_version_file ${CMAKE_CURRENT_SOURCE_DIR}/../version/dsle_dll_version.rc)
include_directories(${version_include_dir})
add_library(dsle SHARED ${DSLE_SOURCE_FILES} ${rc_version_file})

set_target_properties (dsle PROPERTIES
    DEFINE_SYMBOL "DSLE_EXPORTS"
    OUTPUT_NAME "dsle"
    PUBLIC_HEADER "include/dsle.h"
    C_STANDARD 23
)

add_library(dsle-static STATIC ${DSLE_SOURCE_FILES})

set_target_properties(dsle-static PROPERTIES
    COMPILE_DEFINITIONS "DSLE_STATIC"
    OUTPUT_NAME "dsle-static"
    PUBLIC_HEADER "include/dsle.h"
    POSITION_INDEPENDENT_CODE ON
    C_STANDARD 23
)

target_include_directories(dsle-static INTERFACE src)

set(INSTALL_TARGETS dsle dsle-static)

# We also generate a 32-bits stdcall version for VBA
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    # 64 bits - do nothing. 64 bits office can just use the regular dll
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
    add_library(dsle-stdcall SHARED ${DSLE_SOURCE_FILES})

    set_target_properties (dsle-stdcall PROPERTIES
        DEFINE_SYMBOL "DSLE_EXPORTS"
        COMPILE_DEFINITIONS "DSLE_USE_STDCALL"
        OUTPUT_NAME "dsle-stdcall"
        PUBLIC_HEADER "include/dsle.h"
        C_STANDARD 23
    )

    set(INSTALL_TARGETS ${INSTALL_TARGETS} dsle-stdcall)
endif()

add_subdirectory(src/unity)
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${DSLE_SOURCE_FILES})
set_target_properties (dsle PROPERTIES FOLDER engines_gpl/dsle)
set_target_properties (dsle-static PROPERTIES FOLDER engines_gpl/dsle)
# Unit tests
add_subdirectory(tests)

install(
    TARGETS
    dsle RUNTIME DESTINATION lib)
endif(WIN32)
