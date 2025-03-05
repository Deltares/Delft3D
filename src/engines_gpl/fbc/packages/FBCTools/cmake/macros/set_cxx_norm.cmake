# Taken from POLDER, licensed under LGPL v3.
# https://github.com/Morwenn/POLDER

# Version
cmake_minimum_required(VERSION 2.6.3)

set(CXX_NORM_CXX98 1)   # C++98
set(CXX_NORM_CXX03 2)   # C++03
set(CXX_NORM_CXX11 3)   # C++11

# - Set the wanted C++ norm
# Adds the good argument to the command line in function of the compiler
# Currently only works with g++ and clang++
macro(set_cxx_norm NORM CXX_NORM_FLAGS)

    # Extract c++ compiler --version output
    exec_program(
        ${CMAKE_CXX_COMPILER}
        ARGS --version
        OUTPUT_VARIABLE _compiler_output
    )
    # Keep only the first line
    string(REGEX REPLACE
        "(\n.*$)"
        ""
        cxx_compiler_version "${_compiler_output}"
    )
    # Extract the version number
    string(REGEX REPLACE
        "([^0-9.])|([0-9.][^0-9.])"
        ""
        cxx_compiler_version "${cxx_compiler_version}"
    )

    if(CMAKE_COMPILER_IS_GNUCXX)

        if(${NORM} EQUAL ${CXX_NORM_CXX98})
            set(${CXX_NORM_FLAGS} "-std=c++98")
        elseif(${NORM} EQUAL ${CXX_NORM_CXX03})
            set(${CXX_NORM_FLAGS} "-std=c++03")
        elseif(${NORM} EQUAL ${CXX_NORM_CXX11})
            if(${cxx_compiler_version} VERSION_LESS "4.7.0")
                set(${CXX_NORM_FLAGS} "-std=c++0x")
            else()
                set(${CXX_NORM_FLAGS} "-std=c++11")
            endif()
        endif()

    elseif(${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")

        if(${NORM} EQUAL ${CXX_NORM_CXX11})
            set(${CXX_NORM_FLAGS} "-std=c++0x")
        endif()

    endif()

endmacro()
