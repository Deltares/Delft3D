project(f90tw LANGUAGES CXX Fortran)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# f90 line length; TODO handle compilers other than GNU
set(CMAKE_Fortran_FORMAT FREE)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} "-ffree-line-length-none")
endif()

add_subdirectory(f90tw)

message(STATUS "f90tw_INCLUDE_DIR is ${f90tw_INCLUDE_DIR}")
message(STATUS "f90tw_MODULES_DIR is ${f90tw_MODULES_DIR}")
