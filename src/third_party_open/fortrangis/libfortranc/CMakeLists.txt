# CMake file for the fortranc library
#
# TODO: iso_varying_string?

set(fortranc_LIB_SRCS
    fortranc.F90
    )

add_library(fortranc ${fortranc_LIB_SRCS})
add_library(${PROJECT_NAME}::fortranc ALIAS fortranc)

set_target_properties(fortranc PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(fortranc PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules> PRIVATE
  ${PROJECT_BINARY_DIR})

# Skipping this: it relies on a Unix/Linx specific include file
#add_executable(fortranc_test fortranc_test.F90 c_test.c)
#target_link_libraries(fortranc_test fortranc)
