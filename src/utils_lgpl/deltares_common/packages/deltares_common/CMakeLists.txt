# Set include path
set(deltares_common_include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set version file variables
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/deltares_common_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set include path to have it accessible for other components
set(public_include_path ${deltares_common_include_path})

get_fortran_source_files(src/rd_token rd_token_source)

set(stdlib_source src/stdlib/stdlib_kinds.f90
                  src/stdlib/stdlib_optval.f90
                  src/stdlib/stdlib_sorting.f90
                  src/stdlib/stdlib_sorting_sort.f90
                  src/stdlib/stdlib_sorting_ord_sort.f90
                  src/stdlib/stdlib_sorting_radix_sort.f90
                  src/stdlib/stdlib_sorting_sort_index.f90
)
set(malloc_template_file "${CMAKE_CURRENT_SOURCE_DIR}/src/malloc_includes/m_alloc_body.f90.in")
set(malloc_output_file "${CMAKE_CURRENT_SOURCE_DIR}/src/malloc_includes/m_alloc_generated.f90")
set(malloc_generator_file "${CMAKE_CURRENT_SOURCE_DIR}/cmake/generate_m_alloc.cmake")
include("${malloc_generator_file}")
set_source_files_properties("${malloc_generator_file}"
    PROPERTIES HEADER_FILE_ONLY TRUE
)
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
    "${malloc_generator_file}"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/malloc_includes/m_alloc_body.f90.in"
)
generate_malloc()

get_fortran_source_files(src source)

set(library_name deltares_common)
add_library(${library_name} ${source}
                            ${rd_token_source}
                            ${stdlib_source}
                            ${malloc_output_file}
                            ${malloc_template_file}
                            ${malloc_generator_file}
                            ${fortran_version_file}
)

target_link_libraries(${library_name} PRIVATE deltares_common_c)

if(UNIX)
   add_definitions(-DHAVE_CONFIG_H=1)
endif(UNIX)

target_include_directories(${library_name}  PRIVATE ${version_include_dir})

set_source_files_properties(${fortran_version_file}
                            src/system_utils.F90
                            src/MessageHandling.F90
                            src/malloc.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}"
)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source}
                                                    ${rd_token_source}
                                                    ${stdlib_source}
                                                    ${fortran_version_file}
                                                    ${malloc_output_file}
                                                    ${malloc_template_file}
                                                    ${malloc_generator_file})

set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)

set(gtest_path ${CMAKE_CURRENT_SOURCE_DIR}/../../gtest)
f90twtest(test_deltares_common_gtest
    CFILES ${gtest_path}/test_deltares_common_gtest.cpp
    F90FILES ${gtest_path}/test_deltares_common_gtest.f90
             ${gtest_path}/test_malloc.f90
    F2HFILES ${gtest_path}/test_deltares_common_gtest.f90
             ${gtest_path}/test_malloc.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES deltares_common
    VISUAL_STUDIO_PATH utils_lgpl/deltares_common/gtest
)
