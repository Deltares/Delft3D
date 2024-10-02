# Set include path
set(deltares_common_include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set version file variables
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/deltares_common_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set include path to have it accessible for other components
set(public_include_path ${deltares_common_include_path})

# Set the directory of where the source code is located
set(src_path src)

get_fortran_source_files(${src_path}/rd_token rd_token_source)

set(stdlib_source ${src_path}/stdlib/stdlib_kinds.f90
                  ${src_path}/stdlib/stdlib_optval.f90
                  ${src_path}/stdlib/stdlib_sorting.f90
                  ${src_path}/stdlib/stdlib_sorting_sort.f90
                  ${src_path}/stdlib/stdlib_sorting_ord_sort.f90
                  ${src_path}/stdlib/stdlib_sorting_radix_sort.f90
                  ${src_path}/stdlib/stdlib_sorting_sort_index.f90
)

get_fortran_source_files(${src_path} source)

set(library_name deltares_common)
add_library(${library_name} ${source}
                            ${rd_token_source}
                            ${stdlib_source}
                            ${fortran_version_file})

if(UNIX)
   set(oss_dependencies deltares_common_c)
   target_link_libraries(${library_name} ${oss_dependencies})
   add_definitions(-DHAVE_CONFIG_H=1)
endif(UNIX)

target_include_directories(${library_name}  PRIVATE ${version_include_dir})


# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

set_source_files_properties(${fortran_version_file}
                            ${src_path}/system_utils.F90
                            ${src_path}/MessageHandling.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")




# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source}
                                                    ${rd_token_source}
                                                    ${stdlib_source}
                                                    ${fortran_version_file})

set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)

