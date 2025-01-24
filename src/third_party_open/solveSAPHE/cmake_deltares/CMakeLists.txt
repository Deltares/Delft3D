# Set the source path where the library components are located
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

set(source_path "${library_path}/src")

set(fortran_sources ${source_path}/mod_precision.f90
                    ${source_path}/mod_chemconst.f90
                    ${source_path}/mod_acbw_phsolvers.F90)

# Add library
set (solvesaphe_library_name solvesaphe)
add_library(${solvesaphe_library_name} ${fortran_sources})

target_link_libraries(${solvesaphe_library_name} PRIVATE no_compiler_warnings)

# Set dependencies
set(oss_dependencies    deltares_common)

target_link_libraries(${solvesaphe_library_name} PRIVATE ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${solvesaphe_library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${solvesaphe_library_name} PRIVATE ${waq_default_flags})

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortran_sources} )
set_target_properties (${solvesaphe_library_name} PROPERTIES FOLDER third_party_open)