# Set the path to the directory which contains all the source files and definitions
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../petsc-3.10.2")

# Include directories that are necessary references
include_directories(${library_path}/include)
include_directories(${mpi_include_path})

set(source_path "${library_path}/src")
set(sources ${source_path}/dm/f90-mod/petscdmmod.F
            ${source_path}/dm/f90-mod/petscdmplexmod.F
            ${source_path}/ksp/f90-mod/petsckspmod.F
            ${source_path}/mat/f90-mod/petscmatmod.F
            ${source_path}/snes/f90-mod/petscsnesmod.F
            ${source_path}/sys/f90-mod/petscsysmod.F
            ${source_path}/tao/f90-mod/petsctaomod.F
            ${source_path}/ts/f90-mod/petsctsmod.F
            ${source_path}/vec/f90-mod/petscvecmod.F)

add_compile_options("${file_preprocessor_flag}")

set(library_name petsc)
add_library(${library_name} ${sources})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the vfproj structure
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${sources} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/petsc)