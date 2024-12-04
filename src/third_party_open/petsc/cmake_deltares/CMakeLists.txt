if(intel_version LESS 23)
    message(FATAL_ERROR "PETSc was compiled with Intel OneAPI 2023 or 2024, not with older versions. Please update your compiler.")
endif()
# Set the path to the directory which contains all the source files and definitions
set(library_path ${petsc_path}/..)

set(source_path "${library_path}/src")

set(sources ${source_path}/ksp/f90-mod/petscpcmod.F90
            ${source_path}/ksp/f90-mod/petsckspdefmod.F90
            ${source_path}/dm/f90-mod/petscdmdamod.F90
            ${source_path}/ksp/f90-mod/petsckspmod.F90
            ${source_path}/dm/f90-mod/petscdmmod.F90
            ${source_path}/dm/f90-mod/petscdmplexmod.F90
            ${source_path}/mat/f90-mod/petscmatmod.F90
            ${source_path}/sys/f90-mod/petscsysmod.F90
            ${source_path}/tao/f90-mod/petsctaomod.F90
            ${source_path}/ts/f90-mod/petsctsmod.F90
            ${source_path}/vec/f90-mod/petscvecmod.F90
            ${source_path}/sys/f90-mod/petscsysmod.F90
            ${source_path}/tao/f90-mod/petsctaomod.F90
            ${source_path}/snes/f90-mod/petscsnesmod.F90
)

add_compile_options("${file_preprocessor_flag}")

set(library_name petsc)
add_library(${library_name} ${sources})

target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

target_include_directories(${library_name} PRIVATE ${mpi_module_path} ${mpi_include_path})
target_include_directories(${library_name} PRIVATE ${petsc_path})

# Create the vfproj structure
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${sources})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/petsc)

if (WIN32)
	install(DIRECTORY ${library_path}/lib/x64/Release-oneAPI/ DESTINATION lib
	FILES_MATCHING
	PATTERN "*.exe"
	PATTERN "*.dll")
endif(WIN32)
