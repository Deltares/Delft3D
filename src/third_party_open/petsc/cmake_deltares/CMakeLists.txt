# Set the path to the directory which contains all the source files and definitions
set(library_path ${petsc_path}/..)

# Include directories that are necessary references
include_directories(${library_path}/include)
include_directories(${mpi_module_path})

set(source_path "${library_path}/src")
if (CMAKE_Fortran_COMPILER_VERSION MATCHES "2021.2")
    set(sources ${source_path}/ksp/f90-mod/petsckspmod.F
                ${source_path}/dm/f90-mod/petscdmmod.F
                ${source_path}/dm/f90-mod/petscdmplexmod.F
                ${source_path}/mat/f90-mod/petscmatmod.F
                ${source_path}/sys/f90-mod/petscsysmod.F
                ${source_path}/tao/f90-mod/petsctaomod.F
                ${source_path}/ts/f90-mod/petsctsmod.F
                ${source_path}/vec/f90-mod/petscvecmod.F
                ${source_path}/sys/f90-mod/petscsysmod.F
                ${source_path}/tao/f90-mod/petsctaomod.F
                ${source_path}/snes/f90-mod/petscsnesmod.F)
else()
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
                ${source_path}/snes/f90-mod/petscsnesmod.F90)
endif()

add_compile_options("${file_preprocessor_flag}")

set(library_name petsc)
add_library(${library_name} ${sources})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the vfproj structure
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${sources} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/petsc)

if (WIN32)
	install(DIRECTORY ${library_path}/lib/x64/Release-oneAPI/ DESTINATION lib
	FILES_MATCHING 
	PATTERN "*.exe" 
	PATTERN "*.dll" )
endif(WIN32)