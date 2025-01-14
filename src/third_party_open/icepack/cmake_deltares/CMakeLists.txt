# Set the directory of where the source code is located.
set(src_directory "${CMAKE_CURRENT_SOURCE_DIR}/../src") 

# Gather source files
get_fortran_source_files(${src_directory} source_files)

# Define library
set(library_name icepack)
add_library(${library_name} ${source_files})

target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

# Define additional compilation flags
set_source_files_properties(${src_directory}/icepack_aerosol.F90
                            ${src_directory}/icepack_age.F90
                            ${src_directory}/icepack_algae.F90
                            ${src_directory}/icepack_atmo.F90
                            ${src_directory}/icepack_brine.F90
                            ${src_directory}/icepack_firstyear.F90
                            ${src_directory}/icepack_flux.F90
                            ${src_directory}/icepack_fsd.F90
                            ${src_directory}/icepack_intfc.F90
                            ${src_directory}/icepack_isotope.F90
                            ${src_directory}/icepack_itd.F90
                            ${src_directory}/icepack_kinds.F90
                            ${src_directory}/icepack_mechred.F90
                            ${src_directory}/icepack_meltpond_cesm.F90
                            ${src_directory}/icepack_meltpond_lvl.F90
                            ${src_directory}/icepack_meltpond_topo.F90
                            ${src_directory}/icepack_mushy_physics.F90
                            ${src_directory}/icepack_ocean.F90
                            ${src_directory}/icepack_orbital.F90
                            ${src_directory}/icepack_parameters.F90
                            ${src_directory}/icepack_shortwave.F90
                            ${src_directory}/icepack_snow.F90
                            ${src_directory}/icepack_therm_0layer.F90
                            ${src_directory}/icepack_therm_bl99.F90
                            ${src_directory}/icepack_therm_itd.F90
                            ${src_directory}/icepack_therm_mushy.F90
                            ${src_directory}/icepack_therm_shared.F90
                            ${src_directory}/icepack_therm_vertical.F90
                            ${src_directory}/icepack_tracers.F90
                            ${src_directory}/icepack_warnings.F90
                            ${src_directory}/icepack_wavefracspec.F90
                            ${src_directory}/icepack_zbgc.F90
                            ${src_directory}/icepack_zbgc_shared.F90
                            ${src_directory}/icepack_zsalinity.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties
# target_compile_options(${library_name} PRIVATE "${real_size_64_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Define how the files should be structured in Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
# set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/icepack_lib)
