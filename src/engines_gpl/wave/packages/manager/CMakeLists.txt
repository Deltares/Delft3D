# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(wave_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the D-Waves root

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name wave_manager)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies wave_data
                     delftio
                     deltares_common
                     ec_module
                     wave_io
                     wave_kernel
)

option(IS_WAVE_WITH_GREETER "Compile wave with preCICE greeter_dummy coupling" OFF)
if(IS_WAVE_WITH_GREETER)
    target_compile_definitions(${library_name} PRIVATE IS_WAVE_WITH_GREETER)
endif()

target_link_libraries(${library_name} PRIVATE ${oss_dependencies} precice_module)

include_directories(${mpi_module_path})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
