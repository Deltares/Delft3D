# Set the directory which contains all the source files
set(src_path "${CMAKE_CURRENT_SOURCE_DIR}/..")

# Define the libFLAP library
set(libFLAP_path ${src_path}/src)

set(libFLAP_sources ${libFLAP_path}/Data_Type_Command_Line_Interface.F90
                    ${libFLAP_path}/IR_Precision.F90)

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            ${libFLAP_path}/Data_Type_Command_Line_Interface.F90
                            ${libFLAP_path}/IR_Precision.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
                            
if (UNIX)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake.in ${libFLAP_path}/config.h)  
    set(configFile ${libFLAP_path}/config.h) 
endif(UNIX)

set(libFLAP_library_name FLAP)
add_library(${libFLAP_library_name} ${libFLAP_sources})

target_link_libraries(${libFLAP_library_name} PRIVATE no_compiler_warnings)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${libFLAP_library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_nopointers_flag}>")

# Create the folder structure in project
source_group("${visual_studio_source_group_name}" FILES ${libFLAP_sources})

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${libFLAP_sources})
set_target_properties (${libFLAP_library_name} PROPERTIES FOLDER third_party_open/libFLAP)
