# Set source directory
set(src_path .)

# Gather source files
get_fortran_source_files(${src_path} source)

# Define executable
set(executable_name calcage)
add_executable(${executable_name} ${source})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/calcage)

# Set post-build step
# internal use only - not part of the DIMR set

if(WIN32)
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)
