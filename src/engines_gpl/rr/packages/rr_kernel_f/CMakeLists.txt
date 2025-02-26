# Define library
set(library_name rr_kernel_f)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)


add_library(${library_name} ${source_files})

set_source_files_properties(
                   src/avundf.F90
                   src/balancemodule.F90
                   src/datatoflowmodule.F90
                   src/dummycapsim.F90
                   src/FILESMODULE.F90
                   src/INITDT.F90
                   src/noconvergence.F90
                   src/PROB.F90
                   src/readlib.F90
                   src/restartmodule.F90
                   src/RRModule.F90
                   src/UnpavedModule.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "X64")


set(oss_dependencies_general 
                            control_lib
                            flow1d_core
                            delftio
                            delftio_shm
                            deltares_common
                            ec_module
                            rr_rtc_tools
                            rr_walrus_c
                            wl_openmi_support)
                             
                             
# Add dependencies
if (UNIX)
    set(oss_dependencies ${oss_dependencies_general} netcdff)

    target_link_libraries(${library_name}
         ${oss_dependencies}
    )
endif(UNIX)

if (WIN32)
    target_link_libraries(${library_name} ${oss_dependencies_general}
                                            netcdff)
endif(WIN32)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/rr COMPILE_OPTIONS "${file_preprocessor_flag}")

