# Define library
set(library_name rtc_kernel)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

add_library(${library_name} ${source_files})

set_source_files_properties(
                   src/CloseMatlab.F90
                   src/Cmpdecv.F90
                   src/datafrommatlab.F90
                   src/datatomatlab.F90
                   src/ERRMSG.F90
                   src/filemodule.F90
                   src/LocationDataModule.F90  # no preprocessot needed, should be .f90
                   src/OpenMatlab.F90
                   src/readlib.F90
                   src/rtc_matlab_module.F90
                   src/Rtc_Rdini.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "X64")


set(oss_dependencies_general delftio
                            delftio_shm
                            deltares_common
                            rr_rtc_tools
                            wl_openmi_support)
                             
                             
# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
    add_definitions(-DHAVE_CONFIG_H)    
endif(UNIX)

if (WIN32)
    target_link_libraries(${library_name} ${oss_dependencies_general}
                                            netcdf4
                                            netcdff)
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/rtc)

