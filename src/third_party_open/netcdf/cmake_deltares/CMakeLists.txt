# Set the source path where the library components are located
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/..")

# Include directories that are necessary references for netcdff
include_directories(${library_path}/netcdf-fortran-4.5.4/libsrc)

set(source_path "${library_path}/netcdf-fortran-4.5.4/fortran")

# Include directories that are necessary references for netcdf4
# message(STATUS ${netcdf_version} " include dir is ${library_path}/${netcdf_version}/include")
# include_directories("${library_path}/${netcdf_version}/include")
# set(c_sources ${source_path}/nf_lib.c)

file(GLOB SRC_FILES ${source_path}/*.f90 ${source_path}/*.F90)
message("HARMEN HIDDE " ${SRC_FILES})

set(fortran_sources ${SRC_FILES})

# Add library
set (netcdf_library_name netcdff)
add_library(${netcdf_library_name} ${fortran_sources})

# Set additional compilation properties for the netcdf library
target_compile_options(${netcdf_library_name} PRIVATE ${file_preprocessor_flag})
target_link_libraries(${netcdf_library_name} PRIVATE no_compiler_warnings)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${netcdf_library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${netcdf_library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Set dependencies
set(oss_dependencies netCDF::netcdf)
oss_include_libraries(${netcdf_library_name} oss_dependencies)

if (WIN32)
   target_compile_options(${netcdf_library_name} PRIVATE "${heap_arrays_100_flag}")
endif(WIN32)

if(UNIX)
    target_compile_options(${netcdf_library_name} PRIVATE ${c_compiler_flags})
    target_link_libraries(${netcdf_library_name} PRIVATE ${oss_dependencies})
    set(Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_DIRECTORY}) 
endif()

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortran_sources} )
set_target_properties (${netcdf_library_name} PROPERTIES FOLDER third_party_open)

# if (WIN32)
	# message(STATUS ${netcdf_version} " binary dir is ${library_path}/netCDF 4.6.1/bin/")
	# install (DIRECTORY ${library_path}netCDF\ 4.6.1/bin/ DESTINATION lib
	# FILES_MATCHING
	# PATTERN "*.dll"
	# )
# endif()