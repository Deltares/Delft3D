# Set the source path where the library components are located
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

# Include directories that are necessary references for netcdff
include_directories(${library_path}/netcdf-fortran-4.4.4/libsrc)

set(source_path "${library_path}/netcdf-fortran-4.4.4/fortran")

# Include directories that are necessary references for netcdf4
message(STATUS ${netcdf_version} " include dir is ${library_path}/${netcdf_version}/include")
include_directories("${library_path}/${netcdf_version}/include")
set(c_sources ${source_path}/nf_lib.c)

# Add library
add_library(netcdf4 ${c_sources})

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${c_sources} )
set_target_properties (netcdf4 PROPERTIES FOLDER third_party_open)

set(fortran_sources ${source_path}/module_netcdf4_nc_interfaces.f90
					${source_path}/module_netcdf4_nf_interfaces.F90
					${source_path}/module_netcdf_nc_data.F90
					${source_path}/module_netcdf_nc_interfaces.f90
					${source_path}/module_netcdf_nf_data.F90
					${source_path}/netcdf4.f90
					${source_path}/nf_attio.F90
					${source_path}/nf_control.F90
					${source_path}/nf_dim.f90
					${source_path}/nf_genatt.f90
					${source_path}/nf_geninq.f90
					${source_path}/nf_genvar.f90
					${source_path}/nf_misc.f90
					${source_path}/nf_nc4.f90
					${source_path}/nf_par_dummy.f90
					${source_path}/nf_var1io.F90
					${source_path}/nf_varaio.F90
					${source_path}/nf_varmio.F90
					${source_path}/nf_varsio.F90
					${source_path}/typeSizes.f90)

# Add library
set (netcdf_library_name netcdff)
add_library(${netcdf_library_name} ${fortran_sources})

# Set additional compilation properties for the netcdf library
target_compile_options(${netcdf_library_name} PRIVATE ${file_preprocessor_flag})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${netcdf_library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${netcdf_library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Set dependencies
set(oss_dependencies netcdf4)
oss_include_libraries(${netcdf_library_name} oss_dependencies)

if (WIN32)
   target_compile_options(${netcdf_library_name} PRIVATE "${heap_arrays_100_flag}")
endif(WIN32)

if(UNIX)
    target_compile_options(${netcdf_library_name} PRIVATE ${c_compiler_flags})
    target_link_libraries(${netcdf_library_name} ${oss_dependencies})
    set(Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_DIRECTORY}) 
endif()

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortran_sources} )
set_target_properties (${netcdf_library_name} PROPERTIES FOLDER third_party_open)
