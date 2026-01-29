# Set the source path where the library components are located
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

if(WIN32)
   if (NOT TARGET zlib)
      add_subdirectory(${checkout_src_root}/${zlib_module} zlib)
   endif()
endif(WIN32)

if(WIN32)
   if (NOT TARGET curl)
      add_subdirectory(${checkout_src_root}/${curl_module} curl)
   endif()
endif(WIN32)

if(WIN32)
   if (NOT TARGET hdf5)
      add_subdirectory(${checkout_src_root}/${hdf5_module} hdf5)
   endif()
endif(WIN32)

# Include directories that are necessary references for netcdff
include_directories(${library_path}/netcdf-fortran-4.4.4/libsrc)

set(source_path "${library_path}/netcdf-fortran-4.4.4/fortran")

# Include directories that are necessary references for netcdf4
message(STATUS ${netcdf_version} " include dir is ${library_path}/${netcdf_version}/include")
include_directories("${library_path}/${netcdf_version}/include")
set(c_sources ${source_path}/nf_lib.c)
find_package(netcdf REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../${netcdf_version})
set(HDF5_USE_STATIC_LIBRARIES true)
find_package(hdf5 REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../../hdf5)
find_package(zlib REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../../zlib)
find_package(curl REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../../curl)
set(hdf5_library_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../hdf5/lib)
# Add library
add_library(netcdf_c_extension STATIC ${c_sources})
# Link static dependencies
target_link_libraries(netcdf_c_extension
    PRIVATE
        ${hdf5_library_dir}/hdf5.lib
		  ${hdf5_library_dir}/hdf5_hl.lib
        CURL::libcurl
		  netCDF::netcdf
		  ZLIB::ZLIB
)
target_link_libraries(netcdf_c_extension PRIVATE no_compiler_warnings)

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${c_sources} )
set_target_properties (netcdf_c_extension PROPERTIES FOLDER third_party_open)

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
 target_link_libraries(${netcdf_library_name} PRIVATE no_compiler_warnings)

 # Set dependencies
 set(oss_dependencies netcdf_c_extension)
 target_link_libraries(${netcdf_library_name} PRIVATE ${oss_dependencies})
 target_compile_options(${netcdf_library_name} PRIVATE "SHELL:${heap_arrays_100_flag}")

 # Create the folder structure in vfproj
 source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortran_sources} )
 set_target_properties (${netcdf_library_name} PROPERTIES FOLDER third_party_open)

message(STATUS ${netcdf_version} " binary dir is ${library_path}/netcdf-c-4.9.2/bin/")
set(binary_dir ${library_path}/netcdf-c-4.9.2/bin)
install (FILES
${binary_dir}/netcdf.dll
DESTINATION lib
)
