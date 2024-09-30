# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location

# Set the DFlow FM component path
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")

# Include directories that are required as references
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set the directory of where the source code is located
set(src_path src)

# Gather the source files
set(headers include/blasfm.h)

# Set version file variables
set(dflowfm_version_header_path ${dflowfm_path}/version)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(dflowfm_version_path ${src_path}/dflowfm_version)
set(fortran_version_files ${dflowfm_version_path}/dflowfm_version.F90
                          ${dflowfm_version_path}/static_version_info.F90)

set_source_files_properties(${fortran_version_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

set(dflowfm_data_path ${src_path}/dflowfm_data)
get_fortran_source_files(${dflowfm_data_path} dflowfm_data)

set(dflowfm_gui_path ${src_path}/dflowfm_gui)
get_fortran_source_files(${dflowfm_gui_path} dflowfm_gui)

set(dflowfm_io_path ${src_path}/dflowfm_io)
get_fortran_source_files(${dflowfm_io_path} dflowfm_io)

list(REMOVE_ITEM dflowfm_io ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/io_openfoam.f90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/read_forcings.f90)

set(dflowfm_kernel_path ${src_path}/dflowfm_kernel)
set(dflowfm_kernel_compute_path ${dflowfm_kernel_path}/compute)
set(dflowfm_kernel_compute_hydrology_path ${dflowfm_kernel_path}/compute_hydrology)
set(dflowfm_kernel_compute_sediment_path ${dflowfm_kernel_path}/compute_sediment)
set(dflowfm_kernel_compute_sediment_netnode_based_path ${dflowfm_kernel_path}/compute_sediment/netnode_based)
set(dflowfm_kernel_compute_transport_path ${dflowfm_kernel_path}/compute_transport)
set(dflowfm_kernel_compute_waq_path ${dflowfm_kernel_path}/compute_waq)
set(dflowfm_kernel_compute_waves_path ${dflowfm_kernel_path}/compute_waves)
set(dflowfm_kernel_compute_waves_surfbeat_path ${dflowfm_kernel_path}/compute_waves/surfbeat)
set(dflowfm_kernel_limiters_path ${dflowfm_kernel_path}/limiters)
set(dflowfm_kernel_main_path ${dflowfm_kernel_path}/main)
set(dflowfm_kernel_model_specific_path ${dflowfm_kernel_path}/model_specific)
set(dflowfm_kernel_prepost_path ${dflowfm_kernel_path}/prepost)
set(dflowfm_kernel_timespace_path ${dflowfm_kernel_path}/timespace)


get_fortran_source_files(${dflowfm_kernel_path} dflowfm_kernel)
get_fortran_source_files(${dflowfm_kernel_compute_path} dflowfm_kernel_compute)
get_fortran_source_files(${dflowfm_kernel_compute_hydrology_path} dflowfm_kernel_compute_hydrology)
get_fortran_source_files(${dflowfm_kernel_compute_sediment_path} dflowfm_kernel_compute_sediment)
get_fortran_source_files(${dflowfm_kernel_compute_sediment_netnode_based_path} dflowfm_kernel_compute_sediment_netnode_based)
get_fortran_source_files(${dflowfm_kernel_compute_transport_path} dflowfm_kernel_compute_transport)
get_fortran_source_files(${dflowfm_kernel_compute_waq_path} dflowfm_kernel_compute_waq)
get_fortran_source_files(${dflowfm_kernel_compute_waves_path} dflowfm_kernel_compute_waves)
get_fortran_source_files(${dflowfm_kernel_compute_waves_surfbeat_path} dflowfm_kernel_compute_waves_surfbeat)
get_fortran_source_files(${dflowfm_kernel_path}/limiters dflowfm_kernel_limiters)
get_fortran_source_files(${dflowfm_kernel_path}/main dflowfm_kernel_main)
get_fortran_source_files(${dflowfm_kernel_path}/model_specific dflowfm_kernel_model_specific)
get_fortran_source_files(${dflowfm_kernel_prepost_path} dflowfm_kernel_prepost)
get_fortran_source_files(${dflowfm_kernel_timespace_path} dflowfm_kernel_timespace)

list(REMOVE_ITEM dflowfm_kernel_compute_waves_surfbeat ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_compute_waves_surfbeat_path}/xbeach_wave_boundary_init.f90)

# Gather source files
set(dflowfm_manager_path ${src_path}/dflowfm_manager/)
get_fortran_source_files(${dflowfm_manager_path} dflowfm_manager)

# Gather source files
set(dflowfm_rgf_path ${src_path}/dflowfm_rgf/)
get_fortran_source_files(${dflowfm_rgf_path} dflowfm_rgf)

# Gather source files
set(dflowfm_utils_path ${src_path}/dflowfm_utils/)
get_fortran_source_files(${dflowfm_utils_path} dflowfm_utils)

get_fortran_source_files(${dflowfm_utils_path}/rest_f90 dflowfm_utils_rest_f90)
get_fortran_source_files(${dflowfm_utils_path}/to_deltares_common dflowfm_utils_to_deltares_common)

list(REMOVE_ITEM dflowfm_utils ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/solve_parms.F90)

get_fortran_source_files(${src_path}/to_thirdparty to_thirdparty)

# Define library
set(library_name dflowfm_kernel)
set(library_source_list     ${fortran_version_files}
                            ${headers}
                            ${dflowfm_data}
                            ${dflowfm_gui}
                            ${dflowfm_io}
                            ${dflowfm_kernel}
                            ${dflowfm_kernel_compute}
                            ${dflowfm_kernel_compute_hydrology}
                            ${dflowfm_kernel_compute_sediment}
                            ${dflowfm_kernel_compute_sediment_netnode_based}
                            ${dflowfm_kernel_compute_transport}
                            ${dflowfm_kernel_compute_waq}
                            ${dflowfm_kernel_compute_waves}
                            ${dflowfm_kernel_compute_waves_surfbeat}
                            ${dflowfm_kernel_limiters}
                            ${dflowfm_kernel_main}
                            ${dflowfm_kernel_model_specific}
                            ${dflowfm_kernel_prepost}
                            ${dflowfm_kernel_timespace}
                            ${dflowfm_manager}
                            ${dflowfm_rgf}
                            ${dflowfm_utils}
                            ${dflowfm_utils_rest_f90}
                            ${dflowfm_utils_to_deltares_common}
                            ${to_thirdparty})

add_library(${library_name} ${library_source_list})

target_link_libraries(${library_name} PRIVATE
    mute_unwanted_compiler_warnings
    additional_compiler_warnings
)

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PETSC REQUIRED IMPORTED_TARGET PETSc)

    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            flow1d_implicit
                            gridgeom
                            ice_data
                            ice_io
                            io_netcdf
                            io_netcdf_data
                            kdtree_wrapper
                            kdtree2
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metis
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            netcdf4
                            netcdff
                            polypack
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_definition
                            wq_processes
                            waq_process
                            waq_hyd_data
                            waq_utils_c
                            waq_utils_f
                            interacter_stub)

    target_link_libraries(${library_name} PRIVATE ${oss_dependencies}
         PkgConfig::PETSC
         uuid
    )

endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         deltares_common_mpi
                         dhydrology_kernel
                         ec_module
                         flow1d
                         flow1d_core
                         flow1d_io
                         flow1d_implicit
                         gridgeom
                         ice_data
                         ice_io
                         io_netcdf
                         io_netcdf_data
                         kdtree_wrapper
                         kdtree2
                         fortrangis
                         libsigwatch
                         md5digest
                         md5
                         metis
                         metisoptions
                         morphology_data
                         morphology_io
                         morphology_kernel
                         morphology_plugins_c
                         netcdf4
                         netcdff
                         petsc
                         polypack
                         trachytopes_io
                         trachytopes_kernel
                         triangle_c
                         waq_definition
                         waq_process
                         waq_hyd_data
                         waq_utils_c
                         waq_utils_f
                         wq_processes
    )

    if(NOT WITH_INTERACTER)
        set(oss_dependencies ${oss_dependencies} interacter_stub)
    endif()

    target_link_libraries(${library_name} PRIVATE
                          ${oss_dependencies}
    )

    include_directories(${petsc_path})
endif(WIN32)

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpi_module_path}
                        ${tecplot_path}
                        ${version_include_dir}
                        ${dflowfm_version_path}
                        ${dflowfm_version_header_path})

# Define additional compilation flags

set_source_files_properties(${fortran_version_files}
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_data_path}/partition.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_gui_path}/m_gui.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_gui_path}/menuv1.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_gui_path}/setwor.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_gui_path}/viewport.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/read_commandline.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/writesomefinaloutput.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/wrwaq.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/tecplot.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/unc_write_his.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_io_path}/unc_write_shp.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_compute_path}/filter.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_compute_waves_surfbeat_path}/xbeach_filefunctions.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/find1dcells.f90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/flow_modelinit.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/inidat.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/print_help_commandline.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/fill_onlyWetLinks.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/m_update_values_on_cross_sections.f90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_prepost_path}/set_model_boundingbox.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_timespace_path}/timespaceinitialfield_mpi.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_manager_path}/inctime_user.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_manager_path}/dfm_signals.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_manager_path}/flow_externaloutput.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/filez.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/init_openmp.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/mormerge_mpi.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/solve_petsc.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/solve_guus.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/saadf.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/saadf90.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/scherm.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/unstruc_shapefile.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/unstruc_display.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/unstruc_opengl.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/rest_f90/dcross.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_utils_path}/rest_f90/read_samples_from_geotiff.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${dflowfm_kernel_compute_waves_path}/fetch_proc.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Define preprocessor definitions
if (WIN32)
    set_target_properties(${library_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
    if(WITH_INTERACTER)
        # Add HAVE_DISPLAY=1
        target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_TECPLOT;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC;HAVE_OPENGL;HAVE_DISPLAY=1")
    else()
        target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_TECPLOT;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC;HAVE_OPENGL;HAVE_DISPLAY=0")
    endif()

    add_warnings_as_errors_post_build_windows(${library_name})
endif(WIN32)

if (UNIX)
    target_compile_definitions(${library_name} PRIVATE "HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC")
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_source_list})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/dflowfm)
