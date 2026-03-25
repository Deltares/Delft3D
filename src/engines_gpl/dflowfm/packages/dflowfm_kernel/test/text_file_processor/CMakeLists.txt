
f90twtest(test_text_file_processor
    CFILES test_text_file_processor.cpp
    F90FILES test_text_file_processor.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_text_file_processor.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)


set(NAME test_text_file_processor)

if(WIN32)

    # Copy PETSc DLL explicitly (not captured by TARGET_RUNTIME_DLLS)
    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "${petsc_libdir}/libpetsc.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying PETSc runtime dependency ${petsc_libdir}/libpetsc.dll to $<TARGET_FILE_DIR:${NAME}>"
    )


    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "$ENV{ONEAPI_ROOT}/mpi/latest/bin/impi.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying impi runtime dependency"
    )

    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "$ENV{ONEAPI_ROOT}/mkl/latest/bin/mkl_sequential.2.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying mkl runtime dependency"
    )

    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "${checkout_src_root}/${hdf5_module}/bin/hdf5.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying mkl runtime dependency"
    )


    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "${checkout_src_root}/${hdf5_module}/bin/hdf5_hl.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying hdf5_hl runtime dependency"
    )

    add_custom_command(TARGET ${NAME} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different 
            "${checkout_src_root}/${zlib_module}/bin/zlib.dll" 
            $<TARGET_FILE_DIR:${NAME}>
        COMMENT "Copying zlib runtime dependency"
    )
endif()

message(STATUS "Preparing copying Resources from: ${CMAKE_CURRENT_SOURCE_DIR}  ${${PROJECT_SOURCE_DIR}}")
file(GLOB_RECURSE test_resources RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/resources" CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/resources/*")

# Create OUTPUT-based custom commands for proper dependency tracking
set(resource_outputs)
foreach(resource_file IN LISTS test_resources)
    set(output_file "${CMAKE_CURRENT_BINARY_DIR}/${resource_file}")
    add_custom_command(
        OUTPUT "${output_file}"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different   
            "${CMAKE_CURRENT_SOURCE_DIR}/resources/${resource_file}"
            "${output_file}"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/resources/${resource_file}"
        COMMENT "Copying resource ${resource_file}"
    )
    list(APPEND resource_outputs "${output_file}")
endforeach()

# Create a custom target that depends on all resource outputs
add_custom_target(${NAME}_resources ALL DEPENDS ${resource_outputs})

# Make the test executable depend on the resources target
add_dependencies(${NAME} ${NAME}_resources)
