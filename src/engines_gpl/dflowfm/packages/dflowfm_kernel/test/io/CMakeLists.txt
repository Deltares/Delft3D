

# set(NAME test_mdu_file_read_write)


# message(STATUS "Preparing copying Resources from: ${CMAKE_CURRENT_SOURCE_DIR}  ${${PROJECT_SOURCE_DIR}}")
# file(GLOB_RECURSE test_resources RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/resources" "${CMAKE_CURRENT_SOURCE_DIR}/resources/*")

# foreach(resource_file IN LISTS test_resources)
#     add_custom_command(
#         TARGET  ${NAME} PRE_BUILD
#         COMMAND ${CMAKE_COMMAND} -E copy_if_different
#             "${CMAKE_CURRENT_SOURCE_DIR}/resources/${resource_file}"
#             "$<TARGET_FILE_DIR:${NAME}>/${resource_file}"
#     )
# endforeach()
function(copy_test_resources NAME SOURCE_FOLDER TARGET_FOLDER)

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

    file(GLOB_RECURSE test_resources RELATIVE "${SOURCE_FOLDER}" "${SOURCE_FOLDER}/*")
    
    foreach(resource_file IN LISTS test_resources)
        add_custom_command(
            TARGET ${NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${SOURCE_FOLDER}/${resource_file}"
                "${TARGET_FOLDER}/${resource_file}"
        )
    endforeach()
endfunction()

f90twtest(test_mdu_file_read_write
    CFILES test_mdu_file_read_write.cpp
    F90FILES test_mdu_file_read_write.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_mdu_file_read_write.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)


f90twtest(test_ec_module_block
    CFILES test_ec_module_block.cpp
    F90FILES test_ec_module_block.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/test_ec_module_block.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)


copy_test_resources("test_mdu_file_read_write" "${CMAKE_CURRENT_SOURCE_DIR}/resources/MDUversion" "$<TARGET_FILE_DIR:test_mdu_file_read_write>/MDUversion")
copy_test_resources("test_ec_module_block" "${CMAKE_CURRENT_SOURCE_DIR}/resources/ec_module" "$<TARGET_FILE_DIR:test_ec_module_block>/ec_module")