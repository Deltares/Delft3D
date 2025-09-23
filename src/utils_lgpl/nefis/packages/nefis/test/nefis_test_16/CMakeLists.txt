f90twtest(nefis_test_16
    CFILES nefis_test_16.cpp
    F90FILES nefis_test_16.f90 ../common/tests_nefis_helper.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/nefis_test_16.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES nefis
    INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/resources
    VISUAL_STUDIO_PATH utils_lgpl/nefis/tests
) 

# Copy all files from resources to the build output directory
file(GLOB_RECURSE test_resources RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/resources" "${CMAKE_CURRENT_SOURCE_DIR}/resources/*")

foreach(resource_file IN LISTS test_resources)
    add_custom_command(
        TARGET nefis_test_16 PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_CURRENT_SOURCE_DIR}/resources/${resource_file}"
            "$<TARGET_FILE_DIR:nefis_test_16>/${resource_file}"
    )
endforeach()

add_custom_command(
    TARGET nefis_test_16 PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
        "$<TARGET_FILE_DIR:nefis_test_16>/libifcoremdd.dll"
)

add_custom_command(
    TARGET nefis_test_16 PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libmmd.dll"
        "$<TARGET_FILE_DIR:nefis_test_16>/libmmd.dll"
)