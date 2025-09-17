f90twtest(tests_nefis
    CFILES tests_nefis.cpp
    F90FILES tests_nefis.f90 tests_nefis_helper.f90
    F2HFILES ${CMAKE_CURRENT_SOURCE_DIR}/tests_nefis.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES nefis
    VISUAL_STUDIO_PATH utils_lgpl/nefis/tests
)

set(test_resources
    test4.result
    test5.result
)

# Copy each resource to the build directory
foreach(resource_file IN LISTS test_resources)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${resource_file}"
        COMMAND ${CMAKE_COMMAND} -E copy
                "${CMAKE_CURRENT_SOURCE_DIR}/${resource_file}"
                "${CMAKE_CURRENT_BINARY_DIR}/${resource_file}"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${resource_file}"
    )
    list(APPEND test_resource_outputs "${CMAKE_CURRENT_BINARY_DIR}/${resource_file}")
endforeach()

# Add a custom target to group the resource copying
add_custom_target(copy_test_resources ALL DEPENDS ${test_resource_outputs})

# Make your test(s) depend on the resources
add_dependencies(tests_nefis copy_test_resources)