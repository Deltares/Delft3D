set(USE_SLOW_NEFIS_TESTS OFF)

add_fortran_gtest(nefis_tests
    SOURCES nefis_tests.f90 tests_nefis_helper.f90
    LIBRARIES nefis
    INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/resources
    VISUAL_STUDIO_PATH utils_lgpl/nefis/tests
) 

if(USE_SLOW_NEFIS_TESTS)
   add_fortran_gtest(slow_nefis_tests
       SOURCES slow_nefis_tests.f90 tests_nefis_helper.f90
       LIBRARIES nefis
       INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/resources
       VISUAL_STUDIO_PATH utils_lgpl/nefis/slow_tests
   ) 
endif()

# Copy all files from resources to the build output directory
file(GLOB_RECURSE test_resources RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/resources" "${CMAKE_CURRENT_SOURCE_DIR}/resources/*")

foreach(resource_file IN LISTS test_resources)
    add_custom_command(
        TARGET nefis_tests PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${CMAKE_CURRENT_SOURCE_DIR}/resources/${resource_file}"
            "$<TARGET_FILE_DIR:nefis_tests>/${resource_file}"
    )
endforeach()
