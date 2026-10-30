add_library(gtest_fortran_assertion_probes STATIC
    test_assertions.f90 test_assertion_edges.f90 test_assertion_surface.f90)
target_link_libraries(gtest_fortran_assertion_probes PRIVATE gtest_fortran_wrapper)
add_executable(test_gtest_fortran_assertions test_assertions.cpp)
target_link_libraries(test_gtest_fortran_assertions PRIVATE
    gtest_fortran_assertion_probes gtest_fortran_wrapper)
set_target_properties(gtest_fortran_assertion_probes test_gtest_fortran_assertions
    PROPERTIES FOLDER tools_gpl/gtest_fortran_wrapper/test)
if(WIN32)
    add_custom_command(TARGET test_gtest_fortran_assertions POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t
            $<TARGET_FILE_DIR:test_gtest_fortran_assertions>
            $<TARGET_RUNTIME_DLLS:test_gtest_fortran_assertions>
        COMMAND_EXPAND_LISTS)
endif()
gtest_discover_tests(test_gtest_fortran_assertions)

add_test(NAME gtest_fortran_registry_parser
    COMMAND "${Python_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/test_generate_test_registry.py")

add_fortran_gtest(test_gtest_fortran_registration
    SOURCES test_registration.f90
    CXX_SOURCES test_cleanup.cpp
    TEST_TEARDOWN wrapper_test_cleanup
    VISUAL_STUDIO_PATH tools_gpl/gtest_fortran_wrapper/test)
add_test(NAME gtest_fortran_registration_aggregate COMMAND test_gtest_fortran_registration)
