include(CTest)

# Test dependencies interface library
add_library(test_dependencies INTERFACE)
target_link_libraries(test_dependencies INTERFACE
    Unity
    zsf-static
)
target_compile_definitions(test_dependencies INTERFACE ZSF_STATIC)
set_target_properties(test_dependencies PROPERTIES C_STANDARD 23)

# Test executables and add them to CTest
add_executable(test_dimr_bmi test_dimr_bmi.c)
target_link_libraries(test_dimr_bmi PRIVATE test_dependencies)
add_test(NAME dsle_test_dimr_bmi COMMAND test_dimr_bmi)

add_executable(test_io_layer_distribution test_io_layer_distribution.c)
target_link_libraries(test_io_layer_distribution PRIVATE test_dependencies)
add_test(NAME dsle_test_io_layer_distribution COMMAND test_io_layer_distribution)

add_executable(test_load_csv test_load_csv.c)
target_link_libraries(test_load_csv PRIVATE test_dependencies)
add_test(NAME dsle_test_load_csv COMMAND test_load_csv WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(test_ini_read test_ini_read.c)
target_link_libraries(test_ini_read PRIVATE test_dependencies)
add_test(NAME dsle_test_ini_read COMMAND test_ini_read WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(test_timestamp test_timestamp.c)
target_link_libraries(test_timestamp PRIVATE test_dependencies)
add_test(NAME dsle_test_timestamp COMMAND test_timestamp)

foreach(test_target
    test_dimr_bmi
    test_io_layer_distribution
    test_load_csv
    test_ini_read
    test_timestamp
)
    set_target_properties(${test_target} PROPERTIES FOLDER tests/engines_gpl/dsle)
endforeach()
