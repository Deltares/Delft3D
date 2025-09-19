#Only for windows build
if (WIN32)

set(executable_name nefis_test_00)
set(source ${executable_name}.c)
set_source_files_properties(${executable_name}.c PROPERTIES LANGUAGE CXX)
add_executable(${executable_name} ${source})  

    target_include_directories(${executable_name} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../../include
        ${CMAKE_CURRENT_SOURCE_DIR}/../../../../../../third_party_open/googletest/googletest-main/googletest/include
    )

    target_link_libraries(${executable_name} nefis)
    
endif()