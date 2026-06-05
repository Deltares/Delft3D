#Only for windows build
if (WIN32)

# Collect test directories test_00 to test_21
set(TEST_DIRS)
foreach(i RANGE 0 21)
    if(i LESS 10)
        set(test_name test_0${i})
    else()
        set(test_name test_${i})
    endif()
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${test_name}")
        if(NOT i MATCHES 13)
            list(APPEND TEST_DIRS ${test_name})
        endif()
    endif()
endforeach()

foreach(test_dir ${TEST_DIRS})
    set(executable_name ${test_dir})

    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${executable_name}/${executable_name}.c")
        set (source ${CMAKE_CURRENT_SOURCE_DIR}/${executable_name}/${executable_name}.c)
        if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${executable_name}/${executable_name}.f90")
           message(ERROR "Both .c and .f90 files found for ${executable_name}. Stopping build.")
        endif() 
    elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${executable_name}/${executable_name}.f90")
        set (source ${CMAKE_CURRENT_SOURCE_DIR}/${executable_name}/${executable_name}.f90)
    else()
        message(ERROR "No source file found for ${executable_name}. Stopping build.")
    endif()
    add_executable(${executable_name} ${source})
    
    target_include_directories(${executable_name} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../packages/nefis/include
    )
    
    target_link_libraries(${executable_name} nefis)
    
    # Create the folder structure in vfproj
    source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
    set_target_properties (${executable_name} PROPERTIES FOLDER tests/nefis/${executable_name})

    configure_visual_studio_user_file(${executable_name})

    install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/run_test.${platform_extension} DESTINATION test/nefis)
    install(TARGETS ${executable_name} RUNTIME  DESTINATION test/nefis)

endforeach()

endif()