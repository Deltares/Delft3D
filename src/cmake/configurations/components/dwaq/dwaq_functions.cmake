function(add_target_with_subdirectory target_name module_path)
    if(TARGET ${target_name})
        return()
    endif()

    message(STATUS ${target_name})
    list(APPEND CMAKE_MESSAGE_INDENT " - ")

    set(full_module_path ${checkout_src_root}/${module_path})

    message(DEBUG "adding directory:${full_module_path}")
    add_subdirectory(${full_module_path} ${target_name})

    list(POP_BACK CMAKE_MESSAGE_INDENT)

endfunction()

function(include_component component_name files_to_include)
    message(STATUS ${component_name})
    list(APPEND CMAKE_MESSAGE_INDENT "  --> ")

    foreach(path IN LISTS files_to_include)
        message(VERBOSE "including: ${path}")
        include("${path}")
    endforeach()

    list(POP_BACK CMAKE_MESSAGE_INDENT)
endfunction()


function(include_target_with_path target_name target_path)
    if(TARGET ${target_name})
        return()
    endif()

    message(STATUS ${target_name})
    list(APPEND CMAKE_MESSAGE_INDENT " - ")
    include(${target_path})
    list(POP_BACK CMAKE_MESSAGE_INDENT)

endfunction()
