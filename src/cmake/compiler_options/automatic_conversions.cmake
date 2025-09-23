function(check_style_double_precision target_name)
    if (NOT Python_FOUND)
        find_package(Python COMPONENTS Interpreter REQUIRED)
    endif()
    get_target_property(target_source_dir ${target_name} SOURCE_DIR)
    add_custom_command(TARGET ${target_name}
                       PRE_BUILD
                       COMMAND Python::Interpreter convert_double.py "--check" "--directory" "${target_source_dir}"
                       WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/../../tools/double_precision_conversion
                       VERBATIM
                       )
endfunction()
