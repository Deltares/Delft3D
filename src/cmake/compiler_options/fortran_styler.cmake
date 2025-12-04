function(check_style_double_precision target_name)
    if (NOT Python_FOUND)
        find_package(Python COMPONENTS Interpreter REQUIRED)
    endif()
    get_target_property(target_source_dir ${target_name} SOURCE_DIR)
    add_custom_command(TARGET ${target_name}
                       PRE_BUILD
                       COMMAND Python::Interpreter fortran_styler.py "--check" "--directory" "${target_source_dir}"
                       WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/../../tools/deltares_fortran_styler/src/deltares_fortran_styler
                       VERBATIM
                       )
endfunction()
