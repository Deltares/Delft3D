install(CODE "
    include(\"${CMAKE_CURRENT_LIST_DIR}/../functions.cmake\")
    execute_process(
        COMMAND bash \"${CMAKE_CURRENT_SOURCE_DIR}/copy_libs.sh\" \"${CMAKE_INSTALL_PREFIX}/bin\"
        RESULT_VARIABLE result
    )
    if(NOT result EQUAL 0)
        message(FATAL_ERROR \"copy_libs.sh failed with exit code \${result}\")
    endif()

    set_rpath(\"${CMAKE_INSTALL_PREFIX}/bin\" \"$ORIGIN:$ORIGIN/../lib\")
    set_rpath(\"${CMAKE_INSTALL_PREFIX}/lib\" \"$ORIGIN\")
    set_rpath(\"${CMAKE_INSTALL_PREFIX}/share\" \"$ORIGIN/../lib:$ORIGIN\")
")