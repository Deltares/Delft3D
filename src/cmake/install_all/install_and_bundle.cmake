IF(UNIX)
include(BundleUtilities)

function(add_optional_dependency target dep)
    if(TARGET ${dep})
        add_dependencies(${target} ${dep})
    else()
        message(STATUS "Target ${dep} not found; skipping dependency.")
    endif()
endfunction()

add_optional_dependency(bundle_all dflowfm_cli_exe)
add_optional_dependency(bundle_all dimr)
add_optional_dependency(bundle_all FBCTools)
add_optional_dependency(bundle_all wave)
add_optional_dependency(bundle_all d_hydro)
add_optional_dependency(bundle_all delwaq)
add_optional_dependency(bundle_all rr)


set(VALID_EXECUTABLES "dflowfm-cli.exe dimr.exe")
foreach(EXE IN LISTS EXECUTABLES)
    if(EXISTS "${EXE}")
        list(APPEND VALID_EXECUTABLES "${EXE}")
    else()
        message(WARNING "Executable not found: ${EXE}")
    endif()
endforeach()

if(VALID_EXECUTABLES)
    get_bundle_all_dependencies("${VALID_EXECUTABLES}" "${CMAKE_INSTALL_PREFIX}/bin" "" DEPENDENCIES)
    copy_resolved_dependencies("${DEPENDENCIES}" "${CMAKE_INSTALL_PREFIX}/bin" "")
endif()



ENDIF(UNIX)