# --- Generated unc_put_var_map ---
if(NOT Python_FOUND)
    find_package(Python COMPONENTS Interpreter REQUIRED)
endif()

set(unc_put_var_map_output    "${CMAKE_CURRENT_BINARY_DIR}/generated/m_unc_put_var_map_generated.f90")
set(unc_put_var_map_generator "${CMAKE_CURRENT_SOURCE_DIR}/src/python/generate_unc_put_var_map.py")

add_custom_command(
    OUTPUT  "${unc_put_var_map_output}"
    COMMAND ${Python_EXECUTABLE} ${unc_put_var_map_generator} ${unc_put_var_map_output}
    DEPENDS "${unc_put_var_map_generator}"
    COMMENT "Regenerating m_unc_put_var_map_generated.f90"
    VERBATIM
)

add_custom_target(generate_unc_put_var_map DEPENDS "${unc_put_var_map_output}")
set_target_properties(generate_unc_put_var_map PROPERTIES FOLDER engines_gpl/dflowfm)
set_source_files_properties("${unc_put_var_map_output}" PROPERTIES GENERATED TRUE)
