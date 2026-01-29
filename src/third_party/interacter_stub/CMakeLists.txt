set(library_name interacter_stub)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
