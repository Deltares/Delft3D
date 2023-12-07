set(library_name interacter_stub)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
