# Define the library 
set(library_name interacter_stub)

create_library(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)

set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)

