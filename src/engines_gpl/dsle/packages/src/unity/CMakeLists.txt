set(unity_source_files
    unity_internals.h
    unity.h
    unity.c
)

add_library(Unity STATIC ${unity_source_files})
set_target_properties (Unity PROPERTIES FOLDER third_party_open)
target_include_directories(Unity PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_definitions(Unity PUBLIC UNITY_INCLUDE_DOUBLE)

set_target_properties(Unity PROPERTIES C_STANDARD 23)
