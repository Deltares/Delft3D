# Generate the MDU schema C++ source from mdu.json at build time.
find_package(Python3 REQUIRED COMPONENTS Interpreter)

set(MDU_SCHEMA_JSON    ${CMAKE_CURRENT_SOURCE_DIR}/../json/mdu.json)
set(MDU_SCHEMA_SCRIPT  ${CMAKE_CURRENT_SOURCE_DIR}/scripts/generate_mdu_schema.py)
set(MDU_SCHEMA_GENERATED ${CMAKE_CURRENT_BINARY_DIR}/generated/MduSchema.cpp)

add_custom_command(
    OUTPUT  ${MDU_SCHEMA_GENERATED}
    COMMAND ${Python3_EXECUTABLE} ${MDU_SCHEMA_SCRIPT} ${MDU_SCHEMA_JSON} ${MDU_SCHEMA_GENERATED}
    DEPENDS ${MDU_SCHEMA_SCRIPT} ${MDU_SCHEMA_JSON}
    COMMENT "Generating MduSchema.cpp from mdu.json"
    VERBATIM
)

set_source_files_properties(${MDU_SCHEMA_GENERATED} PROPERTIES GENERATED TRUE)

add_library(dflowfm_io STATIC
    include/dflowfm_io/IssueReport.h
    include/dflowfm_io/MduData.h
    include/dflowfm_io/MduDataConverter.h
    include/dflowfm_io/MduDocument.h
    include/dflowfm_io/MduSchema.h
    include/dflowfm_io/MduSchemaIndex.h
    include/dflowfm_io/MduValidator.h
    include/dflowfm_io/MduValueConverter.h
    include/dflowfm_io/StringUtils.h

    src/IssueReport.cpp
    src/MduData.cpp
    src/MduDataConverter.cpp
    src/MduDocument.cpp
    ${MDU_SCHEMA_GENERATED}
    src/MduSchemaIndex.cpp
    src/MduValidator.cpp
    src/MduValueConverter.cpp
)

target_link_libraries(dflowfm_io
    PRIVATE dflowfm_ini_parser
)

target_include_directories(dflowfm_io
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
)

add_subdirectory(test)

