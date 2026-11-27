# Generate the MDU schema C++ source from mdu.json at build time.
find_package(Python3 REQUIRED COMPONENTS Interpreter)

set(MDU_SCHEMA_SCRIPT  ${CMAKE_CURRENT_SOURCE_DIR}/scripts/generate_mdu_schema.py)
set(MDU_SCHEMA_GENERATED ${CMAKE_CURRENT_BINARY_DIR}/generated/dflowfm_io/MduSchemaGenerated.h)

add_custom_command(
    OUTPUT  ${MDU_SCHEMA_GENERATED}
    COMMAND ${Python3_EXECUTABLE} ${MDU_SCHEMA_SCRIPT} ${DFLOWFM_IO_MDU_JSON} ${MDU_SCHEMA_GENERATED}
    DEPENDS ${MDU_SCHEMA_SCRIPT} ${DFLOWFM_IO_MDU_JSON}
    COMMENT "Generating MduSchemaGenerated.h from mdu.json"
    VERBATIM
)

set_source_files_properties(${MDU_SCHEMA_GENERATED} PROPERTIES GENERATED TRUE)

# Folder with the internal (non-public) headers. Visible to this directory and its subdirectories (e.g. test).
set(DFLOWFM_IO_PRIVATE_HEADER_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/private_headers)

add_library(dflowfm_io STATIC
    include/dflowfm_io/IssueReport.h
    include/dflowfm_io/MduDocument.h
    include/dflowfm_io/MduSchema.h
    ${MDU_SCHEMA_GENERATED}

    # Internal headers; not part of the public API.
    src/private_headers/MduConversion.h
    src/private_headers/MduData.h
    src/private_headers/MduValidation.h
    src/private_headers/StringUtils.h

    src/IssueReport.cpp
    src/MduConversion.cpp
    src/MduData.cpp
    src/MduDocument.cpp
    src/MduSchema.cpp
    src/MduValidation.cpp
)

target_link_libraries(dflowfm_io
    PRIVATE dflowfm_ini_parser
)

target_include_directories(dflowfm_io
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
    PRIVATE
        ${DFLOWFM_IO_PRIVATE_HEADER_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/generated
)

add_subdirectory(test)
