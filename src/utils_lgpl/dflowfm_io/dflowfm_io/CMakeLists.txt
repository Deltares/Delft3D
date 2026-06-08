add_library(dflowfm_io SHARED
    include/dflowfm_io/ConversionResult.h
    include/dflowfm_io/IssueReport.h
    include/dflowfm_io/MduConverter.h
    include/dflowfm_io/MduFile.h
    include/dflowfm_io/MduModel.h
    include/dflowfm_io/MduSchema.h
    include/dflowfm_io/StringUtils.h

    src/dflowfm_io.cpp
    src/IssueReport.cpp
    src/MduConverter.cpp
    src/MduFile.cpp
    src/MduModel.cpp
)

target_link_libraries(dflowfm_io
    PRIVATE dflowfm_ini_parser
)

include(GenerateExportHeader)
generate_export_header(dflowfm_io
    EXPORT_FILE_NAME ${CMAKE_CURRENT_BINARY_DIR}/dflowfm_io/dflowfm_io_export.h
)

target_include_directories(dflowfm_io
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_BINARY_DIR}
)

add_subdirectory(wrappers/fortran)

add_subdirectory(test)

