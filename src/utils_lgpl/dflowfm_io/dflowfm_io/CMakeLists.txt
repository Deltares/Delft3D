add_library(dflowfm_io STATIC
    include/dflowfm_io/IssueReport.h
    include/dflowfm_io/MduConverter.h
    include/dflowfm_io/MduData.h
    include/dflowfm_io/MduDocument.h
    include/dflowfm_io/MduSchema.h
    include/dflowfm_io/MduValidator.h
    include/dflowfm_io/StringUtils.h

    src/IssueReport.cpp
    src/MduConverter.cpp
    src/MduData.cpp
    src/MduDocument.cpp
    src/MduSchema.cpp
    src/MduValidator.cpp
)

target_link_libraries(dflowfm_io
    PRIVATE dflowfm_ini_parser
)

target_include_directories(dflowfm_io
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
)

add_subdirectory(test)

