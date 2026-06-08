add_library(dflowfm_io_api SHARED
    include/dflowfm_io_api/dflowfm_io_api.h
    dflowfm_io_api.cpp
)

target_link_libraries(dflowfm_io_api
    PRIVATE dflowfm_io
)

include(GenerateExportHeader)
generate_export_header(dflowfm_io_api
    BASE_NAME dflowfm_io_api
    EXPORT_FILE_NAME ${CMAKE_CURRENT_BINARY_DIR}/dflowfm_io_api/dflowfm_io_api_export.h
)

target_include_directories(dflowfm_io_api
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_BINARY_DIR} # For the generated export header
)

include(CheckLanguage)
check_language(Fortran)
if(CMAKE_Fortran_COMPILER)
    enable_language(Fortran)
    add_subdirectory(wrappers/fortran)
endif()

add_subdirectory(test)
