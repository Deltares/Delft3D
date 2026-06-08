cmake_minimum_required(VERSION 3.21)
project(dflowfm_io LANGUAGES CXX Fortran)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(dflowfm_io SHARED
    include/dflowfm_io/ConversionResult.h
    include/dflowfm_io/IssueReport.h
    include/dflowfm_io/MduConverter.h
    include/dflowfm_io/MduFile.h
    include/dflowfm_io/MduModel.h
    include/dflowfm_io/MduSchema.h
    include/dflowfm_io/StringUtils.h
    include/ini/IniData.h
    include/ini/IniFile.h
    include/ini/IniFormatter.h
    include/ini/IniFormatterOptions.h
    include/ini/IniParser.h
    include/ini/IniParserOptions.h
    include/ini/IniProperty.h
    include/ini/IniScheme.h
    include/ini/IniSection.h
    include/ini/IniValueConverter.h
    include/ini/StringUtils.h

    src/dflowfm_io.cpp
    src/IssueReport.cpp
    src/MduConverter.cpp
    src/MduFile.cpp
    src/MduModel.cpp
    src/ini/IniData.cpp
    src/ini/IniFile.cpp
    src/ini/IniFormatter.cpp
    src/ini/IniParser.cpp
    src/ini/IniProperty.cpp
    src/ini/IniSection.cpp
    src/ini/IniValueConverter.cpp
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

add_executable(dflowfm_io_tool
    src/dflowfm_io_tool.cpp
)

target_link_libraries(dflowfm_io_tool
    PRIVATE dflowfm_io
)

target_include_directories(dflowfm_io_tool
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_BINARY_DIR}
)

add_subdirectory(test)

