cmake_minimum_required(VERSION 3.21)
project(dflowfm_io LANGUAGES CXX Fortran)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(dflowfm_io SHARED
    include/dflowfm_io/MduModel.h
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

    src/MduModel.cpp
    src/dflowfm_io.cpp
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

add_subdirectory(test)

