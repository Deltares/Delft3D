add_library(dflowfm_ini_parser STATIC
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

    src/IniData.cpp
    src/IniFile.cpp
    src/IniFormatter.cpp
    src/IniParser.cpp
    src/IniProperty.cpp
    src/IniSection.cpp
    src/IniValueConverter.cpp
)

target_include_directories(dflowfm_ini_parser
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
)

add_subdirectory(test)
