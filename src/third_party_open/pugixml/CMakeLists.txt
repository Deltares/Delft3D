if(WIN32)
    find_package(pugixml CONFIG REQUIRED PATHS "${CMAKE_CURRENT_SOURCE_DIR}/pugixml-1.15/lib/cmake/pugixml" GLOBAL)
    set_target_properties(pugixml::pugixml pugixml::shared PROPERTIES
        MAP_IMPORTED_CONFIG_DEBUG Release
        MAP_IMPORTED_CONFIG_RELWITHDEBINFO Release
    )

    install(IMPORTED_RUNTIME_ARTIFACTS
            pugixml::shared
            RUNTIME DESTINATION lib
    )
endif()

if(UNIX)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(pugixml REQUIRED IMPORTED_TARGET pugixml GLOBAL)
    add_library(pugixml::pugixml ALIAS PkgConfig::pugixml)

    install(FILES
            ${pugixml_LINK_LIBRARIES}
            DESTINATION lib
    )
endif()
