if (WIN32)
    find_package(pugixml CONFIG REQUIRED PATHS "${CMAKE_CURRENT_SOURCE_DIR}/pugixml-1.15/lib/cmake/pugixml" GLOBAL)
    set_target_properties(pugixml::pugixml PROPERTIES
        MAP_IMPORTED_CONFIG_DEBUG Release
        MAP_IMPORTED_CONFIG_RELWITHDEBINFO Release
    )
endif()
