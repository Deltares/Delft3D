# ZLIBConfig.cmake

# Specify the version of Zlib
set(ZLIB_VERSION 1.3.1)

# Specify the include directory
set(ZLIB_INCLUDE_DIR "${CMAKE_CURRENT_LIST_DIR}/../include")

# Specify the library directory
set(ZLIB_LIBRARY "${CMAKE_CURRENT_LIST_DIR}/../lib/zlib.lib")
set(ZLIB_STATIC_LIBRARY "${CMAKE_CURRENT_LIST_DIR}/../lib/zlibstatic.lib")

# Create imported targets
add_library(ZLIB::ZLIB SHARED IMPORTED)
set_target_properties(ZLIB::ZLIB PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${ZLIB_INCLUDE_DIR}"
    IMPORTED_LOCATION "${ZLIB_LIBRARY}"
    IMPORTED_LOCATION "${ZLIB_STATIC_LIBRARY}"
)
set_target_properties(ZLIB::ZLIB PROPERTIES
    IMPORTED_IMPLIB_RELEASE "${ZLIB_LIBRARY}"
    MAP_IMPORTED_CONFIG_MINSIZEREL Release
    MAP_IMPORTED_CONFIG_RELWITHDEBINFO Release
    MAP_IMPORTED_CONFIG_DEBUG Release
)