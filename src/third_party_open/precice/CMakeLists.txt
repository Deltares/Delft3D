if(UNIX)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(precice REQUIRED IMPORTED_TARGET libprecice)
    add_library(precice::precice ALIAS PkgConfig::precice)
    find_package(Boost REQUIRED COMPONENTS log log_setup program_options thread)
    find_package(LibXml2 REQUIRED)
    install(FILES
            ${precice_LINK_LIBRARIES}
            ${LIBXML2_LIBRARIES}
            DESTINATION lib
    )
endif(UNIX)

if(WIN32)
    find_package(precice CONFIG REQUIRED PATHS "${CMAKE_CURRENT_SOURCE_DIR}/precice-3_3_0_modified/lib/cmake/precice")
    set(Boost_USE_STATIC_LIBS OFF)
    find_package(Boost REQUIRED COMPONENTS log log_setup program_options thread PATHS "${CMAKE_CURRENT_SOURCE_DIR}/../boost/boost_1_90_0/lib/cmake")
    find_package(LibXml2 REQUIRED PATHS "${CMAKE_CURRENT_SOURCE_DIR}/../libxml2/libxml2-v2.15.1/lib/cmake/libxml2")
    install(IMPORTED_RUNTIME_ARTIFACTS
            precice::precice
            LibXml2::LibXml2
            RUNTIME DESTINATION lib
            LIBRARY DESTINATION lib
    )
endif(WIN32)

install(IMPORTED_RUNTIME_ARTIFACTS
        Boost::log
        Boost::log_setup
        Boost::program_options
        Boost::thread
        RUNTIME DESTINATION lib
        LIBRARY DESTINATION lib
)
