add_library(precice_fortran precice.F90)

if(UNIX)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(precice REQUIRED IMPORTED_TARGET libprecice)
    target_link_libraries(precice_fortran PRIVATE PkgConfig::precice)
endif(UNIX)

if(WIN32)
    add_library(precice SHARED IMPORTED)
    set_target_properties(precice PROPERTIES
        IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/bin/libprecice.dll"
        IMPORTED_IMPLIB "${CMAKE_CURRENT_SOURCE_DIR}/lib/libprecice.dll.a"
    )
    target_link_libraries(precice_fortran PRIVATE precice)
    file(GLOB precice_dll_files "${CMAKE_CURRENT_SOURCE_DIR}/bin/*.dll")
    install(FILES ${precice_dll_files} DESTINATION lib)
endif(WIN32)
