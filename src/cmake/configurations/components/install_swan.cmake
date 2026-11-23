# Shared by the SWAN and D-Waves configurations, which can occur in one suite.
include_guard(GLOBAL)

set(swan_install_configurations ${CMAKE_CONFIGURATION_TYPES})
if(NOT swan_install_configurations)
    set(swan_install_configurations ${CMAKE_BUILD_TYPE})
endif()
if(NOT swan_install_configurations)
    set(swan_install_configurations Release)
endif()

get_cmake_property(swan_cmake_variables VARIABLES)
foreach(swan_variant IN ITEMS mpi omp)
    set(swan_package swan_${swan_variant})
    string(TOUPPER "${swan_variant}" swan_variant_upper)
    if(NOT TARGET SWAN_${swan_variant_upper}::SWAN_${swan_variant_upper})
        message(FATAL_ERROR "The Conan target for ${swan_package} is missing.")
    endif()

    # Dependencies may be built in Release even when Delft3D is built in Debug.
    set(swan_package_configurations)
    foreach(swan_variable IN LISTS swan_cmake_variables)
        if(swan_variable MATCHES "^${swan_package}_PACKAGE_FOLDER_(.+)$")
            list(APPEND swan_package_configurations "${CMAKE_MATCH_1}")
        endif()
    endforeach()
    if(NOT swan_package_configurations)
        message(FATAL_ERROR "Conan did not provide a package folder for ${swan_package}.")
    endif()
    list(GET swan_package_configurations 0 swan_fallback_configuration)
    if("RELEASE" IN_LIST swan_package_configurations)
        set(swan_fallback_configuration RELEASE)
    endif()

    foreach(swan_configuration IN LISTS swan_install_configurations)
        string(TOUPPER "${swan_configuration}" swan_package_configuration)
        if(NOT swan_package_configuration IN_LIST swan_package_configurations)
            set(swan_package_configuration ${swan_fallback_configuration})
        endif()
        set(swan_package_folder "${${swan_package}_PACKAGE_FOLDER_${swan_package_configuration}}")

        # Upstream SWAN also adds .exe on Linux. Keep Delft3D's installed names.
        find_program(swan_executable
            NAMES ${swan_package}${CMAKE_EXECUTABLE_SUFFIX} ${swan_package}.exe
            PATHS "${swan_package_folder}/bin"
            NO_DEFAULT_PATH NO_CACHE REQUIRED)
        install(PROGRAMS "${swan_executable}"
            DESTINATION bin RENAME ${swan_package}${CMAKE_EXECUTABLE_SUFFIX}
            CONFIGURATIONS ${swan_configuration})
        if(WIN32)
            install(DIRECTORY "${swan_package_folder}/bin/"
                DESTINATION bin CONFIGURATIONS ${swan_configuration}
                FILES_MATCHING PATTERN "*.dll")
        endif()
        unset(swan_executable)
    endforeach()
endforeach()

# The launchers belong to Delft3D-WAVE, not to the upstream SWAN packages.
install(PROGRAMS "${checkout_src_root}/engines_gpl/wave/scripts/swan.${platform_extension}"
    DESTINATION bin)