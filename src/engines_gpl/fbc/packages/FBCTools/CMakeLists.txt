# CMake settings
cmake_minimum_required(VERSION 3.0.0)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/macros)

# Project name
project(FBCTools)

# Set platform-dependent output folder
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
        set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/Debug")
    else()
        set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/Release")
    endif()
    set(DESTINATION_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
else()
    # Visual studio will have its own debug and release configuration
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
    set(DESTINATION_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>")
endif()

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  # when makeing a relocatable / standalone install make sure the libraries can be found in the install dir (or $ORIGIN)
  set(CMAKE_INSTALL_RPATH "$ORIGIN:$ORIGIN/../lib")
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
endif()

# Use C++11 if available
include(set_cxx_norm)
set_cxx_norm(CXX_NORM_CXX11 CXX_NORM_FLAGS)

add_compile_options("${CXX_NORM_FLAGS}")

# Enable position independent code for libraries
set(CMAKE_POSITION_INDEPENDENT_CODE TRUE)

# Enable OpenMP
find_package(OpenMP)
if(OPENMP_FOUND)
    add_compile_options("${OpenMP_CXX_FLAGS}")
endif()

# Boost C++, it is a required package, if not found configuration will abort
if(WIN32)
  # In windows use pre-compiled static libraries
  set(Boost_USE_STATIC_LIBS ON)
endif()
# version 1.65 is a minimum version
find_package(Boost 1.65 REQUIRED COMPONENTS system filesystem)
link_directories(${Boost_LIBRARY_DIRS})

# Set warning levels
if(MSVC)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /W3")
    set(CMAKE_C_FLAGS_DEBUG   "${CMAKE_C_FLAGS_DEBUG} /W3")
else()
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -std=c++11 -lboost_filesystem -lboost_system -lboost_date_time") 
    set(CMAKE_C_FLAGS_DEBUG   "${CMAKE_C_FLAGS_DEBUG} -Wall -std=c++11 -lboost_filesystem -lboost_system -lboost_date_time")
endif()

if(MSVC)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /W0")
    set(CMAKE_C_FLAGS_RELEASE   "${CMAKE_C_FLAGS_RELEASE} /W0")
else()
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -std=c++11 -lboost_filesystem -lboost_system -lboost_date_time") 
    set(CMAKE_C_FLAGS_RELEASE   "${CMAKE_C_FLAGS_RELEASE} -std=c++11 -lboost_filesystem -lboost_system -lboost_date_time")
endif()

# Optionally enable code coverage report generation
option(GENERATE_COVERAGE_REPORT "Generate code coverage report" OFF)
if(${GENERATE_COVERAGE_REPORT})
    include(CodeCoverage)

    add_compile_options("-g -O0 -fprofile-arcs -ftest-coverage")

    setup_target_for_coverage(coverage ${CMAKE_CTEST_COMMAND} coverage)
endif()

# Add project include directories
include_directories("${PROJECT_SOURCE_DIR}/src")
include_directories("${PROJECT_SOURCE_DIR}/src/dataBinding")
include_directories("${PROJECT_SOURCE_DIR}/src/optimizationProblem")
include_directories("${PROJECT_SOURCE_DIR}/src/postprocess")
include_directories("${PROJECT_SOURCE_DIR}/src/schematization")
include_directories("${PROJECT_SOURCE_DIR}/src/timeseries")
include_directories("${PROJECT_SOURCE_DIR}/src/utilities")


if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xsd/xsd-3.3.0-i686-windows/libxsd")
    if(${CMAKE_SIZEOF_VOID_P} MATCHES "8")
        include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/include")
        link_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/lib/")
    else()
        include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-windows-vc-10.0/include")
        link_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-windows-vc-10.0/lib/")
    endif()
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xsd/xsd-3.3.0-x86_64-linux-gnu/libxsd")
    if(${CMAKE_SIZEOF_VOID_P} MATCHES "8")
        include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/include")
        link_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/lib/")
    else()
        include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-linux-gcc-3.4/include")
        link_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-linux-gcc-3.4/lib/")
    endif()
endif()

# Add source folder (FBCTools_static target)
add_subdirectory(src)

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
# Define FBCTools executable
    add_executable(FBCTools src/rtcTools.cpp ${PROJECT_SOURCE_DIR}/src/version.rc)

# Define BMI library
    add_library(FBCTools_BMI SHARED src/rtcToolsBMI.cpp src/bmi.h ${PROJECT_SOURCE_DIR}/src/version.rc)

# Define OpenMI library
    add_library(FBCTools_OpenMI SHARED src/rtcToolsOpenMI.cpp ${PROJECT_SOURCE_DIR}/src/version.rc)

elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
# Define FBCTools executable
    add_executable(FBCTools src/rtcTools.cpp)

# Define BMI library
    add_library(FBCTools_BMI SHARED src/rtcToolsBMI.cpp src/bmi.h)

# Define OpenMI library
    add_library(FBCTools_OpenMI SHARED src/rtcToolsOpenMI.cpp)

endif()
    
# Link dependencies
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(LINK_LIBRARIES FBCTools_static 
                       xerces-c_3)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set(LINK_LIBRARIES FBCTools_static 
                       xerces-c
                       dl
                       boost_system
                       boost_date_time
                       boost_filesystem)
endif()

if(${CMAKE_COMPILER_IS_GNUCXX}) 
    set(LINK_LIBRARIES ${LINK_LIBRARIES} gomp)
endif()

target_link_libraries(FBCTools LINK_PUBLIC ${LINK_LIBRARIES} ${Boost_LIBRARIES})
target_link_libraries(FBCTools_BMI LINK_PUBLIC ${LINK_LIBRARIES} ${Boost_LIBRARIES})
target_link_libraries(FBCTools_OpenMI LINK_PUBLIC ${LINK_LIBRARIES} ${Boost_LIBRARIES})

target_include_directories(FBCTools PUBLIC ${Boost_LIBRARY_DIRS})
target_include_directories(FBCTools_BMI PUBLIC ${Boost_LIBRARY_DIRS})
target_include_directories(FBCTools_OpenMI PUBLIC ${Boost_LIBRARY_DIRS})

# Install project files into output folder
add_custom_command(TARGET FBCTools
                   POST_BUILD
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_diag.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_modelparameters.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_run.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_sharedtypes.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_state.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/pi_timeseries.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcDataConfig.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcObjectiveConfig.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcRuntimeConfig.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcSharedTypes.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcStateConfig.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/rtcToolsConfig.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/xsd/treeVector.xsd ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/GPLv2.txt ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/LICENSE.txt ${DESTINATION_DIR}
                   COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/release_notes.txt ${DESTINATION_DIR})

# Install library dependencies into output folder
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    STRING(REGEX REPLACE "/" "\\\\" WINDOWS_FORMAT_PROJECT_SOURCE_DIR ${PROJECT_SOURCE_DIR})
    if(${CMAKE_SIZEOF_VOID_P} MATCHES "8")
        add_custom_command(TARGET FBCTools
                           POST_BUILD
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/bin/xerces-c_3_1.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/vcomp/x64/vcomp100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/msvcr/x64/msvcr100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/msvcr140/x64/vcruntime140_1$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/msvcp100/x64/msvcp100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR})
    else()
        add_custom_command(TARGET FBCTools
                           POST_BUILD
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-windows-vc-10.0/bin/xerces-c_3_1.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-windows-vc-10.0/bin/xerces-c_3_1D.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/vcomp/x86/vcomp100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/msvcr/x86/msvcr100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_SOURCE_DIR}/thirdParty/msvcp100/x86/msvcp100$<$<CONFIG:Debug>:d>.dll ${DESTINATION_DIR})
    endif()
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    if(${CMAKE_SIZEOF_VOID_P} MATCHES "8")
        add_custom_command(TARGET FBCTools
                           POST_BUILD
                           COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/lib/libxerces-c-3.1.so ${DESTINATION_DIR})
    else()
        add_custom_command(TARGET FBCTools
                           POST_BUILD
                           COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-linux-gcc-3.4/lib/libxerces-c-3.1.so ${DESTINATION_DIR})
    endif()
    configure_file(src/RTCTools.sh.in src/RTCTools.sh)
    add_custom_command(TARGET FBCTools
                       POST_BUILD                      
                       COMMAND "${CMAKE_COMMAND}" -E copy ${PROJECT_BINARY_DIR}/src/RTCTools.sh ${DESTINATION_DIR})
endif()

# Optionally add OpenMI C# projects
option(BUILD_OPENMI_DOTNET_INTERFACE "Build OpenMI .NET interface" OFF)
if(${BUILD_OPENMI_DOTNET_INTERFACE})
    if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        include_external_msproject(RTCTools.OpenMI.Sdk.Backbone
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Backbone/RTCTools.OpenMI.Sdk.Backbone.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU)
        include_external_msproject(RTCTools.OpenMI.Sdk.Buffer
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Buffer/RTCTools.OpenMI.Sdk.Buffer.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU
                                   RTCTools.OpenMI.Sdk.Backbone)
        include_external_msproject(RTCTools.OpenMI.Sdk.DevelopmentSupport
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/DevelopmentSupport/RTCTools.OpenMI.Sdk.DevelopmentSupport.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU)
        include_external_msproject(RTCTools.OpenMI.Sdk.Spatial
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Spatial/RTCTools.OpenMI.Sdk.Spatial.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU
                                   RTCTools.OpenMI.Sdk.Backbone)
        include_external_msproject(RTCTools.OpenMI.Sdk.Wrapper
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Wrapper/RTCTools.OpenMI.Sdk.Wrapper.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU
                                   RTCTools.OpenMI.Sdk.Backbone RTCTools.OpenMI.Sdk.DevelopmentSupport RTCTools.OpenMI.Sdk.Spatial RTCTools.OpenMI.Sdk.Buffer)
        include_external_msproject(Deltares.RTCTools.Net
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/src/Deltares.RtcTools.Net/Deltares.RtcTools.Net.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU
                                   RTCTools.OpenMI.Sdk.Backbone RTCTools.OpenMI.Sdk.Buffer RTCTools.OpenMI.Sdk.DevelopmentSupport RTCTools.OpenMI.Sdk.Spatial RTCTools.OpenMI.Sdk.Wrapper)
        include_external_msproject(Deltares.RTCToolsWrapper
                                   ${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/src/Deltares.RtcToolsWrapper.csproj
                                   TYPE FAE04EC0-301F-11D3-BF4B-00C04F79EFBC
                                   PLATFORM AnyCPU
                                   Deltares.RTCTools.Net)

        add_dependencies(FBCTools Deltares.RTCToolsWrapper)

        add_custom_command(TARGET FBCTools
                           POST_BUILD
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/OpenMI.Standard/src/csharp/bin/OpenMI.Standard.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Backbone/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/RTCTools.OpenMI.Sdk.Backbone.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Buffer/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/RTCTools.OpenMI.Sdk.Buffer.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/DevelopmentSupport/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/RTCTools.OpenMI.Sdk.DevelopmentSupport.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Spatial/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/RTCTools.OpenMI.Sdk.Spatial.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/thirdParty/Oatc.SDK_1.4.0.0/Oatc/src/csharp/Sdk/Wrapper/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/RTCTools.OpenMI.Sdk.Wrapper.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/src/Deltares.RtcTools.Net/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/Deltares.RtcTools.Net.dll" ${DESTINATION_DIR}
                           COMMAND "${CMAKE_COMMAND}" -E copy "${PROJECT_SOURCE_DIR}/../FBCToolsOpenMI/src/obj/$<$<CONFIG:Debug>:Debug>$<$<CONFIG:Release>:Release>/Deltares.RtcToolsWrapper.dll" ${DESTINATION_DIR})
    endif()
endif()

#------------------------------
# configure installer for linux

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR}/install)
  install(TARGETS FBCTools DESTINATION bin)
  install(TARGETS FBCTools_BMI DESTINATION lib)
  install(TARGETS FBCTools_OpenMI DESTINATION lib)
  add_subdirectory("${PROJECT_SOURCE_DIR}/cmake/install" DFBC_install)
endif()

# TODO: copy tests from proprietary part of original RTCTools
    # Add tests from proprietary repository
    # add_subdirectory(${PROPRIETARY_REPOSITORY}/RTCTools/test proprietary-test)
