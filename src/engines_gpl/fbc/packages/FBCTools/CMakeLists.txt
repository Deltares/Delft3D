# CMake settings
cmake_minimum_required(VERSION 3.10.0)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/macros)

# Project name
project(FBCTools)

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  # when makeing a relocatable / standalone install make sure the libraries can be found in the install dir (or $ORIGIN)
  set(CMAKE_INSTALL_RPATH "$ORIGIN:$ORIGIN/../lib")
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
endif()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

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
set(Boost_USE_RELEASE_LIBS ON)

# version 1.65 is a minimum version
if(LINUX)
   find_package(Boost 1.65 REQUIRED COMPONENTS system filesystem)
   find_package(XercesC REQUIRED)
   if(XercesC_FOUND)
      message(STATUS "Found Xerces-C: ${XercesC_VERSION}")
      include_directories(${XercesC_INCLUDE_DIRS})
   else()
      message(FATAL_ERROR "Xerces-C not found")
   endif()
else()
   set(BOOST_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/thirdParty/boost)
   set(BOOST_LIBRARY_DIR ${BOOST_ROOT}/stage/lib)
endif()
link_directories(${BOOST_LIBRARY_DIR})
include_directories(${BOOST_ROOT})

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

set (version_include_dir ${PROJECT_SOURCE_DIR}/../../version/)

# Add project include directories

include_directories("${CMAKE_SOURCE_DIR}/../version_includes")
include_directories("${version_include_dir}")
include_directories("${PROJECT_SOURCE_DIR}/src")
include_directories("${PROJECT_SOURCE_DIR}/src/dataBinding")
include_directories("${PROJECT_SOURCE_DIR}/src/optimizationProblem")
include_directories("${PROJECT_SOURCE_DIR}/src/postprocess")
include_directories("${PROJECT_SOURCE_DIR}/src/schematization")
include_directories("${PROJECT_SOURCE_DIR}/src/timeseries")
include_directories("${PROJECT_SOURCE_DIR}/src/utilities")


if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xsd/xsd-3.3.0-i686-windows/libxsd")
    include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/include")
    link_directories("${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/lib/")
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    include_directories("${PROJECT_SOURCE_DIR}/thirdParty/xsd/xsd-3.3.0-x86_64-linux-gnu/libxsd")
endif()

# Add source folder (FBCTools_static target)
add_subdirectory(src)


if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
# Define FBCTools executable
    add_executable(FBCTools src/rtcTools.cpp ${version_include_dir}/fbc_version.rc)

# Define BMI library
    add_library(FBCTools_BMI SHARED src/rtcToolsBMI.cpp src/bmi.h ${version_include_dir}/fbc_version.rc)

elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
# Define FBCTools executable
    add_executable(FBCTools src/rtcTools.cpp)

# Define BMI library
    add_library(FBCTools_BMI SHARED src/rtcToolsBMI.cpp src/bmi.h)
endif()
    
# Link dependencies
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(LINK_LIBRARIES FBCTools_static 
                       xerces-c_3)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set(LINK_LIBRARIES FBCTools_static 
                       ${XercesC_LIBRARIES}
                       dl
                       boost_system
                       boost_date_time
                       boost_filesystem)
endif()

if(${CMAKE_COMPILER_IS_GNUCXX}) 
    set(LINK_LIBRARIES ${LINK_LIBRARIES} gomp)
endif()

target_link_libraries(FBCTools PRIVATE ${LINK_LIBRARIES})
target_link_libraries(FBCTools_BMI PRIVATE ${LINK_LIBRARIES})
#target_link_libraries(FBCTools_BMI PRIVATE ${Boost_LIBRARIES})
# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (FBCTools PROPERTIES FOLDER engines_gpl/fbc)
set_target_properties (FBCTools_BMI PROPERTIES FOLDER engines_gpl/fbc)

#------------------------------
# configure installer for linux
set(FBC_SUPPORT_FILES
  ${PROJECT_SOURCE_DIR}/xsd/pi_diag.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_modelparameters.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_run.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_sharedtypes.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_state.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_timeseries.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcDataConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcObjectiveConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcRuntimeConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcSharedTypes.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcStateConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcToolsConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/treeVector.xsd
)
  install(FILES ${FBC_SUPPORT_FILES} DESTINATION share/drtc)
  install(TARGETS FBCTools RUNTIME DESTINATION bin)
  install(TARGETS FBCTools_BMI RUNTIME DESTINATION lib)
  if(WIN32)
     install(FILES ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-windows-vc-10.0/bin/xerces-c_3_1.dll DESTINATION lib)
  endif()
