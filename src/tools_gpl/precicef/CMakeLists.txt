# Gather source files
set(source_files src/precice.F90)

add_library(precicef STATIC ${source_files})

if(WIN32)
    find_package(precice REQUIRED CONFIG PATHS "${CMAKE_SOURCE_DIR}/../third_party_open/precice/precice-3_3_0_modified/lib/cmake/precice")
else()
# Find preCICE library
    find_package(precice REQUIRED CONFIG)
endif()

target_link_libraries(precicef PUBLIC precice::precice)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (precicef PROPERTIES FOLDER tools_gpl/precicef)
