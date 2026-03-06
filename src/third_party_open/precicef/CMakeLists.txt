# Gather source files
set(source_files src/precice.F90)

add_library(precicef STATIC ${source_files})

target_link_libraries(precicef PRIVATE precice::precice)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (precicef PROPERTIES FOLDER third_party_open/precicef)
