set(source_files main.cpp)

add_executable(csumo_precice ${source_files})
target_link_libraries(csumo_precice PRIVATE compiler_warnings_settings)
target_include_directories(csumo_precice PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../include)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (csumo_precice PROPERTIES FOLDER tools_gpl/csumo_precice)

install(TARGETS csumo_precice
    RUNTIME DESTINATION bin
)
