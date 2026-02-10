set(source_files csumo_precice_lib.cpp)
set(include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(header_files ${include_dir}/csumo_precice_lib.hpp)

add_library(csumo_precice_lib ${source_files})
target_link_libraries(csumo_precice_lib 
    PRIVATE compiler_warnings_settings
    PUBLIC precice::precice
)
target_include_directories(csumo_precice_lib PUBLIC ${include_dir})

add_executable(csumo_precice main.cpp)
target_link_libraries(csumo_precice 
    PRIVATE compiler_warnings_settings 
    PUBLIC csumo_precice_lib
)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files} main.cpp)
set_target_properties(csumo_precice csumo_precice_lib PROPERTIES FOLDER tools_gpl/csumo_precice)

install(TARGETS csumo_precice
    RUNTIME DESTINATION bin
)
