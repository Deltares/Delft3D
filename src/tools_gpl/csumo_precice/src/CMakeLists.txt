set(SOURCE_FILES main.cpp)

add_executable(csumo_precice ${SOURCE_FILES})
target_link_libraries(csumo_precice PRIVATE compiler_warnings_settings)
target_include_directories(csumo_precice PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../include)
