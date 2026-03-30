set(source_files
    preC-SUMO_lib.cpp
    csumo_settings_reader.cpp
)
set(include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(header_files
    ${include_dir}/preC-SUMO_lib.hpp
    ${include_dir}/csumo_settings_reader.hpp
    ${include_dir}/monadic_utils.hpp
)

add_library(preC-SUMO_lib ${source_files} ${header_files})

target_link_libraries(preC-SUMO_lib PRIVATE
    compiler_warnings_settings
    precice::precice
    pugixml::pugixml
    Boost::headers
)

target_include_directories(preC-SUMO_lib PUBLIC ${include_dir})

add_executable(preC-SUMO main.cpp)
target_link_libraries(preC-SUMO PRIVATE
    compiler_warnings_settings
    preC-SUMO_lib
    Boost::program_options
)

# GCC 14 false-positive: at release optimisation levels it inlines deeply through
# boost::program_options::typed_value<string>::notify -> std::string::operator= ->
# basic_string.h and then fires -Wnull-dereference on GCC's own standard-library header.
# -Wno-error=null-dereference keeps the warning visible but prevents it from being a
# hard error, without disabling the check for the rest of the translation unit.
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(preC-SUMO PRIVATE -Wno-error=null-dereference)
endif()

if(CMAKE_GENERATOR MATCHES "Visual Studio" AND NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/preC-SUMO.vcxproj.$ENV{USERNAME}.user")
    configure_file(
    "${CMAKE_BINARY_DIR}/template.vcxproj.user"
    "${CMAKE_CURRENT_BINARY_DIR}/preC-SUMO.vcxproj.user"
    @ONLY
    )
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files} main.cpp)
set_target_properties(preC-SUMO preC-SUMO_lib PROPERTIES FOLDER tools_gpl/preC-SUMO)

install(TARGETS preC-SUMO
    RUNTIME DESTINATION bin
)
