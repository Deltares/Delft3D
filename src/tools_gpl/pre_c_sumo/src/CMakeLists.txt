set(source_files
    endpoints.cpp
    pre_c_sumo_lib.cpp
    coupling_steps.cpp
    csumo_settings_reader.cpp
    FF2NF_writer.cpp
    parsing_utils.cpp
)
set(include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(header_files
    ${include_dir}/pre_c_sumo_lib.hpp
    ${include_dir}/coupling_steps.hpp
    ${include_dir}/csumo_settings_reader.hpp
    ${include_dir}/endpoints.hpp
    ${include_dir}/monadic_utils.hpp
    ${include_dir}/FF2NF_writer.hpp
    ${include_dir}/parsing_utils.hpp
    ${include_dir}/parsing_types.hpp
)

add_library(pre_c_sumo_lib ${source_files} ${header_files})

target_link_libraries(pre_c_sumo_lib PRIVATE
    compiler_warnings_settings
    precice::precice
    pugixml::pugixml
    Boost::headers
)

target_include_directories(pre_c_sumo_lib PUBLIC ${include_dir})

add_executable(preC-SUMO main.cpp)
target_link_libraries(preC-SUMO PRIVATE
    compiler_warnings_settings
    pre_c_sumo_lib
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
set_target_properties(preC-SUMO pre_c_sumo_lib PROPERTIES FOLDER tools_gpl/pre_c_sumo)

install(TARGETS preC-SUMO
    RUNTIME DESTINATION bin
)
