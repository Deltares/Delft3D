set(source_files
    pre_c_sumo_lib.cpp
    precice_sink_source_connections.cpp
    csumo_exchange.cpp
    precice_to_csumo.cpp
    csumo_to_precice.cpp
    csumo_diffuser.cpp
    csumo_file_paths.cpp
    precice_data.cpp
    precice_exchange.cpp
    file_io.cpp
    csumo_xml_utils.cpp
    csumo_settings_reader.cpp
    csumo_ff2nf_writer.cpp
    csumo_nf2ff_reader.cpp
    xml_parsing_utils.cpp
)
set(include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(header_files
    ${include_dir}/pre_c_sumo_lib.hpp
    ${include_dir}/precice_sink_source_connections.hpp
    ${include_dir}/csumo_exchange.hpp
    ${include_dir}/precice_to_csumo.hpp
    ${include_dir}/csumo_to_precice.hpp
    ${include_dir}/csumo_diffuser.hpp
    ${include_dir}/csumo_file_paths.hpp
    ${include_dir}/csumo_data.hpp
    ${include_dir}/precice_data.hpp
    ${include_dir}/precice_exchange.hpp
    ${include_dir}/file_io.hpp
    ${include_dir}/io_errors.hpp
    ${include_dir}/csumo_xml_utils.hpp
    ${include_dir}/csumo_settings_reader.hpp
    ${include_dir}/monadic_utils.hpp
    ${include_dir}/csumo_ff2nf_writer.hpp
    ${include_dir}/csumo_nf2ff_reader.hpp
    ${include_dir}/xml_parsing_utils.hpp
)

add_library(pre_c_sumo_lib ${source_files} ${header_files})

target_link_libraries(pre_c_sumo_lib PRIVATE
    compiler_warnings_settings
    precice::precice
    Boost::headers
)
target_link_libraries(pre_c_sumo_lib PUBLIC
    pugixml::pugixml
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

install(TARGETS preC-SUMO RUNTIME DESTINATION bin)
install(FILES $<TARGET_RUNTIME_DLLS:preC-SUMO> DESTINATION bin)
