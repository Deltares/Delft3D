set(source_files
    pre_c_sumo_lib.cpp
    conversions.cpp
    csumo/diffuser.cpp
    csumo/exchange.cpp
    csumo/ff2nf_writer.cpp
    csumo/file_paths.cpp
    csumo/nf2ff_reader.cpp
    csumo/settings_reader.cpp
    csumo/xml.cpp
    precice/data.cpp
    precice/exchange.cpp
    precice/sink_source_connections.cpp
    utils/file_io.cpp
    utils/xml_parsing.cpp
)
set(include_dir ${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(header_files
    ${include_dir}/pre_c_sumo_lib.hpp
    ${include_dir}/conversions.hpp
    ${include_dir}/csumo/data.hpp
    ${include_dir}/csumo/diffuser.hpp
    ${include_dir}/csumo/exchange.hpp
    ${include_dir}/csumo/ff2nf_writer.hpp
    ${include_dir}/csumo/file_paths.hpp
    ${include_dir}/csumo/nf2ff_reader.hpp
    ${include_dir}/csumo/settings_reader.hpp
    ${include_dir}/csumo/xml.hpp
    ${include_dir}/precice/data.hpp
    ${include_dir}/precice/exchange.hpp
    ${include_dir}/precice/sink_source_connections.hpp
)
set(utility_headers
    ${include_dir}/utils/file_io.hpp
    ${include_dir}/utils/io_errors.hpp
    ${include_dir}/utils/monadic.hpp
    ${include_dir}/utils/xml_parsing.hpp
)

add_library(pre_c_sumo_lib ${source_files} ${header_files} ${utility_headers})

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
source_group(TREE ${include_dir} PREFIX "Header Files" FILES ${header_files} ${utility_headers})
set_target_properties(preC-SUMO pre_c_sumo_lib PROPERTIES FOLDER tools_gpl/pre_c_sumo)

install(TARGETS preC-SUMO RUNTIME DESTINATION bin)
install(FILES $<TARGET_RUNTIME_DLLS:preC-SUMO> DESTINATION bin)
