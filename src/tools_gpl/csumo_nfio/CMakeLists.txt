# Gather source files
set(source_files src/coupled.f90
                 src/determine_densprof.f90
                 src/nfl_data.f90
			     src/read_cosumo_file.f90
			     src/read_nf2ff_files.f90
			     src/write_ff2nf_files.f90
)

add_library(csumo_nfio STATIC ${source_files})

target_link_libraries(csumo_nfio PRIVATE ec_module)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (csumo_nfio PROPERTIES FOLDER tools_gpl/csumo_nfio)
