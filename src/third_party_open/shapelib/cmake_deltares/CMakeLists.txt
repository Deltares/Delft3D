set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/../src")

set(headers ${source_path}/shapefil.h)
set(source  ${source_path}/dbfopen.c
            ${source_path}/safileio.c
            ${source_path}/sbnsearch.c
            ${source_path}/shapelibint.c
            ${source_path}/shpopen.c
            ${source_path}/shptree.c)

set(library_name shp)
add_library(${library_name} ${headers}
                            ${source})

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE _LIB)

# Create the folder structure in project
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${source} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/fortrangis)
