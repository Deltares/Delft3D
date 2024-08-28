# Add this directory for internal users.
include_directories(.)
# Find sources.
file(GLOB metis_sources *.c)
# Build libmetis.
add_library(metis ${METIS_LIBRARY_TYPE} ${GKlib_sources} ${metis_sources})

target_link_libraries(metis PRIVATE no_compiler_warnings)

if(UNIX)
  target_link_libraries(metis PRIVATE m)
endif()

if(METIS_INSTALL)
  install(TARGETS metis
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION lib
    ARCHIVE DESTINATION lib)
endif()

set_target_properties (metis PROPERTIES FOLDER third_party_open/metis)
