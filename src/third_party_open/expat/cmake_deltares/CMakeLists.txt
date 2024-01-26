if(UNIX)

   # Set the source path where the library components are located
   set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

   set(source_path "${library_path}/lib")

   # Include directories that are necessary references
   include_directories(${source_path})

   # add source files
   file(GLOB c_sources ${source_path}/*.c)

   # Add library
   add_library(expat ${c_sources})
   add_definitions(-DHAVE_BCOPY)

endif(UNIX)
if (CMAKE_GENERATOR MATCHES "Visual Studio")
   # Create the vfproj structure
   source_group(${visual_studio_source_group_name} FILES ${c_sources})
endif()
