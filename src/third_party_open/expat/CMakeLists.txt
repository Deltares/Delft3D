message(STATUS "expat")
# Set the path to the directory which contains all the source files and definitions
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/x64/x64/")
if (WIN32)
	install(FILES ${library_path}/$<IF:$<CONFIG:Debug>,Debug,Release>/libexpat.dll DESTINATION lib)
endif(WIN32)
