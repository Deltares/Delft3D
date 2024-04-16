message(STATUS "Tecplot")
# Set the path to the directory which contains all the source files and definitions
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/lib/x64/")
if (WIN32)
	install(DIRECTORY ${library_path} DESTINATION bin 
	FILES_MATCHING 
	PATTERN "*.exe")
	install(DIRECTORY ${library_path} DESTINATION lib
	FILES_MATCHING 
	PATTERN "*.dll" )
endif(WIN32)