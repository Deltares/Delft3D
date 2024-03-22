message(STATUS "pthreads")
# Set the path to the directory which contains all the source files and definitions
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/bin/x64/")
if (WIN32)
	install(DIRECTORY ${library_path} DESTINATION lib 
	FILES_MATCHING 
	PATTERN "*.dll" )
	install(DIRECTORY ${library_path} DESTINATION bin 
	FILES_MATCHING 
	PATTERN "*.exe" )
endif(WIN32)