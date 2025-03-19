message(STATUS "GISInternals")
# Set the path to the directory which contains all the source files and definitions
set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/release-1911-x64/bin/")
if (WIN32)
	install(FILES 
	${library_path}/xerces-c_3_2.dll
	${library_path}/gdal300.dll
	${library_path}/expat.dll      
	${library_path}/libpq.dll      
	${library_path}/sqlite3.dll    
	${library_path}/libmysql.dll   
	${library_path}/spatialite.dll 
	${library_path}/proj.dll       
	${library_path}/proj_6_1.dll   
	${library_path}/openjp2.dll    
	${library_path}/geos_c.dll    
	${library_path}/libxml2.dll    
	${library_path}/iconv.dll     
	${library_path}/geos.dll     
	${library_path}/freexl.dll    
	DESTINATION lib)
endif(WIN32)