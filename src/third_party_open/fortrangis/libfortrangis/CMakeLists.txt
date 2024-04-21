# CMake file for the fortrangis library

#message(STATUS "Shapelib: ${HAVE_SHAPELIB}")

set(fortrangis_LIB_SRCS)
set(fortrangis_LIBS fortranGIS fortranc)

if(SHAPELIB_LIBRARY)
    set(fortrangis_LIB_SRCS
        shapelib.F90
        )
    set(fortrangisc_LIB_SRCS
        shapelibint.c
        )
    list(APPEND fortrangis_LIBS ${SHAPELIB_LIBRARY})

endif(SHAPELIB_LIBRARY)

if(PROJ_LIBRARY)
    list(APPEND fortrangis_LIB_SRCS
        proj.F90
        )
    list(APPEND fortrangis_LIBS ${PROJ_LIBRARY})
endif(PROJ_LIBRARY)

if(GDAL_LIBRARY)
    list(APPEND fortrangis_LIB_SRCS
        gdal.F90
        )
    list(APPEND fortrangis_LIBS ${GDAL_LIBRARY})
endif(GDAL_LIBRARY)

if(READOSM_LIBRARY)
    list(APPEND fortrangis_LIB_SRCS
        readosm.F90
        )
    list(APPEND fortrangis_LIBS ${READOSM_LIBRARY})
endif(READOSM_LIBRARY)

message(STATUS "${fortrangis_LIB_SRCS}")

add_library(fortranGIS ${fortrangis_LIB_SRCS} ${fortrangisc_LIB_SRCS})
target_link_libraries(fortranGIS PRIVATE fortranc ${SHAPELIB_LIBRARY})
add_library(${PROJECT_NAME}::fortranGIS ALIAS fortranGIS)

set_target_properties(fortranGIS PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(fortranGIS PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules> PRIVATE
  ${Shapelib_INCLUDE_DIR})

if(SHAPELIB_LIBRARY)
   add_executable(shapelib_test shapelib_test.F90)
   add_executable(shapelib_dump shapelib_dump.F90)
   add_executable(shp_fxtr shp_fxtr.F90)
   add_executable(ung2shp ung2shp.F90)
   target_link_libraries(shapelib_test ${fortrangis_LIBS})
   target_link_libraries(shapelib_dump ${fortrangis_LIBS})
   target_link_libraries(shp_fxtr ${fortrangis_LIBS})
   target_link_libraries(ung2shp ${fortrangis_LIBS})
   add_test("Shapelib" shapelib_test)
endif(SHAPELIB_LIBRARY)

if(PROJ_LIBRARY)
   add_executable(proj_test proj_test.F90)
   target_link_libraries(proj_test ${fortrangis_LIBS})
   add_test("Proj" proj_test)
endif(PROJ_LIBRARY)

if(GDAL_LIBRARY)
   add_executable(gdal_test gdal_test.F90)
   target_link_libraries(gdal_test ${fortrangis_LIBS})
   add_test("Gdal" gdal_test)
endif(GDAL_LIBRARY)

if(READOSM_LIBRARY)
   add_executable(readosm_test readosm_test.F90)
   target_link_libraries(readosm_test ${fortrangis_LIBS})
   add_test("ReadOSM" readosm_test)
endif(READOSM_LIBRARY)

