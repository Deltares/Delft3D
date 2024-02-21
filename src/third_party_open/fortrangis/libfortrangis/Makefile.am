# moved from libfortrangis_ to AM_ to avoid private _FCFLAGS for
# programs, which break object file naming, similar to
# https://stackoverflow.com/questions/42226738/why-is-automake-prefixing-my-object-files-with-the-package-name
AM_FCFLAGS = -I../libfortranc -I../include
lib_LTLIBRARIES = libfortrangis.la
libfortrangis_la_SOURCES = 
libfortrangis_la_LIBADD = ../libfortranc/libfortranc.la
libfortrangis_la_LDFLAGS =
nodist_include_HEADERS = 
noinst_PROGRAMS =


if VERSIONINFO
libfortrangis_la_LDFLAGS += -version-info @VERSIONLIB@
endif
libfortrangis_la_LDFLAGS += -no-undefined

check_PROGRAMS =
TESTS = $(check_PROGRAMS)

if SHP
libfortrangis_la_SOURCES += shapelib.F90 shapelibint.c
libfortrangis_la_LIBADD += $(SHP_LIBS)
nodist_include_HEADERS += shapelib.mod

check_PROGRAMS += shapelib_test
shapelib_test_SOURCES = shapelib_test.F90
shapelib_test_LDADD = libfortrangis.la

noinst_PROGRAMS += ung2shp
ung2shp_SOURCES = ung2shp.F90
ung2shp_LDADD = libfortrangis.la
# add since F29, needed because of .mod, the dependency on
# shapelib.$(OBJEXT), more logical, does not work
ung2shp.o: libfortrangis.la
#ung2shp.o: shapelib.$(OBJEXT)

noinst_PROGRAMS += shapelib_dump
shapelib_dump_SOURCES = shapelib_dump.F90
shapelib_dump_LDADD = libfortrangis.la
shapelib_dump.o: libfortrangis.la

noinst_PROGRAMS += shp_fxtr
shp_fxtr_SOURCES = shp_fxtr.F90
shp_fxtr_LDADD = libfortrangis.la
shp_fxtr_dump.o: libfortrangis.la
endif

if PROJ
libfortrangis_la_LIBADD += $(PROJ_LIBS)
if PROJ4
libfortrangis_la_SOURCES += proj.F90
nodist_include_HEADERS += proj.mod
check_PROGRAMS += proj_test
proj_test_SOURCES = proj_test.F90
proj_test_FCFLAGS = -I../libfortranc
proj_test_LDADD = libfortrangis.la ../libfortranc/libfortranc.la $(PROJ_LIBS)
endif

if PROJ6
libfortrangis_la_SOURCES += proj6.F90
nodist_include_HEADERS += proj6.mod
check_PROGRAMS += proj6_test
proj6_test_SOURCES = proj6_test.F90
proj6_test_FCFLAGS = -I../libfortranc
proj6_test_LDADD = libfortrangis.la ../libfortranc/libfortranc.la $(PROJ_LIBS)
endif
endif

if GDAL
libfortrangis_la_SOURCES += gdal.F90 
libfortrangis_la_LIBADD += $(GDAL_LIBS)
nodist_include_HEADERS += gdal.mod

libfortrangis_la_LDFLAGS += $(GDAL_LIBS)

check_PROGRAMS += gdal_test
gdal_test_SOURCES = gdal_test.F90
gdal_test_CPPFLAGS = -I../libfortranc
gdal_test_FCFLAGS = -I../libfortranc
gdal_test_LDADD = libfortrangis.la ../libfortranc/libfortranc.la $(GDAL_LIBS)
endif

if READOSM
libfortrangis_la_SOURCES += readosm.F90
libfortrangis_la_LIBADD += $(READOSM_LIBS)
nodist_include_HEADERS += readosm.mod

check_PROGRAMS += readosm_test
readosm_test_SOURCES = readosm_test.F90
readosm_test_CPPFLAGS = -I../libfortranc
readosm_test_LDADD = libfortrangis.la ../libfortranc/libfortranc.la
readosm_test.o: readosm.$(OBJEXT)
endif

EXTRA_DIST = readosm_test.osm

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod testshape.dbf testshape.shp testshape.shx
