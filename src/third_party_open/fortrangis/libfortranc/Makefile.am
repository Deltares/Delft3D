lib_LTLIBRARIES = libfortranc.la
libfortranc_la_SOURCES = fortranc.F90
libfortranc_la_LDFLAGS =
nodist_include_HEADERS = fortranc.mod


if VERSIONINFO
libfortranc_la_LDFLAGS += -version-info @VERSIONLIB@
endif

# experimental for mingw
#libfortranc_la_LDFLAGS += -no-undefined

TESTS = $(check_PROGRAMS)
check_PROGRAMS = fortranc_test

fortranc_test_SOURCES = fortranc_test.F90 c_test.c
fortranc_test_LDADD = libfortranc.la


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
