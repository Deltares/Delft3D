#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libwl_openmi_support.la

#-------------------------------------------------------------------------------
#Sources
libwl_openmi_support_la_SOURCES = \
         wlopenmi_version.F90 \
         WLOpenMISupport.F90

#-------------------------------------------------------------------------------
#Libraries

EXTRA_DIST = \
	wlopenmi_version.F90.svn

BUILT_SOURCES = ./always \
	./wlopenmi_version.F90

./always:
	rm -rf ./wlopenmi_version.F90

./wlopenmi_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/wlopenmi_version.F90 $(srcdir) $(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src/version_number.ini $(top_builddir)
