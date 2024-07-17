#-------------------------------------------------------------------------------
#   Makefile for rtc
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = rtc

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
   -I$(top_srcdir)/engines/rtc/packages/rtc_kernel/src \
   -I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
   -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src \
   -I$(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src

AM_FCFLAGS += \
   -DHAVE_CONFIG_H

#-------------------------------------------------------------------------------
#Sources
rtc_SOURCES = \
         rtc_version.F90 \
         rtcmodule.F90 \
         rtc.F90 \
         rtc_wrlogo.f90

#-------------------------------------------------------------------------------
#Libraries

rtc_LDADD=\
      $(top_srcdir)/engines/rtc/packages/rtc_kernel/src/librtc_kernel.la \
      $(top_srcdir)/engines/rtc/packages/plugin_rtc_c/src/libplugin_rtc_c.la \
      $(top_srcdir)/utils/control_lib/packages/control_lib/src/libcontrol_lib.la \
      $(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
      $(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
      $(top_srcdir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
      $(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src/librr_rtc_tools.la \
      $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
      $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la \
      $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libESMC.la \
      $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libESMFSMVersionNumber.la \
      $(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src/libwl_openmi_support.la \
      -ldl

rtc_LDFLAGS = -export-dynamic

# To be added when using Intel 11:
#    $(FCLIBS)

BUILT_SOURCES = ./always \
	./rtc_version.F90

./always:
	rm -rf ./rtc_version.F90

./rtc_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/rtc_version.F90 $(srcdir) $(srcdir)/version_number.ini $(top_builddir)
