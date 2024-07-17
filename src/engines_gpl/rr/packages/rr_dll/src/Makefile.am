#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = librr_dll.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
  -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
  -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src \
  -I$(top_srcdir)/utils_lgpl/gridgeom/packages/gridgeom/src \
  -I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src \
  -I$(top_srcdir)/engines/rr/packages/rr_kernel_f/src \
  -I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src \
  -I$(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src

AM_FCFLAGS += \
   -DHAVE_CONFIG_H \
   -DLINUX64

#-------------------------------------------------------------------------------
#Sources
librr_dll_la_SOURCES = \
       rr_dll_version.F90 \
       rr_dll_bmi.f90 \
       OESOpenMI.F90 \
       rr_dll.F90

#-------------------------------------------------------------------------------
# Libraries

if USENETCDF
AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff
endif 

librr_dll_la_LIBADD = \
    $(top_builddir)/engines/rr/packages/rr_kernel_f/src/librr_kernel_f.la \
    $(top_builddir)/engines/rr/packages/rr_walrus_c/src/librr_walrus_c.la \
    $(top_builddir)/utils/wl_openmi_support/packages/wl_openmi_support/src/libwl_openmi_support.la \
    $(top_builddir)/utils/control_lib/packages/control_lib/src/libcontrol_lib.la \
    $(top_builddir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src/librr_rtc_tools.la \
    $(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src/libgridgeom.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync/libDelftIO.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
    $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
    $(top_builddir)/third_party_open/kdtree2/src-f90/libkdtree2f.la \
    $(top_builddir)/third_party_open/triangle/libtriangle_c.la \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) \
    -ldl

# To be added when using Intel 11:
#    $(FCLIBS)

BUILT_SOURCES = ./always \
	./rr_dll_version.F90

./always:
	rm -rf ./rr_dll_version.F90

./rr_dll_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/rr_dll_version.F90 $(srcdir) $(srcdir)/../../version/version_number.ini $(top_builddir)
