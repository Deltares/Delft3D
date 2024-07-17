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
noinst_LTLIBRARIES = libcontrol_lib.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils/control_lib/packages/control_lib/src \
	-I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src

#-------------------------------------------------------------------------------
#Sources
libcontrol_lib_la_SOURCES = \
         CONTROL.F90

#-------------------------------------------------------------------------------
#Libraries
