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
noinst_LTLIBRARIES = librr_rtc_tools.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src

#-------------------------------------------------------------------------------
#Sources
librr_rtc_tools_la_SOURCES = \
         Julian.f90 \
         dh-alloc.f90 \
         Weekdy.f90 \
         GPSLEEP.F90 \
         Nxtday.f90

#-------------------------------------------------------------------------------
#Libraries

