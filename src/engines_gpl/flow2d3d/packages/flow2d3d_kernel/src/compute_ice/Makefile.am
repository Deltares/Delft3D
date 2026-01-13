#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   26 aug 10
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libKernelComputeIce.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I/opt/mpich2/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics

#-------------------------------------------------------------------------------
#Sources
libKernelComputeIce_la_SOURCES = \
         dens_chnmil.f90 \
         dens_teos.f90 \
         d_ice.f90 \
         gsw_oceanographic_toolbox.f90 \
         dif_ice.f90 \
         hea_ice.f90 \
         init_iceberg.f90 \
         knmi.f90 \
         knmi_ice.f90 \
         limadv.f90 \
         limrhg.f90 \
         limtrp.f90

#-------------------------------------------------------------------------------
#Libraries

