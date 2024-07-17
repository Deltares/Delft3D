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
noinst_LTLIBRARIES = librtc_kernel.la

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
   -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
   -I$(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src \
   -I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src

#-------------------------------------------------------------------------------
#Sources
librtc_kernel_la_SOURCES = \
         readlib.F90 \
         PARAMETERMODULE.f90 \
         MeasureModule.f90 \
         ParseToken.f90 \
         newtablemodule.f90 \
         filemodule.F90 \
         LocationDataModule.F90 \
         Calbar.f90 \
         DecisionModule.f90 \
         Ch3bmeas.f90 \
         otherdata.f90 \
         CheckAndSetSeriesReference.f90 \
         Chsbmeas.f90 \
         rtc_matlab_module.F90 \
         CloseMatlab.F90 \
         CmpWindTablePrediction.f90 \
         ReservoirModule.f90 \
         externaldllmodule.f90 \
         Cmpdecv.F90 \
         Cmprhlp.f90 \
         languagemodule.f90 \
         ERRMSG.F90 \
         Fnddat.f90 \
         Getfil.f90 \
         Gregor.f90 \
         Initar.f90 \
         Interp.f90 \
         Nxtstp.f90 \
         OpenMatlab.F90 \
         Rd3bms.f90 \
         RdWindTableMod.f90 \
         Rdbar.f90 \
         Rdpara.f90 \
         Rdrain.f90 \
         Rdwind.f90 \
         ReadDioPlt.f90 \
         Rtc_Init1.f90 \
         Rtc_Rdini.F90 \
         SetTimEnd.f90 \
         calcDiffDays.f90 \
         cmprainprediction.f90 \
         cmprhlp2.f90 \
         cmpwindprediction.f90 \
         datafrommatlab.F90 \
         datatomatlab.F90 \
         interp_double.f90 \
         rdsmeas.f90 \
         rtcopenmisupport.f90 \
         scanrain.f90 \
         scanwind.f90 \
         setratio.f90 \
         upd_ar.f90

#-------------------------------------------------------------------------------
#Libraries
