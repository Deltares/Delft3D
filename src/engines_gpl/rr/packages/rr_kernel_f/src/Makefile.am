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
noinst_LTLIBRARIES = librr_kernel_f.la

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
   -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
   -I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/delftio_sync \
   -I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src \
   -I$(top_srcdir)/utils/wl_openmi_support/packages/wl_openmi_support/src \
   -I$(top_srcdir)/utils/rr_rtc_tools/packages/rr_rtc_tools/src

AM_FCFLAGS += \
   -DHAVE_CONFIG_H

#-------------------------------------------------------------------------------
#Sources
librr_kernel_f_la_SOURCES = \
			globals.f90 \
			capsim.f90 \
			quadpack.f90 \
			testdate.f90 \
			Ifreal.f90 \
			Sacramento.f90 \
			Locate.f90 \
			Gregor.f90 \
			scanfile.f90 \
			bubblesort.f90 \
			Parallelmodule.f90 \
			propert.f90 \
			Infiltration.f90 \
			mod_rrerror.f90 \
			rropenmisupport.f90 \
			readlib.F90 \
			Conf_fil.f90 \
			pminterface.f90 \
			ParseToken.f90 \
			newtablemodule.f90 \
			NetCdfData.f90 \
			languagemodule.f90 \
			MessageModule.f90 \
			NetworkModule.f90 \
			RunoffFormulation.f90 \
			InfiltrationFormula.f90 \
			Conf_arr.f90 \
			MeteoModule.f90 \
			hash.f90 \
			CropModule.f90 \
			BoundaryModule.f90 \
			OpenwaterModule.f90 \
			NWRWModule.f90 \
			UnpavedModule.F90 \
			StructureModule.f90 \
			rwzimodule.f90 \
			PAVEDMODULE.f90 \
			GreenhouseModule.f90 \
			FILESMODULE.F90 \
            hash_search_rr.f90 \
            nodes_from_flow1d.f90 \
            branches_from_flow1d.f90 \
            Network_from_flow1d.f90 \
            read_drr_topology.f90 \
			LinkModule.f90 \
			RrConnectionModule.f90 \
			RRRunoffNode.f90 \
			RrRoutingLink.f90 \
			IndustryModule.f90 \
			SacramentoModule.f90 \
			SaltModule.f90 \
			OUTPUTMODULE.f90 \
			BinFileModule.f90 \
			restartmodule.F90 \
			balancemodule.F90 \
			datatoflowmodule.F90 \
			RRModule.F90 \
			SetSobekModflowFluxes.f90 \
			ReadRROutputOptions.f90 \
			rdini.f90 \
			Interp.f90 \
			checkmessages.f90 \
			avundf.F90 \
			setqconn.f90 \
			Nodelp.f90 \
			DETERMINEEXTRASTORAGEAREA.f90 \
			INITDT.F90 \
			CheckEvaporation.f90 \
			Fndnod.f90 \
			SETINITIALDATESANDTIMES.f90 \
			noconvergence.F90 \
			Init2.f90 \
			checkconvergence.f90 \
			fndnd2.f90 \
			SetDatesAndTimes.f90 \
			PROB.F90 \
			Init1.f90 \
			SPLFL3.f90 \
			SetControlDatesAndTimes.f90 \
			bui.f90 \
			Splfil.f90 \
			Openfl.F90 \
			ModflowData_to_SobekArrays.f90 \
			ExtFunctions.f90 \
			Rdloca.f90

AM_FCFLAGS+=${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}
# Manualy add the netcdff (fortran dll), bug in pkg-config file is not fixed until in NetCDF 4.4 HEAD
LIBS+=${NETCDF_LIBS} ${NETCDF_FORTRAN_LIBS} -lnetcdff

#-------------------------------------------------------------------------------
#Libraries
