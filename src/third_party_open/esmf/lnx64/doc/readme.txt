ESMF_RegridWeightGen.exe is build on the p-h5devux with the recipe below.


added:
esmf/build_h5.sh

NetCDF
======
module load netcdf/v4.3.2_v4.4.0_intel_14.0.3


ESMF
====
Download from http://www.earthsystemmodeling.org: esmf_7_0_0_src
1) export ESMF_DIR=/cygdrive/c/code/esmf/esmf
   export ESMF_COMM="mpiuni"
   export ESMF_COMPILER=gfortran
   export ESMF_NETCDF="split"
   export ESMF_NETCDF_INCLUDE=/cygdrive/c/code/netcdf/include
   export ESMF_NETCDF_LIBPATH=/cygdrive/c/code/netcdf/lib
   INSTEAD OF:
       #  export ESMF_CXXLINKLIBS="-lhdf5_hl -lhdf5 -lz -lcurl -lgfortran -lstdc++"
       #  export ESMF_CXXESMFLINKLIBS="-lhdf5_hl -lhdf5 -lz -lcurl -lgfortran -lstdc++"
       #  export ESMF_F90LINKLIBS="-lhdf5_hl -lhdf5 -lz -lcurl -lgfortran -lstdc++"
   DO (see esmf\esmf\build\common.mk):
       export ESMF_NETCDF_LIBS="-lnetcdff -lnetcdf_c++ -lnetcdf -lhdf5_hl -lhdf5 -lz -lcurl -lgfortran -lstdc++"
       !Watch out! The order is important: first netcdf, then the libraries that netcdf uses (hdf5, curl)
2) (solved in esmf 7.0.0) esmf\build_config\Cygwin.gfortran.default\build_rules.mk:
   Replace:
     ESMF_F90DEFAULT         = gfortran-4
     ESMF_CXXDEFAULT         = g++-4
   With:
     ESMF_F90DEFAULT         = gfortran
     ESMF_CXXDEFAULT         = g++
3) (solved in esmf 7.0.0) esmf\makefile: disable ranlib:
   Replace:
      # Ranlib on the libraries
      ranlib:
	$(ESMF_RANLIB) $(wildcard $(ESMF_LIBDIR)/lib*.a)
   With:
      # Ranlib on the libraries
      ranlib:
      	-@echo "A3M: ranlib disabled"
4) (solved in esmf 7.0.0) esmf\src\apps\makefile: skip ESMF_WebServController: (maybe not needed anymore?)
   Replace:
      DIRS      = ESMF_Info ESMF_InfoC ESMF_RegridWeightGen ESMF_WebServController 
   With:
      DIRS      = ESMF_Info ESMF_InfoC ESMF_RegridWeightGen
5) make build_apps
ESMF_RegridWeightGen.exe is in "esmf\apps\appsO\Cygwin.gfortran.64.mpiuni.default"
