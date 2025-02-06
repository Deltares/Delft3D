ESMF_RegridWeightGen.exe is build in Cygwin with the recipe below.
Several Cygwin dll's are needed to run ESMF_RegridWeightGen.exe on Windows outside Cygwin.



Cygwin
======
Download from http://cygwin.com/: setup-x86_64.exe
When installing cygwin, install the packages listed in the 6 "cygwin_installedpackages_screenshots"-files.
This is enough. Probably not all packages are needed.
Don't forget "zlib".


NetCDF
======
Needed by ESMF:
include/
    ncvalues.h
    netcdf.h
    netcdf.hh
    netcdf.mod
    netcdfcpp.h
lib/
    libnetcdf.a
    libnetcdff.a
    libnetcdf_c++.a
This is spread over 3 subpackages:
    NetCDF-C
    NetCDF-C++
    NetCDF-fortran

NetCDF-C
========
Download from http://www.unidata.ucar.edu/: netcdf-c-netcdf-4.3.0
1) autoreconf -ivf
2) ./configure --prefix=`pwd`
3) In "include\Makefile": remove "install-includeHEADERS" at install-data-am and .PHONY
4) make check install

NetCDF-C++
==========
Download from http://www.unidata.ucar.edu/: netcdf-cxx-4.2
1) autoreconf -ivf
2) CPPFLAGS="-I/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/include" LD_LIBRARY_PATH=/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/lib LDFLAGS="-L/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/lib" LIBS="-lnetcdf -lhdf5_hl -lhdf5 -lz -lcurl" ./configure --disable-shared --prefix=`pwd`
3) make check install

NetCDF-fortran
==============
Download from http://www.unidata.ucar.edu/: netcdf-fortran-netcdf-fortran-4.2
1) autoreconf -ivf
2) CPPFLAGS="-I/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/include" LD_LIBRARY_PATH=/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/lib LDFLAGS="-L/cygdrive/c/code/netcdf/netcdf-c-netcdf-4.3.0/lib" LIBS="-lnetcdf -lhdf5_hl -lhdf5 -lz -lcurl" ./configure --disable-shared --prefix=`pwd`
3) http://www.unidata.ucar.edu/mailing_lists/archives/netcdfgroup/2013/msg00342.html
         Index: man4/netcdf-f90.texi
         ===================================================================
         --- man4/netcdf-f90.texi        (revision 57)
         +++ man4/netcdf-f90.texi        (working copy)
         @@ -2127,7 +2127,6 @@
          The name of the
          group will be copied to this character array. The name will be less
          than NF90_MAX_NAME in length.
         -@item
          
          @end table
          
         @@ -7023,7 +7022,7 @@
          @node FORTRAN 77 to Fortran 90 Transition Guide, Combined Index, Summary of 
         Fortran 90 Interface, Top
          @appendix Appendix B - FORTRAN 77 to Fortran 90 Transition Guide
          
         -@unnumberedsubsec The new Fortran 90 interface 
         +@unnumberedsec The new Fortran 90 interface 
          
          The Fortran 90 interface to the netCDF library closely follows the
          FORTRAN 77 interface. In most cases, function and constant names and
         @@ -7045,7 +7044,7 @@
          versions may be implemented entirely in Fortran 90, adding additional
          error checking possibilities.
         
         -@unnumberedsubsec Changes to Inquiry functions 
         +@unnumberedsec Changes to Inquiry functions 
         
          In the Fortran 90 interface there are two inquiry functions each for
          dimensions, variables, and attributes, and a single inquiry function
         @@ -7079,7 +7078,7 @@
           INTEGER FUNCTION  NF_INQ_ATTNAME    (NCID, VARID, ATTNUM, name)
          @end example
         
         -@unnumberedsubsec Changes to put and get function 
         +@unnumberedsec Changes to put and get function 
         
          The biggest simplification in the Fortran 90 is in the nf90_put_var
          and nf90_get_var functions. Both functions are overloaded: the values
4) make check install



ESMF
====
Download from http://www.earthsystemmodeling.org: esmf_6_2_0_src
or Download from http://www.earthsystemmodeling.org: esmf_7_0_0_src
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
