Log file: building NetCDF 4 on Windows and Linux
------------------------------------------------
By Arjen Markus

dd. 23 december 2015
Using MS Visual Studio 2013 and Intel Fortran 2015

HDF5:
    build using DLL - otherwise there is a build error (nothing produced)
    batch file adapted for this purpose
    no zlib/szlib included - I want to solve the problem with NetCDF4-Fortran

NetCDF4 (C part):
    For reasons I do not understand the configuration of CMake fails,
    yesterday it worked, but an error occurred during building. I used
    MS VS 2010 then.
    Right, Zlib is required.

Okay, restart. I need the file CMake-hdf5-1.8.16.zip. This contains what
we need.

Instructions on: https://www.hdfgroup.org/HDF5/release/cmakebuild.html

Running "build-VS2013-64.bat" in the directory hdf5-w64. This ought to
do the trick. Result: HDF5-1.8.16-win64.zip.

Unpack this file into the directory HDF5-1.8.16-win64.

But why do I get a problem link NetCDF? Static/dynamic libraries?

Trying again, this time with dynamic libraries. Copying the file
build-VS2013-64.bat to build-VS2013-64-dyn.bat

From the code in HDF518config.cmake I deduce that the actual option is
STATICLIBRARIES, not STATIC_LIBRARIES. I set both in the argument list.

It turned out that the HDF5 libraries were not included in the link
step. I specified the libraries explicitly via HDF5_LIB, HDF5_HL_LIB and
HDF5_INCLUDE_DIR. This did the trick insofar as to include the
libraries, but problems with unresolved external symbols remained. The
symbols in question are data, not routines. IIRC that requires a
slightly different approach under Windows.

In february this year I was able to build the library, using 32 bits
instead of 64 bits. That is what I am trying now.

I have switched to the static version of the HDF5 libraries - that
avoids one possible link problem. And I need to specify all the
libraries - hdf5, zlib and szip.

Okay, that worked. The solution: only static HDF5 libraries.

I am gettin further along the build path. Changed the file
netcdf-config.cmake.in to make sure the variables are set. That seems to
have worked up to a certain point - failure finding the netcdf.h include
file when compiling nf_lib.c.

Not sure what to do now. I cannot find where the include is coming from.
Ah, the command-line. It also seems the netcdf-config.cmake file is not
used for building the libraries, only the examples? I could not find a
reference to the file anywhere.

That problem solved, the next is here: _ncerr and _ncopts and a bunch of
external symbols.

Right! I did some patching - netcdf.h defines these variables and I
added MSC_EXTRA to the missing routines in dparallel.c


----
dd. 8 january 2016
Retrying:
- Copy the contents of CMake-hdf5-1.8.16.zip to a new directory
- Run build-VS2013-32.bat in a DOS window enabled for VS2013, 32bits
- Wait (a large bunch of test programs is being run)

Crucial step:
- Copy the contents of subdirectory
  build\_CPack_Packages]win32\ZIP\HDF5-1.8.16-win32 to the work
  directory

Changes I made to NetCDF 4.3.2 (C):
- liblib: CMakeLists.txt - maintain!
- include: netcdf.h - revert
- libdispatch: dparallel.c - revert

Changes I made to NetCDF 4.4.1 (Fortran):
- fortran: netcdff.def - added, is no longer needed if building statically
- fortran: CMakeLists.txt - revert
- fortran: nf_v2compat.c - revert

Now building NetCDF 4.3.2 (C):
- Work directory: netcdf-build-w32
- Run cmakenc-w32.bat (set to build static libraries)
- Run nmake

Now building NetCDF 4.4.1 (Fortran):
- Work directory: netcdf-build-fortran-w32
- Run cmakencf-w32.bat (set to build static libraries, copies the
  NetCDf C library to a convenient location)
- Run nmake

And the machine stops. All manner of unresolved externals that should
come from HDF5. Hm, it could be that this is actually irrelevant.

Trying to build a program manually, instead of via CMake. That succeeds:

ifort simple_xy_wr.f90 netcdff.lib netcdf.lib libhdf5_hl.lib libhdf5.lib libzlib.lib libszip.lib \
    /link /nodefault lib:libcmt.lib /nodefaultlib:msvcrtd.lib

I copied all the libraries into the working directory, that contained
also the .mod files.

By request of Jan Mooiman I examined the source code for HDF5, NetCDF
(C) and NetCDF (Fortran) for routines returning version strings:
- HDF5: H5_get_libversion( major, minor, revision) - returns three
      numbers in its arguments
- NetCDF C: char *nc_get_libvers() - returns a pointer to a string
      describing the version
- NetCDF Fortran: character(len=80) nf90_get_libvers() - returns a
      string describing the version


Building on Linux
-----------------
- Get the tarball from https://www.hdfgroup.org/HDF5/release/cmakebuild.html
  to build via CMake
- Unpack the tarball in a convenient working directory
- Run build.sh
  Problem: I need CMake 3.1 or later. That is not installed on the
  machine. So I will build CMake locally first
- Got me version 3.4.1 - the current one from www.cmake.org
- Running bootstrap and then gmake, as per the instructions.
- Creating a script "cmakenc.sh", based on "cmakenc-w32.bat", to build
  the C library (largest problem sofar: getting the library file names
  right)

  Failure: the routine H5PL_load in H5PL.c uses dlopen and friends.
  Which is typically only required for loading shared libraries. So
  the build is failing because of this.

  Switching to the shared version instead. That is: HDF5 is still
  static, but NetCDF4 is not. (I do not immediately see how to build
  HDF5 as shared)

  HM, that did not help much. I will have to add -ldl
  That worked: via CMAKE_SHARED_LINKER_FLAGS

- Note: CMake found the C compiler in /usr/bin/ - GCC 4.4.7, not what
  I had meant. Oh, no Intel Fortran available via the path. CMake
  was correct.

- Moving on to the Fortran libraries
- Problem: netcdf-config.cmake was based on the faulty version in the
  source tarball. Manually adapted it. Also found that the libraries
  are 64-bits, so copy the library to lib64.

- Tested using:
  gfortran -o simple_xy_wr simple_xy_wr.f90 libnetcdff.so libnetcdf.so \
      libhdf5.a libhdf5_hl.a libz.a libszip.a

Now, retrying: Intel CC and Fortran, all static libraries (as I realised
that the shared version of NetCDF is most probably superfluous - I
needed to add the flag -ldl anyway).

That fails: /usr/bin/ar: -ldl: no such file or directory. And CMake
is still using /usr/bin/cc. So I will have to force it to use Intel CC
and Fortran manually. Very odd, this stuff.


dd. 11 january 2016
I checked the code in H5PL.c to see if I can simply get around the
dlopen problem. That does not seem all that easy, unless I patch
the code - redefine the macros that are responsible for the actual
calls to dlopen and friends. Perhaps worth the while though. No!
Further checking reveals that the routine H5PL_load is used in
several places in the HDF5 library itself, so it is not simply
an external API for loading plugins.

Possible problem on Linux: the libraries use soft links according to the
well-known scheme, but on copying the links are not preserved.

Now preparing everything for Windows, using MS VC, Visual Studio and
Intel Fortran.

dd. 12 january 2016
By setting the environment variables CC and FC to the names of the
compilers of choice we can make CMake use these compilers. It is as
simple as that.


