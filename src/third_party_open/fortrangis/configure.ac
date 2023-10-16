dnl Process this file with autoconf to produce a configure script.

AC_INIT([FortranGIS],[3.0],[dcesari@arpae.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([m4])

VERSIONLIB=`echo $VERSION | awk -F. '{printf "%d:%d", $1, $2}'`
AC_SUBST(VERSIONLIB)

AC_LANG([C])
AC_PATH_XTRA
dnl PKG_PROG_PKG_CONFIG()

dnl capire perche' senza --enable-qualcosa objext='' e non va il compilatore

AC_ARG_ENABLE(gdal, AS_HELP_STRING([--disable-gdal],[enable build of Fortran 2003 bindings for gdal]),,
[enableval=yes])dnl default enable
AM_CONDITIONAL(GDAL, test "x$enableval" = "xyes")

if test "x$enableval" = "xyes"; then
if test "x$host" = "xi686-pc-mingw32"; then
dnl find a better way to check whether we are cross compiling for mingw
  if test "x$GDAL_LIBS" = "x"; then
#  GDAL_LIBS="-lgdal_i"
    GDAL_LIBS="-lgdal19"
  fi
  AC_SUBST(GDAL_LIBS)
  AC_DEFINE(HAVE_LIBGDAL, 1, [Enable gdal]),
else
AC_CHECK_PROG(gdal_config, [gdal-config], [yes], [no],,)
if test "x$gdal_config" = "xyes"; then
  GDAL_LIBS=`gdal-config --libs`
  AC_SUBST(GDAL_LIBS)
  AC_DEFINE(HAVE_LIBGDAL, 1, [Enable gdal])
else
dnl AC_CHECK_LIB([gdal], [GDALAllRegister],
AC_SEARCH_LIBS([GDALAllRegister], [gdal],
 gdal=yes
 if "x$ac_cv_search_GDALAllRegister" != "xnone required" ]; then
   GDAL_LIBS="-lgdal"
 else
   GDAL_LIBS=""
 fi
 AC_SUBST(GDAL_LIBS)
 AC_DEFINE(HAVE_LIBGDAL, 1, [Enable gdal]),
 AC_MSG_ERROR([gdal library not found]), )
fi
fi
fi

AC_ARG_ENABLE(proj, AS_HELP_STRING([--disable-proj],[enable build of Fortran 2003 bindings for proj]),,
[enableval=yes])dnl default enable
AM_CONDITIONAL(PROJ, test "x$enableval" = "xyes")

AM_CONDITIONAL(PROJ4, false)
AM_CONDITIONAL(PROJ6, false)
if test "x$enableval" = "xyes"; then
    AC_CHECK_LIB([proj], [pj_init], [proj4=yes], [proj4=no])
    AC_CHECK_LIB([proj], [proj_info], [proj6=yes], [proj6=no])
    dnl AM_CONDITIONAL and AC_DEFINE for all 3 variants may be too much
    dnl keep only what is really needed when closing issue15
    AM_CONDITIONAL(PROJ4, test "x$proj4" = "xyes")
    AM_CONDITIONAL(PROJ6, test "x$proj6" = "xyes")
    if test x$proj4 = xyes; then
        proj=yes
        AC_DEFINE(HAVE_LIBPROJ4, 1, [Enable proj <=4])
    fi
    if test x$proj6 = xyes; then
        proj=yes
        AC_DEFINE(HAVE_LIBPROJ6, 1, [Enable proj >=6])
    fi
    if test x$proj = xyes; then
        PROJ_LIBS="-lproj"
        AC_SUBST(PROJ_LIBS)
        AC_DEFINE(HAVE_LIBPROJ, 1, [Enable proj])
    else
        AC_MSG_ERROR([proj library not found])
    fi
fi

AC_ARG_ENABLE(shapelib, AS_HELP_STRING([--disable-shapelib],[enable build of Fortran 2003 bindings for shapelib]),,
[enableval=yes])dnl default enable
AM_CONDITIONAL(SHP, test "x$enableval" = "xyes")

if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([shp], [SHPOpen],
 shp=yes
 SHP_LIBS="-lshp"
 AC_SUBST(SHP_LIBS)
 AC_DEFINE(HAVE_LIBSHP, 1, [Enable shp]),
 AC_MSG_ERROR([shapelib library not found]), )
AC_CHECK_HEADER([shapefil.h],,
 AC_MSG_ERROR([you probably have to provide the correct CPPFLAGS=-I... option e.g. CPPFLAGS=-I/usr/include/libshp]))
fi

AC_ARG_ENABLE(readosm, AS_HELP_STRING([--enable-readosm],[enable build of Fortran 2003 bindings for readosm]),,
[enableval=no])dnl default disable
AM_CONDITIONAL(READOSM, test "x$enableval" = "xyes")

if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([readosm], [readosm_open],
 readosm=yes
 READOSM_LIBS="-lreadosm"
 AC_SUBST(READOSM_LIBS)
 AC_DEFINE(HAVE_LIBREADOSM, 1, [Enable readosm]),
 AC_MSG_ERROR([readosm library not found]), )
fi

dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([ifort gfortran g95 pgf90 xlf90 f90])
F77=$FC
FFLAGS=$FCFLAGS

AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension

dnl check for Fortran 2003 ISO_C_BINDING intrinsic module
AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM(,[[       USE, INTRINSIC :: iso_c_binding]])],
                          [AC_MSG_RESULT([Fortran 2003 C bindings found!])],
                          [AC_MSG_ERROR([Fortran 2003 C bindings not found!
Sorry, FortranGIS requires a fortran compiler
with at least the ISO_C_BINDING module capabilities of Fortran 2003 standard]
)])

AC_LANG_POP()dnl back to C

AC_ARG_ENABLE(versioninfo, AS_HELP_STRING([--disable-versioninfo],[disable version information in shared libraries]),,[enableval=yes])dnl default enable
AM_CONDITIONAL(VERSIONINFO, test "x$enableval" = "xyes")

AC_ARG_ENABLE(doxydoc, AS_HELP_STRING([--disable-doxydoc],[disable generation of doxygen documentation]),,[enableval=yes])dnl default enable

DOXYBIN=
if test "x$enableval" = "xyes"; then
 AC_CHECK_PROG(DOXYBIN, doxygen, yes, no)
 if test "x$DOXYBIN" = "xno"; then
   AC_MSG_ERROR([doxygen not found use --disable-doxydoc to disable documentation generation])
 fi
fi
AM_CONDITIONAL(DOXYDOC, test "x$DOXYBIN" = "xyes")


dnl check for debug mode
AC_ARG_ENABLE(debug-mode, AS_HELP_STRING([--enable-debug-mode],[enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
CFLAGS="$CFLAGS -g"
CXXFLAGS="$CXXFLAGS -g"
if test "x$FC" = "xgfortran"; then
FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall"
else
FCFLAGS="$FCFLAGS -g -C"
fi
AC_DEFINE(_DEBUG, 1, [Enable debugging mode])
else
CFLAGS="$CFLAGS -O"
CXXFLAGS="$CXXFLAGS -O"
FCFLAGS="$FCFLAGS -O"
fi
AM_CONDITIONAL(DEBUG, test "x$enableval" = "xyes")

LT_INIT([win32-dll])
dnl if test "x$host" = "xi686-pc-mingw32"; then
dnl   AM_LIBTOOLFLAGS=-no-undefined
dnl   AC_SUBST(AM_LIBTOOLFLAGS)
dnl fi

pkgdocdir='${datadir}/doc/${PACKAGE}-${VERSION}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
libfortranc/Makefile
libfortrangis/Makefile
doc/Doxyfile
doc/Makefile
])

AC_OUTPUT
