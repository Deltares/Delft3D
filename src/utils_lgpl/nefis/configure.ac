#                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(Delft3D, 0.6, delft3d-support@wldelft.nl)
dnl Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
dnl Define a unique file to check if we're in the right directory
AC_CONFIG_SRCDIR([packages/nefis/src/nefis_version.cpp])
AC_CONFIG_HEADER([config.h])


dnl Initialise automake
AM_INIT_AUTOMAKE

# c, c++ and c preprocessor
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

#These are required for java
#AC_PROG_JAVA
#AC_PROG_JAVA_CC
#AC_PROG_JAVAC
#this is needed when sun java is unavailable
#AM_PROG_GCJ

# Other programs
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_LEX
AC_PROG_YACC

#check if old compilers are used
#AC_CHECK_OLD_COMPILER
#Arjen: commented this line, the macro is apparently not defined

## This is copied from the libtool examples
## -------------------------- ##
## Fortran90 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran])
dnl http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_FC([ifort gfortran g95])
dnl Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
dnl Check for underscoring of external names
AC_FC_WRAPPERS
dnl We need to use .f90 and not .f to enable Automake FC support
dnl Some Intel ifc/ifort do not understand .f95.  :-/
AC_FC_SRCEXT([f90])
dnl Yes, we want free form Fortran!
AC_FC_FREEFORM
AC_FC_LINE_LENGTH([unlimited])
AC_LANG_POP

## -------------------------- ##
## Fortran77 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran 77])
AC_PROG_F77([ifort gfortran g77])
AC_F77_LIBRARY_LDFLAGS
AC_LANG_POP


# As of the writing of this demo, GNU Autoconf's AC_OBJEXT and
# AC_EXEEXT macros only works for C compilers!
# Libtool's setup macro calls AC_OBJEXT and AC_EXEEXT without setting
# the test language to C.  We do it before any libtool setup macros are
# called so that the proper values are cached beforehand.  We also do
# it before any linker flags (LDFLAGS) are set so that fortran specific
# ones don't break the tests.
AC_LANG_PUSH([C])
AC_OBJEXT
AC_LANG_POP


# executables will get an .exe extension
AC_EXEEXT([.exe])


## ----------------------- ##
## Libtool initialisation. ##
## ----------------------- ##
dnl Libtool is disabled (static linking issues)
#LT_INIT -> new in 2.0
AC_LIBLTDL_CONVENIENCE
AC_PROG_LIBTOOL
AC_SUBST([LIBTOOL_DEPS])
AC_CONFIG_SUBDIRS(libltdl)
AC_SUBST(LIBLTDL)
AC_SUBST(INCLTDL)


dnl define options
dnl Use double support
AC_MSG_CHECKING([whether to disable double support])
AC_ARG_ENABLE(double,
[  --disable-double           Disable double support],
   [ AC_MSG_RESULT([yes]) ],
   [ AC_MSG_RESULT([no]); AC_DEFINE(WITH_DOUBLE, 1, [Use double precision]) ]
)


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h netdb.h netinet/in.h stdlib.h string.h sys/socket.h sys/time.h unistd.h])

#check for jni.h (for later)
#AC_JNI_INCLUDE_DIR

#check for c++ standard library
AC_CHECK_LIB([stdc++], [main],, [AC_MSG_ERROR([Compiler can't find libstdc++])])

#Check for expat
AC_CHECK_HEADER(expat.h,, [AC_MSG_ERROR([Compiler can't find <expat.h>])])
AC_CHECK_LIB(expat, XML_Parse,, [AC_MSG_ERROR([Compiler can't find libexpat])])
AC_PATH_X
AC_PATH_XTRA

# checks for pthreads
ACX_PTHREAD([enable_threads="pthread"],[enable_threads="no"])
if test $enable_threads != "pthread"; then
    AC_MSG_ERROR([unable to find pthreads, currently this is required])
else
    AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.])
    LIBS="$PTHREAD_LIBS $LIBS"
    CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
    CXXFLAGS="$PTHREAD_CFLAGS $CXXFLAGS"
fi


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_HEADER_LSEEK64

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_HEADER_MAJOR
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([gethostname gettimeofday memset strerror])

# Checks for QT library functions.
dnl gw_CHECK_QT
dnl AX_HAVE_QT


#which makefiles are we gonna make?
AC_CONFIG_FILES([Makefile
                 packages/Makefile
                 packages/nefis/Makefile
                 packages/nefis/src/Makefile
                 tests/Makefile
                 tests/test_00/Makefile
                 tests/test_01/Makefile
                 tests/test_02/Makefile
                 tests/test_03/Makefile
                 tests/test_04/Makefile
                 tests/test_05/Makefile
                 tests/test_06/Makefile
                 tests/test_07/Makefile
                 tests/test_08/Makefile
                 tests/test_09/Makefile
                 tests/test_10/Makefile
                 tests/test_11/Makefile
                 tests/test_12/Makefile
                 tests/test_13/Makefile
                 tests/test_14/Makefile
                 tests/test_15/Makefile
                 tests/test_16/Makefile
                 tests/test_17/Makefile
                 tests/test_18/Makefile
                 tests/test_19/Makefile
                 tests/demo_00/Makefile
                 tests/demo_01/Makefile
                 tests/demo_02/Makefile
                 ])
AC_OUTPUT
