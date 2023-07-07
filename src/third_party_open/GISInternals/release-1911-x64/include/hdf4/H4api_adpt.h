/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF.  The full HDF copyright notice, including       *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at      *
 * http://hdfgroup.org/products/hdf4/doc/Copyright.html.  If you do not have *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

#ifndef H4API_ADPT_H
#define H4API_ADPT_H

#include "h4config.h"

/**
 * Provide the macros to adapt the HDF public functions to
 * dll entry points.
 * In addition it provides error lines if the configuration is incorrect.
 **/


/* This will only be defined if HDF4 was built with CMake */
#if defined(H4_BUILT_AS_DYNAMIC_LIB)

#if defined(xdr_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define XDRLIBAPI extern __declspec(dllexport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define XDRLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif /* xdr_EXPORTS */

#if defined(hdf_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFERRPUBLIC __declspec(dllimport)
    #define HDFPUBLIC __declspec(dllexport)
    #define HDFLIBAPI extern __declspec(dllexport)
    #define HDFFCLIBAPI extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFERRPUBLIC extern __attribute__ ((visibility("default")))
    #define HDFPUBLIC __attribute__ ((visibility("default")))
    #define HDFLIBAPI extern __attribute__ ((visibility("default")))
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif /* hdf_EXPORTS */

#if defined(hdf_fcstub_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFPUBLIC __declspec(dllexport)
    #define HDFLIBAPI extern __declspec(dllimport)
    #define HDFFCLIBAPI extern __declspec(dllexport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFPUBLIC __attribute__ ((visibility("default")))
    #define HDFLIBAPI extern __attribute__ ((visibility("default")))
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif /* hdf_fcstub_EXPORTS */

#if defined(mfhdf_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFERRPUBLIC extern __declspec(dllimport)
    #define HDFPUBLIC __declspec(dllimport)
    #define HDFLIBAPI extern __declspec(dllexport)
    #define HDFFCLIBAPI extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFERRPUBLIC extern __attribute__ ((visibility("default")))
    #define HDFPUBLIC __attribute__ ((visibility("default")))
    #define HDFLIBAPI extern __attribute__ ((visibility("default")))
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif /* mfhdf_EXPORTS */

#if defined(mfhdf_fcstub_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFPUBLIC __declspec(dllimport)
    #define HDFLIBAPI extern __declspec(dllimport)
    #define HDFFCLIBAPI extern __declspec(dllexport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFPUBLIC __attribute__ ((visibility("default")))
    #define HDFLIBAPI extern __attribute__ ((visibility("default")))
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif /* mfhdf_fcstub_EXPORTS */

#if defined(hdf_test_fcstub_EXPORTS)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFFCLIBAPI extern __declspec(dllexport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif/* hdf_test_fcstub_EXPORTS */

#if !defined(XDRLIBAPI)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define XDRLIBAPI extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define XDRLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif
#if !defined(HDFERRPUBLIC)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFERRPUBLIC extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFERRPUBLIC extern __attribute__ ((visibility("default")))
  #endif
#endif
#if !defined(HDFPUBLIC)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFPUBLIC __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFPUBLIC __attribute__ ((visibility("default")))
  #endif
#endif
#if !defined(HDFLIBAPI)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFLIBAPI extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif
#if !defined(HDFFCLIBAPI)
  #if defined (_MSC_VER)  /* MSVC Compiler Case */
    #define HDFFCLIBAPI extern __declspec(dllimport)
  #elif (__GNUC__ >= 4)  /* GCC 4.x has support for visibility options */
    #define HDFFCLIBAPI extern __attribute__ ((visibility("default")))
  #endif
#endif

#elif defined(H4_BUILT_AS_STATIC_LIB)
  #define XDRLIBAPI extern
  #define HDFERRPUBLIC extern
  #define HDFPUBLIC
  #define HDFLIBAPI extern
  #define HDFFCLIBAPI extern

#else
/* This is the original HDFGroup defined preprocessor code which should still work
 * with the VS projects that are maintained by "The HDF Group"
 * This will be removed after the next release.
 */

#ifdef _WIN32
/**
 * Under _WIN32 we have single threaded static libraries, or
 * mutli-threaded DLLs using the multithreaded runtime DLLs.
 **/
#  if defined(_MT) &&  defined(_DLL) &&!defined(_HDFDLL_)
/*     If the user really meant to use _HDFDLL_, but he forgot, just define it. */
#      define _HDFDLL_
#  endif

#  if !defined(_MT) && defined(_HDFDLL_)
#      error To use the HDF libraries from a single-threaded project, you must use static HDF libraries
#      error Undefine the macro "_HDFDLL_"
#  endif

#  if defined(xdr_EXPORTS)
#      define XDRLIBAPI extern __declspec(dllexport)
#  endif /* xdr_EXPORTS */

#  if defined(_HDFDLL_)
#    pragma warning( disable: 4273 ) /* Disable the stupid dll linkage warnings */
#    if defined(_HDFLIB_)
#      define HDFPUBLIC __declspec(dllexport)
#      define HDFLIBAPI extern __declspec(dllexport)
#    endif

#    if defined(_MFHDFLIB_)
#      define HDFLIBAPI extern __declspec(dllexport)
#    endif

#    if defined(_HDFLIB_C_STUB_EXPORTS) || defined(_MFHDFLIB_C_STUB_EXPORTS) || defined(_DLLLIBTEST_FCSTUB_EXPORTS)
#      define HDFFCLIBAPI extern __declspec(dllexport)
#    endif 

#    if defined(_HDFLIB_C_STUB_EXPORTS)
#      define HDFPUBLIC __declspec(dllexport)
#    endif

#    if !defined(XDRLIBAPI)
#      define XDRLIBAPI extern __declspec(dllimport)
#    endif
#    if !defined(HDFERRPUBLIC)
       #define HDFERRPUBLIC extern __declspec(dllimport)
#    endif
#    if !defined(HDFPUBLIC)
#      define HDFPUBLIC __declspec(dllimport)
#    endif
#    if !defined(HDFLIBAPI)
#      define HDFLIBAPI extern __declspec(dllimport)
#    endif
#    if !defined(HDFFCLIBAPI)
#      define HDFFCLIBAPI extern __declspec(dllimport)
#    endif

#  else
#    define XDRLIBAPI extern
#    define HDFERRPUBLIC extern
#    define HDFPUBLIC
#    define HDFLIBAPI extern
#    define HDFFCLIBAPI extern
#  endif
#else  /* !defined( _WIN32 ) */
#  define XDRLIBAPI extern
#  define HDFERRPUBLIC extern
#  define HDFPUBLIC
#  define HDFLIBAPI extern
#  define HDFFCLIBAPI extern
#endif

#endif /*H4_BUILT_AS_DYNAMIC_LIB  */


#endif /* H4API_ADPT_H */
