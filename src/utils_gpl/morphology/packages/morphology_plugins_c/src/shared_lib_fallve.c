//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id$
// $HeadURL$
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32)
#  include <windows.h>
#elif defined(salford32)
#  include <windows.h>
#elif defined(linux)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define PERFORM_FUNCTION_FALLVE  PERF_FUNCTION_FALLVE
#  define STDCALL
#elif defined(salford32)
#  define PERFORM_FUNCTION_FALLVE  PERF_FUNCTION_FALLVE
#  define STDCALL __stdcall
#elif defined(linux)
#   include "config.h"
#  define PERFORM_FUNCTION_FALLVE  FC_FUNC(perf_function_fallve,PERFORM_FUNCTION_FALLVE)
#  define STDCALL
#endif

/*
 *
 * Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
 * Special attention to the WINAPI define, which is needed if the DLL is written in F90
 *
 */

#if defined(WIN32)
    typedef HMODULE DllHandle;
#elif defined(salford32)
    typedef HMODULE DllHandle;
#elif defined(linux)
    typedef void * DllHandle;
#endif

typedef struct {
    DllHandle   dllHandle;
} SharedDLL;

/*
 * ============================================================================
 */
char * strFcpy(char * str_1, int len);
void RemoveTrailingBlanks_dll(char * String);

/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION_FALLVE(long long int  * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings )
	                          // message is a c-string: no length specification added
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_FALLVE(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings)
	                          // message is a c-string: no length specification added
#elif defined (linux)
long STDCALL PERFORM_FUNCTION_FALLVE(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings)
	                          // message is a c-string: no length specification added
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, char   *, long );
                                    // message is a c-string: no length specification added
#elif defined (linux)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, char   *, long );
                                    // message is a c-string: no length specification added
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(linux)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      ws          , message     ,
                      length_dll_strings);
	                  // message is a c-string: no length specification added
#elif defined (linux)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      ws          , message     ,
                      length_dll_strings);
	                  // message is a c-string: no length specification added
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
