//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
// $Id: f902c.c 5717 2016-01-12 11:35:24Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/plugin_culvert_c/src/f902c.c $
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(WIN32)
#  include <windows.h>
#elif defined(salford32)
#  include <windows.h>
#elif defined(HAVE_CONFIG_H)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define PERFORM_FUNCTION_CULVERT PERF_FUNCTION_CULVERT
#  define STDCALL
#elif defined(salford32)
#  define PERFORM_FUNCTION_CULVERT PERF_FUNCTION_CULVERT
#  define STDCALL __stdcall
#elif defined(HAVE_CONFIG_H)
#   include "config.h"
#  define PERFORM_FUNCTION_CULVERT FC_FUNC(perf_function_culvert,PERFORM_FUNCTION_CULVERT)
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
#elif defined(HAVE_CONFIG_H)
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
long STDCALL PERFORM_FUNCTION_CULVERT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_CULVERT(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION_CULVERT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *, double *,
                                    char   *, long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *, double *,
                                    char   *, long    , long    );
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
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      discharge   , zpos1       ,
                      zpos2       , message     ,
                      length_dll_strings, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      discharge   , zpos1       ,
                      zpos2       , message     ,
                      length_dll_strings, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
