//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
#include <stdint.h>

#include "so_fortran_api.h"

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
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define STDCALL
#elif defined(salford32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define STDCALL __stdcall
#elif defined(linux)
#   include "config.h"
#  define OPEN_SHARED_LIBRARY      FC_FUNC(open_shared_library,OPEN_SHARED_LIBRARY)
#  define CLOSE_SHARED_LIBRARY     FC_FUNC(close_shared_library,CLOSE_SHARED_LIBRARY)
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
char * strFcpy(char * str_1, int len)
{
    int m;
    char * str_2;
    m = min( len, (int) strlen(str_1));
    str_2 = (char *) malloc( sizeof(char)*(m+1));
    strncpy(str_2, str_1, m);
    str_2[m] = '\0';
    return str_2;
}

void RemoveTrailingBlanks_dll(char * String)
{
  size_t i;
  i = strlen(String)-1;
  while ( String[i] == ' '  ||
          String[i] == '\n' ||
          String[i] == '\t'    )
  {
    String[i] = '\0';
    i--;
  }
  return;
}
/*
 * ============================================================================
 */
#if defined(WIN32) || defined (linux)
long STDCALL OPEN_SHARED_LIBRARY(long long int * sharedDLLHandle, char * library, long length_lib)
#elif defined (salford32)
extern "C" OPEN_SHARED_LIBRARY(int64_t * sharedDLLHandle, char * library, long length_lib)
#endif
{
    long error = 1;
    SharedDLL * tmpSharedDLL = NULL;
    char * lib_name = strFcpy(library, length_lib);

    *sharedDLLHandle = 0;

    RemoveTrailingBlanks_dll(lib_name);

    tmpSharedDLL = (SharedDLL *) malloc(sizeof(SharedDLL));
#if defined(WIN32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(salford32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(linux)
    tmpSharedDLL->dllHandle = dlopen(lib_name, RTLD_LAZY);
#endif

    if (tmpSharedDLL->dllHandle != NULL)
    {
        error = 0;
        *sharedDLLHandle = (long long int) tmpSharedDLL;
    }

    free(lib_name); lib_name = NULL;

    return error;
}
/*
 * ============================================================================
 */

#if defined (WIN32) || defined (linux)
long STDCALL CLOSE_SHARED_LIBRARY(int64_t * sharedDLLHandle)
#elif defined (salford32)
extern "C" CLOSE_SHARED_LIBRARY(int64_t * sharedDLLHandle)
#endif
{
    SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

#if defined(WIN32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(salford32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(linux)
    (void) dlclose(sharedDLL->dllHandle);
#endif

    /*
     * dllHandle not set to NULL, because FreeLibrary counts the number of 'LoadLibrary's
     */

    return 0;
}
