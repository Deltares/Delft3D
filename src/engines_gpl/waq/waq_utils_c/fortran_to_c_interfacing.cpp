//  Copyright (C)  Stichting Deltares, 2012-2025.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 3,
//  as published by the Free Software Foundation.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to registered trademarks
//  of Stichting Deltares remain the property of Stichting Deltares. All
//  rights reserved.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef min
#  define min(a, b) (a)<(b) ? (a) : (b)
#  define max(a, b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32) || defined(SALF)

#  include <windows.h>

#elif defined(linux)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION PERF_FUNCTION
#  define LOAD____FUNCTION LOAD_FUNCTION
#  define STDCALL /* nothing */
#elif defined(SALF)
#  define OPEN_SHARED_LIBRARY  open_shared_library_
#  define CLOSE_SHARED_LIBRARY close_shared_library_
#  define PERFORM_FUNCTION perf_function_
#  define LOAD____FUNCTION load_function_
#  define STDCALL
#elif defined(linux)
#  define OPEN_SHARED_LIBRARY  open_shared_library_
#  define CLOSE_SHARED_LIBRARY close_shared_library_
#  define PERFORM_FUNCTION perf_function_
#  define LOAD____FUNCTION load_function_
#  define STDCALL
#endif

/*
 * Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
 * Special attention to the WINAPI define, which is needed if the DLL is written in F90
 *
 */

#if defined(WIN32) || defined (SALF)
typedef HINSTANCE DllHandle;
#elif defined(linux)
typedef void * DllHandle;
#endif

typedef struct {
    DllHandle dllHandle;
} SharedDLL;

/*
 * ============================================================================
 */
char *strFcpy(char *str_1, int len) {
    int m;
    char *str_2;
    m = min(len, (int) strlen(str_1));
    str_2 = (char *) malloc(sizeof(char) * (m + 1));
    strncpy(str_2, str_1, m);
    str_2[m] = '\0';
    return str_2;
}

void RemoveTrailingBlanksXX(char *String) {
    int i;
    i = strlen(String) - 1;
    while (String[i] == ' ' ||
           String[i] == '\n' ||
           String[i] == '\t') {
        String[i] = '\0';
        i--;
    }
    return;
}
/*
 * ============================================================================
 */
extern "C" {
/*
 * OPEN_SHARED_LIBRARY and CLOSE_SHARED_LIBRARY are used from utils_lgpl\deltares_common\packages\deltares_common_c\src\shared_library_fortran_api.c
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION(DllHandle *sharedDLLHandle,
                              char *function,
                              float *pmsa,
                              float *fl,
                              long *ipoint,
                              long *increm,
                              long *noseg,
                              long *noflux,
                              long *iexpnt,
                              long *iknmrk,
                              long *noq1,
                              long *noq2,
                              long *noq3,
                              long *noq4,
                              long length_function)
#elif defined(linux) || defined(SALF)
/* TODO: This requires thinking about! */
long STDCALL PERFORM_FUNCTION(long  * sharedDLLHandle ,
                              char  * function,
                              float * pmsa    ,
                              float * fl      ,
                              long  * ipoint  ,
                              long  * increm  ,
                              long  * noseg   ,
                              long  * noflux  ,
                              long  * iexpnt  ,
                              long  * iknmrk  ,
                              long  * noq1    ,
                              long  * noq2    ,
                              long  * noq3    ,
                              long  * noq4    ,
                              long    length_function)
#endif
{
    int error = 1;
    typedef void *(STDCALL *MyProc)(float *, float *,
                                    long *, long *, long *, long *, long *,
                                    long *, long *, long *, long *, long *);
    MyProc proc;
    char *fun_name;
    SharedDLL *sharedDLL = (SharedDLL *) (*sharedDLLHandle);

    if (sharedDLL != NULL) {
        fun_name = strFcpy(function, length_function);
        RemoveTrailingBlanksXX(fun_name);

#if defined(WIN32) || defined (SALF)
        proc = (MyProc) GetProcAddress(sharedDLL->dllHandle, fun_name);
#elif defined(linux)
        proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

        if (proc != NULL) {
            error = 0;
            (void *) (*proc)(pmsa, fl,
                             ipoint, increm, noseg, noflux, iexpnt,
                             iknmrk, noq1, noq2, noq3, noq4);
        }
        free(fun_name);
        fun_name = NULL;
    }
    return error;
}

/*
 * ============================================================================
 */
}
