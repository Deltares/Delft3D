//---- AGPL ---------------------------------------------------------------------
//                                                                               
//  Copyright (C)  Stichting Deltares, 2011-2024.                                
//                                                                               
//  This program is free software: you can redistribute it and/or modify         
//  it under the terms of the GNU Affero General Public License as               
//  published by the Free Software Foundation version 3.                         
//                                                                               
//  This program is distributed in the hope that it will be useful,              
//  but WITHOUT ANY WARRANTY; without even the implied warranty of               
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
//  GNU Affero General Public License for more details.                          
//                                                                               
//  You should have received a copy of the GNU Affero General Public License     
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
//                                                                               
//  contact: delft3d.support@deltares.nl                                         
//  Stichting Deltares                                                           
//  P.O. Box 177                                                                 
//  2600 MH Delft, The Netherlands                                               
//                                                                               
//  All indications and logos of, and references to, "Delft3D" and "Deltares"    
//  are registered trademarks of Stichting Deltares, and remain the property of  
//  Stichting Deltares. All rights reserved.                                     
//                                                                               
//-------------------------------------------------------------------------------

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
#elif defined(HAVE_CONFIG_H)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define RTC_OPEN_SHARED_LIBRARY  RTC_OPEN_SHARED_LIBRARY
#  define RTC_CLOSE_SHARED_LIBRARY RTC_CLOSE_SHARED_LIBRARY
#  define RTC_PERFORM_FUNCTION RTC_PERF_FUNCTION
#  define STDCALL
#elif defined(salford32)
#  define RTC_OPEN_SHARED_LIBRARY  RTC_OPEN_SHARED_LIBRARY
#  define RTC_CLOSE_SHARED_LIBRARY RTC_CLOSE_SHARED_LIBRARY
#  define RTC_PERFORM_FUNCTION RTC_PERF_FUNCTION
#  define STDCALL __stdcall
#elif defined(HAVE_CONFIG_H)
#  define RTC_OPEN_SHARED_LIBRARY  rtc_open_shared_library_
#  define RTC_CLOSE_SHARED_LIBRARY rtc_close_shared_library_
#  define RTC_PERFORM_FUNCTION rtc_perf_function_
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
  int i;
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
#if defined(WIN32) || defined (HAVE_CONFIG_H)
long STDCALL RTC_OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
#elif defined (salford32)
extern "C" RTC_OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
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
#elif defined(HAVE_CONFIG_H)
    tmpSharedDLL->dllHandle = dlopen(lib_name, RTLD_LAZY);
#endif

    if (tmpSharedDLL->dllHandle != NULL)
    {
        error = 0;
        *sharedDLLHandle = (long) tmpSharedDLL;
    }

    free(lib_name); lib_name = NULL;

    return error;
}
/*
 * ============================================================================
 */

#if defined (WIN32) || defined (HAVE_CONFIG_H)
long STDCALL RTC_CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#elif defined (salford32)
extern "C" RTC_CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#endif
{
    SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

#if defined(WIN32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(salford32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(HAVE_CONFIG_H)
    (void) dlclose(sharedDLL->dllHandle);
#endif

    /*
     * dllHandle not set to NULL, because FreeLibrary counts the number of 'LoadLibrary's
     */

    return 0;
}
/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL RTC_PERFORM_FUNCTION(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * sobekfirst         ,
                              long   * sobekdate          ,
                              long   * sobektime          ,
                              long   * sobektimestepsize  ,
                              long   * maxsobek           ,
                              double * dllsobekh          ,
                              double * dllsobekq          ,
                              double * dllsobeksa         ,
                              double * dllsobekwd         ,
                              double * dllsobekcl         ,
                              double * dllsobekcw         ,
                              double * dllsobekgl         ,
                              double * dllsobekgo         ,
                              double * dllsobekfa         ,
                              double * dllsobekqs         ,
                              double * dllsobekvs         ,
                              double * dllsobekhu         ,
                              double * dllsobekhd         ,
                              double * dllsobekdh         ,
                              double * dllsobekpd         ,
                              double * dllsobekpc         ,
                              double * dllsobek1d2dbl     ,
                              double * dllsobek1d2du      ,
                              double * dllsobek1d2dv      ,
                              double * dllsobek1d2dc      ,
                              double * dllsobekvr         ,
                              long   * maxrr              ,
                              double * dllrrh             ,
                              long   * maxrain            ,
                              double * dllrainh           ,
                              long   * maxd3d             ,
                              double * dlld3dh            ,
                              double * dlld3dsal          ,
                              long   * maxlocwq           ,
                              long   * maxparwq           ,
                              double ** dllsobekwq        ,
                              long   * maxsetpoints       ,
                              double * dllsobekc          ,
                              double * dllsobeks          ,
                              long   * returncode         ,
                              char  ** dllidsbr           ,
                              char  ** dllidd3b           ,
                              char  ** dllidpre           ,
                              char  ** dllidd3d           ,
                              char  ** dlllocidwq         ,
                              char  ** dllparidwq         ,
                              char  ** dllmeasid          ,
                              long     length_function    ,
                              long     length_dllidsbr    ,
                              long     length_dllidd3b    ,
                              long     length_dllidpre    ,
                              long     length_dllidd3d    ,
                              long     length_dlllocidwq  ,
                              long     length_dllparidwq  ,
                              long     length_dllmeasid   )
#elif defined(salford32)
extern "C" RTC_PERFORM_FUNCTION(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * sobekfirst         ,
                              long   * sobekdate          ,
                              long   * sobektime          ,
                              long   * sobektimestepsize  ,
                              long   * maxsobek           ,
                              double * dllsobekh          ,
                              double * dllsobekq          ,
                              double * dllsobeksa         ,
                              double * dllsobekwd         ,
                              double * dllsobekcl         ,
                              double * dllsobekcw         ,
                              double * dllsobekgl         ,
                              double * dllsobekgo         ,
                              double * dllsobekfa         ,
                              double * dllsobekqs         ,
                              double * dllsobekvs         ,
                              double * dllsobekhu         ,
                              double * dllsobekhd         ,
                              double * dllsobekdh         ,
                              double * dllsobekpd         ,
                              double * dllsobekpc         ,
                              double * dllsobek1d2dbl     ,
                              double * dllsobek1d2du      ,
                              double * dllsobek1d2dv      ,
                              double * dllsobek1d2dc      ,
                              double * dllsobekvr         ,
                              long   * maxrr              ,
                              double * dllrrh             ,
                              long   * maxrain            ,
                              double * dllrainh           ,
                              long   * maxd3d             ,
                              double * dlld3dh            ,
                              double * dlld3dsal          ,
                              long   * maxlocwq           ,
                              long   * maxparwq           ,
                              double ** dllsobekwq        ,
                              long   * maxsetpoints       ,
                              double * dllsobekc          ,
                              double * dllsobeks          ,
                              long   * returncode         ,
                              char  ** dllidsbr           ,
                              char  ** dllidd3b           ,
                              char  ** dllidpre           ,
                              char  ** dllidd3d           ,
                              char  ** dlllocidwq         ,
                              char  ** dllparidwq         ,
                              char  ** dllmeasid          ,
                              long     length_function    ,
                              long     length_dllidsbr    ,
                              long     length_dllidd3b    ,
                              long     length_dllidpre    ,
                              long     length_dllidd3d    ,
                              long     length_dlllocidwq  ,
                              long     length_dllparidwq  ,
                              long     length_dllmeasid   )
#elif defined (HAVE_CONFIG_H)
long STDCALL RTC_PERFORM_FUNCTION(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * sobekfirst         ,
                              long   * sobekdate          ,
                              long   * sobektime          ,
                              long   * sobektimestepsize  ,
                              long   * maxsobek           ,
                              double * dllsobekh          ,
                              double * dllsobekq          ,
                              double * dllsobeksa         ,
                              double * dllsobekwd         ,
                              double * dllsobekcl         ,
                              double * dllsobekcw         ,
                              double * dllsobekgl         ,
                              double * dllsobekgo         ,
                              double * dllsobekfa         ,
                              double * dllsobekqs         ,
                              double * dllsobekvs         ,
                              double * dllsobekhu         ,
                              double * dllsobekhd         ,
                              double * dllsobekdh         ,
                              double * dllsobekpd         ,
                              double * dllsobekpc         ,
                              double * dllsobek1d2dbl     ,
                              double * dllsobek1d2du      ,
                              double * dllsobek1d2dv      ,
                              double * dllsobek1d2dc      ,
                              double * dllsobekvr         ,
                              long   * maxrr              ,
                              double * dllrrh             ,
                              long   * maxrain            ,
                              double * dllrainh           ,
                              long   * maxd3d             ,
                              double * dlld3dh            ,
                              double * dlld3dsal          ,
                              long   * maxlocwq           ,
                              long   * maxparwq           ,
                              double ** dllsobekwq        ,
                              long   * maxsetpoints       ,
                              double * dllsobekc          ,
                              double * dllsobeks          ,
                              long   * returncode         ,
                              char  ** dllidsbr           ,
                              char  ** dllidd3b           ,
                              char  ** dllidpre           ,
                              char  ** dllidd3d           ,
                              char  ** dlllocidwq         ,
                              char  ** dllparidwq         ,
                              char  ** dllmeasid          ,
                              long     length_function    ,
                              long     length_dllidsbr    ,
                              long     length_dllidd3b    ,
                              long     length_dllidpre    ,
                              long     length_dllidd3d    ,
                              long     length_dlllocidwq  ,
                              long     length_dllparidwq  ,
                              long     length_dllmeasid   )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    long   *, long   *,
                                    long   *,
                                    double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *,
                                    long   *, double *,
                                    long   *, double *,
                                    long   *, double *, double *,
                                    long   *, long   *, double **,
                                    long   *, double *, double *, long   *,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    long    , long    , long    , long    , long   , long   , long);
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    long   *, long   *,
                                    long   *,
                                    double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *, double *,
                                    double *, double *, double *, double *, double *,
                                    long   *, double *,
                                    long   *, double *,
                                    long   *, double *, double *,
                                    long   *, long   *, double **,
                                    long   *, double *, double *, long   *,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    char  **,
                                    long    , long    , long    , long    , long   , long   , long);
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
     (void *) (*proc)(sobekfirst  , sobekdate   ,  sobektime,   sobektimestepsize,
                      maxsobek    ,
                      dllsobekh     , dllsobekq     , dllsobeksa    , dllsobekwd    ,
                      dllsobekcl    , dllsobekcw    , dllsobekgl    , dllsobekgo    , dllsobekfa    , dllsobekqs    ,
                      dllsobekvs    , dllsobekhu    , dllsobekhd    , dllsobekdh    , dllsobekpd    , dllsobekpc    ,
                      dllsobek1d2dbl, dllsobek1d2du , dllsobek1d2dv , dllsobek1d2dc , dllsobekvr    ,
                      maxrr         , dllrrh        ,
                      maxrain       , dllrainh      ,
                      maxd3d        , dlld3dh       , dlld3dsal     ,
                      maxlocwq      , maxparwq      , dllsobekwq    ,
                      maxsetpoints  , dllsobekc     , dllsobeks     , returncode    ,
                      dllidsbr      , dllidd3b      , dllidpre      ,
                      dllidd3d      , dlllocidwq    , dllparidwq    , dllmeasid     ,
                      length_dllidsbr, length_dllidd3b, length_dllidpre,
                      length_dllidd3d, length_dlllocidwq, length_dllparidwq, length_dllmeasid);
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(sobekfirst  , sobekdate   ,  sobektime,   sobektimestepsize,
                      maxsobek    ,
                      dllsobekh     , dllsobekq     , dllsobeksa    , dllsobekwd    ,
                      dllsobekcl    , dllsobekcw    , dllsobekgl    , dllsobekgo    , dllsobekfa    , dllsobekqs    ,
                      dllsobekvs    , dllsobekhu    , dllsobekhd    , dllsobekdh    , dllsobekpd    , dllsobekpc    ,
                      dllsobek1d2dbl, dllsobek1d2du , dllsobek1d2dv , dllsobek1d2dc , dllsobekvr    ,
                      maxrr         , dllrrh        ,
                      maxrain       , dllrainh      ,
                      maxd3d        , dlld3dh       , dlld3dsal     ,
                      maxlocwq      , maxparwq      , dllsobekwq    ,
                      maxsetpoints  , dllsobekc     , dllsobeks     , returncode    ,
                      dllidsbr      , dllidd3b      , dllidpre      ,
                      dllidd3d      , dlllocidwq    , dllparidwq    , dllmeasid     ,
                      length_dllidsbr, length_dllidd3b, length_dllidpre,
                      length_dllidd3d, length_dlllocidwq, length_dllparidwq, length_dllmeasid);
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
