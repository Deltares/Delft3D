//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
/*/--description----------------------------------------------------------------- */
/* */
/* Module: sems.cpp  (DELFT3D-FLOW DD) */
/* */
/* Semaphores for file-io */
/* */
/*/--pseudo code and references-------------------------------------------------- */
/* */
/* Stef.Hummel@deltares.nl */
/* Dec '99 */
/* */
/* Remove all misuse of Hydra Executive semaphore routines */
/* Irv.Elshoff@deltares.nl */
/* 22 may 03 */
/* */
/*/------------------------------------------------------------------------------- */


/*
 * INCLUDE FILES AND DEFINITIONS
 */

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include <pthread.h>

/*
 * C-functions called from fortran
 */

#if linux
#   include "config.h"
#   define STDCALL  /* nothing */
#   define PSEMINIT   FC_FUNC(pseminit,PSEMINIT)
#   define VSEMINIT   FC_FUNC(vseminit,VSEMINIT)
#   define PSEMLUN    FC_FUNC(psemlun,PSEMLUN)
#   define VSEMLUN    FC_FUNC(vsemlun,VSEMLUN)
#   define PSEMNEFIS  FC_FUNC(psemnefis,PSEMNEFIS)
#   define VSEMNEFIS  FC_FUNC(vsemnefis,VSEMNEFIS)
#   define PSEMFINISH FC_FUNC(psemfinish,PSEMFINISH)
#   define VSEMFINISH FC_FUNC(vsemfinish,VSEMFINISH)
#else
/* WIN32 */
#   define STDCALL  /* nothing */
#   define PSEMINIT   PSEMINIT
#   define VSEMINIT   VSEMINIT
#   define PSEMLUN    PSEMLUN
#   define VSEMLUN    VSEMLUN
#   define PSEMNEFIS  PSEMNEFIS
#   define VSEMNEFIS  VSEMNEFIS
#   define PSEMFINISH PSEMFINISH
#   define VSEMFINISH VSEMFINISH
#endif


/*
 * Functions Declaration
 */

#if defined(__cplusplus)
extern "C" {
#endif

void STDCALL PSEMINIT(void);
void STDCALL VSEMINIT(void);
void STDCALL PSEMLUN(void);
void STDCALL VSEMLUN(void);
void STDCALL PSEMNEFIS(void);
void STDCALL VSEMNEFIS(void);
void STDCALL PSEMFINISH(void);
void STDCALL VSEMFINISH(void);
void STDCALL SEMEXIT(void);

#if defined(__cplusplus)
}
#endif


/*
 * Global variables
 */

static pthread_mutex_t inimutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t nfsmutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t lunmutex =  PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t finmutex =  PTHREAD_MUTEX_INITIALIZER;




void STDCALL PSEMINIT(void)          /* Initialize   */
{
    pthread_mutex_lock(&inimutex);
}
void STDCALL VSEMINIT(void)
{
    pthread_mutex_unlock(&inimutex);
}



void STDCALL PSEMLUN(void)          /* New Lun      */
{
    pthread_mutex_lock(&lunmutex);
}
void STDCALL VSEMLUN(void)
{
    pthread_mutex_unlock(&lunmutex);
}



void STDCALL PSEMNEFIS(void)            /* Nefis put/get r/c/l/i*/
{
    pthread_mutex_lock(&nfsmutex);
}
void STDCALL VSEMNEFIS(void)
{
    pthread_mutex_unlock(&nfsmutex);
}



void STDCALL PSEMFINISH(void)           /* Finish up */
{
    pthread_mutex_lock(&finmutex);
}
void STDCALL VSEMFINISH(void)
{
    pthread_mutex_unlock(&finmutex);
}


void STDCALL SEMEXIT(void)
{
// This function should be called by the main thread, after all sub threads are finished
// Unfortunately, the main thread is stopped after initialization
#if defined(WIN32)
    if ( inimutex != 0 )
    {
        pthread_mutex_destroy(&inimutex);
    }
    if ( lunmutex != 0 )
    {
        pthread_mutex_destroy(&lunmutex);
    }
    if ( nfsmutex != 0 )
    {
        pthread_mutex_destroy(&nfsmutex);
    }
    if ( finmutex != 0 )
    {
        pthread_mutex_destroy(&finmutex);
    }
#endif
}
