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
//------------------------------------------------------------------------------
//  Delft-ESM (Easy Shared Memory)
//  Definitions for use by ESM itself
//
//  Irv.Elshoff@deltares.nl
//  12 nov 04
//
//------------------------------------------------------------------------------

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include "esm.h"
#include <stdlib.h>

#if defined (HAVE_CONFIG_H)
#include "config.h"
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#else
#include <malloc.h>
#endif

#include <math.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>


#if (!defined (WIN32))
#   include <sys/ipc.h>
#   include <sys/shm.h>
#   include <sys/sem.h>
#   include <unistd.h>
#endif

#ifndef SHM_R
#define SHM_R      0440   /* defined on most systems, except cygwin */
#endif
#ifndef SHM_W
#define SHM_W      0220   /* defined on most systems, except cygwin */
#endif

#if (defined (WIN32))
#ifndef FALSE
#  define FALSE 0
#  define TRUE  1
#endif
#endif

#if (defined (HAVE_CONFIG_H))
#   if (! defined (SEM_A))
#       define SEM_A    0200    /* alter permission */
#   endif
#   if (! defined (SEM_R))
#       define SEM_R    0400    /* read permission */
#   endif
#endif

#if (defined (SUN_SPARC))
    typedef char *      shmem_t;
#else
    typedef void *      shmem_t;
#endif


//------------------------------------------------------------------------------
//    Global variables


#ifdef ESM_MAIN
#   define Extern
#else
#   define Extern extern
#endif


//  The following variables are static for the process using ESM and all
//  its threads.  It records global information about ESM, such as the flags,
//  which contexts are attached, etc.

Extern int      Flags;                  // global flags (for before thread init)
Extern int      InitFails;              // non-zero if ESM_Init fails early on
Extern int      NumThreads;             // number of threads that have called ESM_Init
Extern int      NumContexts;            // number of contexts we know about
Extern char     Error [ESM_ERROR_LEN];  // error message of last failed operation

Extern pthread_mutex_t  Mutex;          // mutual exclusion for global variables
Extern pthread_key_t    Self;           // set to thread ID

Extern struct esm_thread {
    int     flags;                      // thread-specific flags
    FILE *  tracefile;                  // descriptor for trace output
    char    error [ESM_ERROR_LEN];      // error message of last failed operation
    } Threads [ESM_MAX_THREADS];

Extern struct esm_context {
    int     contextid;                  // context ID
    void *  cd;                         // local addr of context descriptor
    int     nattached;                  // number of mapped pages (shared only)
    void *  pageaddr [ESM_MAX_PAGES];   // local address of mapped pages (shared only)
    } Contexts [ESM_MAX_CONTEXTS];


//------------------------------------------------------------------------------
//  Utility macros

#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))


//------------------------------------------------------------------------------
//  Internal Functions


static void
InitProcess (
    void
    );

void
SetError (
    int     thid,
    char *  message,
    ...
    );

void
ClearError (
    int     thid
    );

int
ESM_Local_Create (
    int thid
    );

int
ESM_Local_Delete (
    int thid,
    int ci,
    int contextid
    );

void *
ESM_Local_Alloc (
    int thid,
    int ci,
    int contextid,
    char * name,
    size_t size
    );

int
ESM_Local_Free (
    int thid,
    int ci,
    int contextid,
    char * name
    );

int
ESM_Local_ListRegions (
    int thid,
    int ci,
    int contextid,
    FILE * output
    );

int
ESM_Shared_Create (
    int thid,
    int pagesize
    );

int
ESM_Shared_Delete (
    int thid,
    int ci,
    int contextid
    );

void *
ESM_Shared_Alloc (
    int thid,
    int ci,
    int contextid,
    char * name,
    size_t size
    );

int
ESM_Shared_Free (
    int thid,
    int ci,
    int contextid,
    char * name
    );

int
ESM_Shared_ListRegions (
    int thid,
    int ci,
    FILE * output
    );


//------------------------------------------------------------------------------
//  In order to simplify the implementation of the ESM_TRACE flag,
//  each API routine has exactly one exit point: the end of the
//  function.  The RETURN macro uses "goto" to enfore this without
//  cluttering the code with overly nested if-then-else constructs.
//  Sorry Edsgar.


#define RETURN(V) { \
    return_value = V; \
    goto return_label; \
    }


//------------------------------------------------------------------------------
//  Debugging Support


#define ESM_DEBUG(X) { \
    int flags = (thid < 0) ? Flags : Threads[thid].flags; \
    if (flags & ESM_TRACE) { \
        FILE * output = (thid < 0 || Threads[thid].tracefile == NULL) ? stdout : Threads[thid].tracefile; \
        fprintf (output, "ESM Trace: "); \
        fprintf X ; \
        fputc ('\n', output); \
        fflush (output); \
        } \
    }


//------------------------------------------------------------------------------

#define WINDOWS_NOSM    "ESM shared memory not available on Microsoft Windows"

