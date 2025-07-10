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
//  Delft-FSM (Fortran Shared Memory)
//  Definitions for use by FSM itself (in fsm.cpp)
//
//  Irv.Elshoff@deltares.nl
//  12 nov 04
//
//------------------------------------------------------------------------------

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#ifndef FSM_H
#define FSM_H

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#if (!defined (WIN32))
#   include <unistd.h>
#endif

#define PTR_VOID_PTR


#include "esm.h"

#if (defined (WIN32))
#ifndef FALSE
#  define FALSE 0
#  define TRUE  1
#endif
#endif

#define UNDEFINED   (-1)


#define FSM_OK      1
#define FSM_ERROR   0


//------------------------------------------------------------------------------
//  FSM Redefinitions of ESM constants


#define FSM_MAX_NAME        ESM_MAX_NAME
#define FSM_MAX_PAGES       ESM_MAX_PAGES
#define FSM_MAX_REGIONS     ESM_MAX_REGIONS
#define FSM_MAX_CON     ESM_MAX_CON

#define FSM_SILENT      ESM_SILENT
#define FSM_TRACE       ESM_TRACE


//------------------------------------------------------------------------------
//    Type definitions


typedef struct {
    char    key [FSM_MAX_NAME]; // key name
    int     length;     // number of elements in array
    int     type;       // element type (integer, real, ...)
    int     eltsize;        // size in bytes of a single element
    } KeyTable;


//------------------------------------------------------------------------------
//  Utility macros

#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))


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

#define RETURN_VOID { \
    goto return_label; \
    }


//------------------------------------------------------------------------------
//  Debugging Support


#define FSM_DEBUG(X) { \
    int flags = (thid < 0) ? FSM.Flags : FSM.Threads[thid].flags; \
    if (flags & FSM_TRACE) { \
        FILE * output = (thid < 0 || FSM.Threads[thid].tracefile == NULL) ? stdout : FSM.Threads[thid].tracefile; \
    fprintf (output, "FSM Trace: "); \
    fprintf X ; \
    fputc ('\n', output); \
    fflush (output); \
    } \
    }


//------------------------------------------------------------------------------
//  Definitions and declarations for Fortran-C interface.
//
#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define FSM_Init            FC_FUNC(fsm_init,FSM_INIT)
#   define FSM_MakePointer     FC_FUNC(fsm_makepointer,FSM_MAKEPOINTER)
#   define FSM_GetPointer      FC_FUNC(fsm_getpointer,FSM_GETPOINTER)
#   define FSM_ReleasePointer  FC_FUNC(fsm_releasepointer,FSM_RELEASEPOINTER)
#   define FSM_PrintKeys       FC_FUNC(fsm_printkeys,FSM_PRINTKEYS)
#   define FSM_Err             FC_FUNC(fsm_err,FSM_ERR)
#   define FSM_TraceFile       FC_FUNC(fsm_tracefile,FSM_TRACEFILE)
#   define FSM_CHECK_ALIGNMENT FC_FUNC(fsm_check_alignment,FSM_CHECK_ALIGNMENT)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define FSM_Init            FSM_INIT
#   define FSM_MakePointer     FSM_MAKEPOINTER
#   define FSM_GetPointer      FSM_GETPOINTER
#   define FSM_ReleasePointer  FSM_RELEASEPOINTER
#   define FSM_PrintKeys       FSM_PRINTKEYS
#   define FSM_Err             FSM_ERR
#   define FSM_TraceFile       FSM_TRACEFILE
#   define FSM_CHECK_ALIGNMENT FSM_CHECK_ALIGNMENT
#endif

#if defined (PTR_UNS_LONG)
    typedef unsigned long pointer;
#elif defined (PTR_VOID_PTR)
    typedef void * pointer;
#endif


#if defined (_cplusplus) || defined (__cplusplus)
extern "C" {
#endif

#if defined(WIN32)
    int     STDCALL FSM_Init (int *, int *);
    pointer STDCALL FSM_MakePointer (char *, int *, int *, int *);
    pointer STDCALL FSM_GetPointer (char *, int *);
    pointer STDCALL FSM_ReleasePointer (char *, int *);
    int     STDCALL FSM_PrintKeys (void);
    int     STDCALL FSM_Err (char *, int *);
    int     STDCALL FSM_TraceFile (char *);
    void    STDCALL FSM_CHECK_ALIGNMENT (void);

#else
    int     STDCALL FSM_Init (int *, int *);
    void *  STDCALL FSM_MakePointer (char *, int *, int *, int *);
    void *  STDCALL FSM_GetPointer (char *, int *);
    void *  STDCALL FSM_ReleasePointer (char *, int *);
    int     STDCALL FSM_PrintKeys (void);
    int     STDCALL FSM_Err (char *, int *);
    int     STDCALL FSM_TraceFile (char *);
    void    STDCALL FSM_CHECK_ALIGNMENT (void);

#endif

#if defined (_cplusplus) || defined (__cplusplus)
}
#endif

//  Note:
//  FTN_UNDERSCORE: linux, sgi, convex, sun, nec-sx4
//  FTN_SMALL: hp, ibm
//  No define: CRAY, Dec-Alpha, Salford (Windows 32-bits) Microsoft Visual C++
//  Define a universal pointer. Eight bytes covers all 32- and 64-bit architectures.
//  PTR_UNS_LONG: sgi, nec-sx4
//  PTR_VOID_PTR: hp, MS-Visual C++,sun


//------------------------------------------------------------------------------
//    Global variables


#ifdef FSM_MAIN
#   define Extern
#else
#   define Extern extern
#endif


//  The following variables are static for the process using FSM and all
//  its threads.  It records global information about FSM, such as the flags,
//  which contexts are attached, etc.

struct globals {
    int     Flags;              // global flags (for before thread init)
    int     InitFails;          // non-zero if FSM_Init fails early on
    int     NumThreads;         // number of threads that have called FSM_Init
    int     NumContexts;        // number of contexts we know about
    char    Error [ESM_ERROR_LEN];  // error message of last failed operation

    pthread_mutex_t Mutex;          // mutual exclusion for global variables
    pthread_key_t   Self;           // set to thread ID

    pointer Pointer [FSM_MAX_REGIONS];

    struct {
    int         flags;              // thread-specific flags
    int         contextid;          // current context
    KeyTable *  keytable;
    FILE *          tracefile;              // descriptor for trace output
    char        error [ESM_ERROR_LEN];  // error message of last failed operation
    } Threads [ESM_MAX_THREADS];
    };

Extern struct globals FSM;


//------------------------------------------------------------------------------
//  FSM Type Definitions      (see also fsm.i).


typedef unsigned int fint;  /* Fortran integer (in FSM) */

typedef enum fsm_tag {
    FSM_INTEGER     = 1,
    FSM_REAL        = 2,
    FSM_DOUBLE_PREC = 3,
    FSM_DOUBLE_COMP = 4,
    FSM_COMPLEX     = 5,
    FSM_LOGICAL     = 6,
    FSM_CHARACTER   = 7
    } alloc_type;

static const char *fsm_type_name [] = {     // note: must correspond with enum
    "",
    "integer",
    "real",
    "double prec",
    "double comp",
    "complex",
    "logical",
    "character",
    };

static const int fsm_type_size [] = {       // note: must match fortapi.f90
    0,
    4,  4,  8, 16,  8,  4,  1
    };


#endif //FSM_H
