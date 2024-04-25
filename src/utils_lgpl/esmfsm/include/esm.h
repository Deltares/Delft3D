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
/*------------------------------------------------------------------------------ */
/*  Delft-ESM (Easy Shared Memory) */
/*  API Definitions for ESM */
/* */
/*  Irv.Elshoff@deltares.nl */
/*  3 aug 06 */
/* */
/*------------------------------------------------------------------------------ */


#ifndef ESM_H
#define ESM_H

#include <stdio.h>


/*------------------------------------------------------------------------------ */
/*  Version Definitions */



#if defined(WIN32)
#  define ESMFSM_ARCH "win-32"
#elif defined(salford32)
#  define ESMFSM_ARCH "salford-32"
#elif defined(USE_SGI)
#  define ESMFSM_ARCH "sgi-mips"
#elif defined(USE_SUN)
#  define ESMFSM_ARCH "sun-sparc"
#elif defined(USE_HPUX)
#  define ESMFSM_ARCH "hp-ux"
#elif defined(GNU_PC)
#  define ESMFSM_ARCH "gnu-pc"
#elif defined(HAVE_CONFIG_H)
#  define ESMFSM_ARCH "linux"
#else
#  define ESMFSM_ARCH "NO_ARCH"
#endif


/*------------------------------------------------------------------------------ */
/*  API Function Declarations */


#if defined (__cplusplus)
    extern "C" {
#endif

int
Esm_Init_f (
    int *    flags
    );


int
ESM_Init (
    int     flags
    );

int
ESM_Create_f (
    int     shared,
    int     pagesize
    );

int
ESM_Create (
    int     shared,
    int     pagesize
    );

int
ESM_Delete (
    int     contextid
    );

void *
ESM_Alloc (
    int     contextid,
    char *  name,
    size_t  size
    );

int
ESM_Free (
    int     contextid,
    char *  name
    );

int
ESM_ListContexts (
    FILE *  output
    );

int
ESM_ListRegions (
    int     contextid,
    FILE *  output
    );

char *
ESM_Error (
    void
    );

int
ESM_TraceFile (
    char *  filename
    );

int
ESM_Shared_Info (
    FILE *  output
    );

int
ESM_Finish(
    void
    ) ;


#if defined (__cplusplus)
    }
#endif


/*------------------------------------------------------------------------------ */
/*  Limit Definitions    (coupled with defs in "fsm.i" and "fsm/globals.i") */


#define ESM_ERROR_LEN       2000    /* longest possible error message */
#define ESM_LOCALID_OFFSET  1000    /* used to construct local memory context IDs */
#define ESM_MAGIC       0x7A03C11A  /* magic number for context segments */
#define ESM_MAX_CONTEXTS    100     /* max simultaneously connected contexts */
#define ESM_MAX_NAME        (64+1)  /* max chars in region name */
#define ESM_MAX_PAGES       64      /* max UNIX shared mem segments per process */
#define ESM_MAX_REGIONS     3011    /* max regions (see note below) */
#define ESM_MAX_THREADS     100     /* max threads in process */

    /*  Note: In FSM, a hash table is used for looking up region names. */
    /*  To ensure a good spread ESM_MAX_REGIONS should be a prime number */
    /*  at least 2-3 times as large as the largest number of keys used. */


/*------------------------------------------------------------------------------ */
/*  Flag Definitions  (coupled with definitions in "fsm.i") */


#define ESM_SILENT      1       /* flag to suppress stderr output */
#define ESM_TRACE       2       /* flag for trace output to stderr */


/*------------------------------------------------------------------------------ */
/*  Other (general) Definitions */


#define ESM_OK          0       /* success return code */
#define ESM_ERROR       (-1)    /* error return code */

#define ESM_ALIGNMENT   16      /* allocated mem is aligned to this boundary */

enum {
    ESM_SHARED,
    ESM_LOCAL,
    };


#endif
