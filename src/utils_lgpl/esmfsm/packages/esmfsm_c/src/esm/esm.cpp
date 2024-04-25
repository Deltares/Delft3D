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
//------------------------------------------------------------------------------
//  Delft-ESM (Easy Shared Memory)
//  Core functions
//
//  Contents of this file:
//      Internal macro functions
//      Internal functions
//      API routines
//
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
//------------------------------------------------------------------------------


#define ESM_MAIN
#include "globals-esm.h"

#if (defined(WIN32))
static int getpid() { return 0; }
#endif

static pthread_once_t   ThreadInit = PTHREAD_ONCE_INIT;


extern "C"
{
extern void getFullVersionString_ESMFSM( char * );
}
//------------------------------------------------------------------------------
//  Internal macro functions


#define LOCK(E) { \
    if (pthread_mutex_lock (&Mutex) != 0) { \
        SetError (thid, "Cannot lock mutex"); \
        RETURN (E) \
        } \
    }

#define UNLOCK(E) { \
    if (pthread_mutex_unlock (&Mutex) != 0) { \
        SetError (thid, "Cannot unlock mutex"); \
        RETURN (E) \
        } \
    }

#define GETTHREADLOCK(I,F,E) { \
    LOCK (E) \
    if (NumThreads <= 0) { \
        SetError (I, "ESM_Init must be called before %s", F); \
        UNLOCK (E) \
        RETURN (E) \
        } \
    int * idp; \
    if ((idp = (int *) pthread_getspecific (Self)) == NULL) { \
        SetError (thid, "Cannot get thread ID in %s", F); \
        UNLOCK (E) \
        RETURN (E) \
        } \
    I = *idp; \
    }

#define LOOKUP_CONTEXT(C,I,T,F,E) { \
    if ((C) == 0) { \
        SetError (T, "Context ID given to %s is zero", F); \
        RETURN (E) \
        } \
    else if ((C) > 0) { \
        for (I = 0 ; I < NumContexts ; I++) { \
            if (Contexts[I].contextid == (C)) \
                break; \
            } \
        if (I == NumContexts) { \
            I = -1; \
            } \
        } \
    else { \
        if ((C) < -(ESM_LOCALID_OFFSET+ESM_MAX_CONTEXTS) || (C) > -ESM_LOCALID_OFFSET) { \
            SetError (T, "Invalid context ID %d given to %s", C, F); \
            RETURN (E) \
            } \
        I = -((C) + ESM_LOCALID_OFFSET); \
        if (Contexts[I].contextid != (C)) { \
            SetError (T, "Unknown context ID %d in %s", C, F); \
            RETURN (E) \
            } \
        } \
    }


//------------------------------------------------------------------------------
//  Internal functions


static void
InitProcess (
    void
    ) {

    //  This routine is called once to initialize ESM for this process

    InitFails   = 0;
    NumThreads  = 0;
    NumContexts = 0;
    Self        = (pthread_key_t) NULL;

    //  Create mutual exclusion object for controlling access to global data

    if (pthread_mutex_init (&Mutex, NULL) != 0) {
    SetError (-1, "ESM_Init cannot create thread mutex");
        InitFails = 1;
        return;
        }

    //  Create thread-specific pointer to thread ID

    if (pthread_key_create (&Self, NULL) != 0) {
    SetError (-1, "ESM_Init cannot create thread-specific key");
        InitFails = 1;
        return;
        }
     }


void
SetError (
    int     thid,
    char *  message,
    ...
    ) {

    va_list     arguments;
    char * buf = (thid < 0) ? Error : Threads[thid].error;

    va_start (arguments, message);
    vsprintf (buf, message, arguments);
    va_end (arguments);

    int flags = (thid < 0) ? Flags : Threads[thid].flags;
    if (~flags & ESM_SILENT) {
        printf ("%s\n", buf);
        fflush (stdout);
        }
    }


void
ClearError (
    int     thid
    ) {

    char * buf = (thid < 0) ? Error : Threads[thid].error;
    sprintf (buf, "");
    }


//------------------------------------------------------------------------------
//  API Routines
//  In most cases just call shared or local memory implementation
//  depending on context.


int
ESM_Init (
    int     flags
    ) {

    int return_value = -999;
    int thid = -999;
    int * thidp = new int;
    int * testp;

    Flags = flags;
    if ((Flags & ESM_TRACE) && (Flags & ESM_SILENT))        // let trace prevail
    Flags &= ~ESM_SILENT;

    ESM_DEBUG ((output, "Entering ESM_Init (flags=0x%x)", flags))


    pthread_once (&ThreadInit, InitProcess);



    if (InitFails) {
        InitFails = 0;
        RETURN (ESM_ERROR)
        }

    //  Determine and set the thread ID

    LOCK (ESM_ERROR)
    thid = *thidp = NumThreads++;

    if (pthread_setspecific (Self, (void *) thidp) != 0) {
    SetError (thid, "ESM_Init cannot set thread ID");
        UNLOCK (ESM_ERROR)
        RETURN (ESM_ERROR)
        }

    if ((testp = (int *) pthread_getspecific (Self)) == NULL) {
    SetError (thid, "ESM_Init cannot get thread ID");
        UNLOCK (ESM_ERROR)
        RETURN (ESM_ERROR)
        }

    if (thid != *testp) {
    SetError (thid, "ESM_Init thread ID doesn't match self");
        UNLOCK (ESM_ERROR)
        RETURN (ESM_ERROR)
        }

    //  Initialize thread-specific ESM information

    Threads[thid].flags = flags;
    Threads[thid].error[0] = '\0';

    UNLOCK (ESM_ERROR)
    RETURN (ESM_OK)

    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_Init (flags=0x%x)", return_value, flags))
        return return_value;
        }
    }


int
ESM_Create (
    int     shared,
    int     pagesize
    ) {

    int return_value = -999;
    int thid = -999;

    GETTHREADLOCK (thid, "ESM_Create", 0)
    ESM_DEBUG ((output, "Entering ESM_Create (shared=%d, pagesize=%d)", shared, pagesize))
    ClearError (thid);

    if (shared) {
#ifdef WIN32
        SetError (thid, WINDOWS_NOSM);
        RETURN (0)
#else
        if (pagesize <= 0) {
            SetError (thid, "Invalid page size (%d) given to ESM_Create", pagesize);
            RETURN (0)
            }

        RETURN (ESM_Shared_Create (thid, pagesize))
#endif
        }
    else
        RETURN (ESM_Local_Create (thid))

    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_Create (shared=%d, pagesize=%d)", return_value, shared, pagesize))
        UNLOCK (0)
        return return_value;
        }
    }


int
ESM_Delete (
    int     contextid
    ) {

    int return_value = -999;
    int thid = -999;
    int ci;             // context table index

    GETTHREADLOCK (thid, "ESM_Delete", ESM_ERROR)
    ESM_DEBUG ((output, "Entering ESM_Delete (contextid=%d)", contextid))
    ClearError (thid);
    LOOKUP_CONTEXT (contextid, ci, thid, "ESM_Delete", ESM_ERROR);

    if (contextid > 0) {
#ifdef WIN32
        SetError (thid, WINDOWS_NOSM);
        RETURN (ESM_ERROR)
#else
        RETURN (ESM_Shared_Delete (thid, ci, contextid))
#endif
        }
    else
        RETURN (ESM_Local_Delete (thid, ci, contextid))

    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_Delete (contextid=%d)", return_value, contextid))
        UNLOCK (ESM_ERROR)
        return return_value;
        }
    }


void *
ESM_Alloc (
    int     contextid,
    char *  name,
    size_t  size
    ) {

    void * return_value = NULL;
    int thid = -999;
    int ci;             // context table index

    GETTHREADLOCK (thid, "ESM_Alloc", NULL)
    ESM_DEBUG ((output, "Entering ESM_Alloc (contextid=%d, name=\"%s\", size=%d)", contextid, name, size))
    ClearError (thid);
    LOOKUP_CONTEXT (contextid, ci, thid, "ESM_Alloc", NULL);

    if (strlen (name) > ESM_MAX_NAME-1) {
        SetError (thid, "Region name too long (%d > %d) in ESM_Alloc", strlen (name), ESM_MAX_NAME-1);
        RETURN (NULL)
        }

    if (contextid > 0) {
#ifdef WIN32
        SetError (thid, WINDOWS_NOSM);
        RETURN (NULL)
#else
        RETURN (ESM_Shared_Alloc (thid, ci, contextid, name, size))
#endif
        }
    else
        RETURN (ESM_Local_Alloc (thid, ci, contextid, name, size))

    return_label: {
        ESM_DEBUG ((output, "Returning %ld from ESM_Alloc (contextid=%d, name=\"%s\", size=%d)", (long) return_value, contextid, name, size))
        UNLOCK (NULL)
        return return_value;
        }
    }


int
ESM_Free (
    int     contextid,
    char *  name
    ) {

    int return_value = -999;
    int thid = -999;
    int ci;             // context table index

    GETTHREADLOCK (thid, "ESM_Free", ESM_ERROR)
    ESM_DEBUG ((output, "Entering ESM_Free (contextid=%d, name=\"%s\")", contextid, name))
    ClearError (thid);
    LOOKUP_CONTEXT (contextid, ci, thid, "ESM_Free", ESM_ERROR);

    if (strlen (name) > ESM_MAX_NAME-1) {
        SetError (thid, "Region name too long (%d > %d) in ESM_Free", strlen (name), ESM_MAX_NAME-1);
        RETURN (ESM_ERROR)
        }

    if (contextid > 0) {
#ifdef WIN32
        SetError (thid, WINDOWS_NOSM);
        RETURN (ESM_ERROR)
#else
        RETURN (ESM_Shared_Free (thid, ci, contextid, name))
#endif
        }
    else
        RETURN (ESM_Local_Free (thid, ci, contextid, name))

    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_Free (contextid=%d, name=\"%s\")", return_value, contextid, name))
        UNLOCK (ESM_ERROR)
        return return_value;
        }
    }


int
ESM_ListContexts (
    FILE *  output
    ) {

    int return_value = -999;
    int thid = -999;
    char * version = (char *) malloc(256*sizeof(char));

    GETTHREADLOCK (thid, "ESM_ListContexts", ESM_ERROR)
    ESM_DEBUG ((output, "Entering ESM_ListContexts (FILE)"))
    ClearError (thid);

    fprintf (output, "--------------------------------------------------------------------------------\n");
    getFullVersionString_ESMFSM(version);
    fprintf (output, "%s\n", &version[14]);
    fprintf (output, "Context List for Process %d\n\n", getpid ());
    fprintf (output, "%-4s%12s%-6s\n", "Slot", "ContextID", "Mode");

    {for (int ci = 0 ; ci < NumContexts ; ci++) {
        int contextid = Contexts[ci].contextid;
        if (contextid != 0) {
            fprintf (output, "%-4d%12d%-6s\n", ci, contextid, (contextid > 0) ? "shared" : "local");
            }
        }}

    fprintf (output, "--------------------------------------------------------------------------------\n");
    free(version);

    RETURN (ESM_OK)
    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_ListContexts (FILE)", return_value))
        UNLOCK (ESM_ERROR)
        return return_value;
        }
    }


int
ESM_ListRegions (
    int     contextid,
    FILE *  output
    ) {

    int return_value = -999;
    int thid = -999;
    int ci;             // context table index

    GETTHREADLOCK (thid, "ESM_ListRegions", ESM_ERROR)
    ESM_DEBUG ((output, "Entering ESM_ListRegions (contextid=%d, FILE)", contextid))
    ClearError (thid);
    LOOKUP_CONTEXT (contextid, ci, thid, "ESM_ListRegions", ESM_ERROR);

    if (contextid > 0) {
#ifdef WIN32
        SetError (thid, WINDOWS_NOSM);
        RETURN (ESM_ERROR)
#else
        RETURN (ESM_Shared_ListRegions (thid, ci, output))
#endif
        }
    else
        RETURN (ESM_Local_ListRegions (thid, ci, contextid, output))


    RETURN (ESM_OK)
    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_ListRegions (contextid=%d, FILE)", return_value, contextid))
        UNLOCK (ESM_ERROR)
        return return_value;
        }
    }


char *
ESM_Error (
    void
    ) {

    char *  return_value = "ESM Error";
    int *   thidp;

    if ((thidp = (int *) pthread_getspecific (Self)) == NULL)
        return "Cannot get thread ID in ESM_Error";

    return Threads[*thidp].error;
    }


int
ESM_TraceFile (
    char *  filename
    ) {

    int return_value = -999;
    int thid = -999;

    GETTHREADLOCK (thid, "ESM_TraceFile", ESM_ERROR)
    ESM_DEBUG ((output, "Entering ESM_TraceFile (filename=\"%s\")", filename))
    ClearError (thid);

    FILE * tracefile;
    if ((tracefile = fopen (filename, "a")) == NULL) {
        SetError (thid, "Cannot create/append to file \"%s\" in ESM_TraceFile", filename);
        RETURN (ESM_ERROR)
        }

    Threads[thid].tracefile = tracefile;
    RETURN (ESM_OK)

    return_label: {
        ESM_DEBUG ((output, "Returning %d from ESM_TraceFile (filename=\"%s\")", return_value, filename))
        UNLOCK (ESM_ERROR)
        return return_value;
        }
    }

int
ESM_Finish(
    void
    ) {
        // This does not work on Linx:
        // ThreadInit.done = 0;
        return 0;
}

