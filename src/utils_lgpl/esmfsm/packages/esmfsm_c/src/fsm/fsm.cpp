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
//  Core functions
//
//  The FSM functions provided in this C file are:
//  FSMINI
//  FSMEND
//  PRTKEY
//  FSMERO
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
//------------------------------------------------------------------------------


#define FSM_MAIN
#include "globals-fsm.h"

#if (defined(WIN32))
static int getpid() { return 0; }
#endif

static pthread_once_t   ThreadInit = PTHREAD_ONCE_INIT;

extern "C"
{
extern void getFullVersionString_ESMFSM( char * );
}

//------------------------------------------------------------------------------
//  Macro functions


#define LOCK(E) { \
    if (pthread_mutex_lock (&FSM.Mutex) != 0) { \
        SetError (thid, "Cannot lock mutex"); \
        RETURN (E) \
        } \
    }

#define UNLOCK(E) { \
    if (pthread_mutex_unlock (&FSM.Mutex) != 0) { \
        SetError (thid, "Cannot unlock mutex"); \
        RETURN (E) \
        } \
    }

#define GETTHREADLOCK(I,F,E) { \
    LOCK (E) \
    if (FSM.NumThreads <= 0) { \
        SetError (I, "FSM_Init must be called before %s", F); \
        UNLOCK (E) \
        RETURN (E) \
        } \
    int * idp; \
    if ((idp = (int *) pthread_getspecific (FSM.Self)) == NULL) { \
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
        for (I = 0 ; I < FSM.NumContexts ; I++) { \
            if (Contexts[I].contextid == (C)) \
            break; \
            } \
        if (I == FSM.NumContexts) { \
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


//------------------------------------------------------------------------
//  Internal function declarations


static void
InitProcess (
    void
    );

static void
SetError (
    int     thid,
    char *  message,
    ...
    );

static void
ClearError (
    int     thid
    );

static pointer
InsertKey (
    KeyTable * ktab,
    char *  name,
    int     length,
    int     type,
    int     eltsize,
    pointer ptr
    );

static int
LookupKey (
    KeyTable * ktab,
    char *  name
    );

static int
Hash (
    char *  name
    );

static void
CstrToFortran (
    char *  cstr,
    int     len,
    char *  fstr
    );


//--------------------------------------------------------------------------
//  Functions in FSM API


int STDCALL
FSM_Init (
    int *   contextid,
    int *   flags
    ) {

    int cid = *contextid;
    int return_value = -999;
    int thid = -999;
    int * thidp = new int;
    int * testp;
    KeyTable * ktab = NULL;

    FSM.Flags = *flags;
    if ((FSM.Flags & FSM_TRACE) && (FSM.Flags & FSM_SILENT))        // let trace prevail
        FSM.Flags &= ~FSM_SILENT;

    FSM_DEBUG ((output, "Entering FSM_Init (contextid=%d, flags=0x%x)", (*contextid), FSM.Flags))

    pthread_once (&ThreadInit, InitProcess);
    if (FSM.InitFails) {
        FSM.InitFails = 0;
        RETURN (FSM_ERROR)
        }

    //  Determine and set the thread ID

    LOCK (FSM_ERROR)
    thid = *thidp = FSM.NumThreads++;

    if (pthread_setspecific (FSM.Self, (void *) thidp) != 0) {
        SetError (thid, "FSM_Init cannot set thread ID");
        UNLOCK (FSM_ERROR)
        RETURN (FSM_ERROR)
        }

    if ((testp = (int *) pthread_getspecific (FSM.Self)) == NULL) {
        SetError (thid, "FSM_Init cannot get thread ID");
        UNLOCK (FSM_ERROR)
        RETURN (FSM_ERROR)
        }

    if (thid != *testp) {
        SetError (thid, "FSM_Init thread ID doesn't match self");
        UNLOCK (FSM_ERROR)
        RETURN (FSM_ERROR)
        }

    // Initialize ESM

    ESM_Init (*flags);
    FSM_CHECK_ALIGNMENT ();

    // Create new local context if specified ID is zero

    if (cid == 0)
        cid = ESM_Create (0, 0);

    // Attach a region in the context to contain the key table.
    // The first thread to call this routine will actually create the region;
    // subsequent calls will return a pointer to the existing region.

    ktab = (KeyTable *) ESM_Alloc (cid, "FSM_KeyTable", (size_t)(0));
    if (ktab == NULL) {
        ktab = (KeyTable *) ESM_Alloc (cid, "FSM_KeyTable", FSM_MAX_REGIONS * sizeof (KeyTable));
        if (ktab == NULL) {
            SetError (thid, "FSM_Init cannot allocate FSM_KeyTable: %s", ESM_Error ());
            UNLOCK (FSM_ERROR)
            RETURN (FSM_ERROR)
            }

        for (int i = 0 ; i < FSM_MAX_REGIONS ; i++) {   // Initialize key table
            ktab[i].key[0]    = '\0';
            ktab[i].length    = -1;
            ktab[i].type      = -1;
            ktab[i].eltsize   = -1;
            }
        }

    // Initialize thread-specific FSM information

    FSM.Threads[thid].flags = *flags;
    FSM.Threads[thid].contextid = cid;
    FSM.Threads[thid].keytable = ktab;
    FSM.Threads[thid].error[0] = '\0';

    UNLOCK (FSM_ERROR)
    RETURN (FSM_OK)

    return_label: {
        FSM_DEBUG ((output, "Returning %d from FSM_Init (flags=0x%x)", return_value, FSM.Flags))
        return return_value;
        }
    }


//------------------------------------------------------------------------------




pointer STDCALL
FSM_MakePointer (
#if defined (WIN32)
    char    *name,      // key name
    int     *length,        // number of elements
    int     *type,      // element type code
    int     *eltsize        // size in bytes of single element
#else
    char    *name,      // key name
    int     *length,        // number of elements
    int     *type,      // element type code
    int     *eltsize        // size in bytes of single element
#endif
    ) {

    size_t len = 0;
    int kindex;
    KeyTable * key = NULL;
    pointer ptr = (pointer) NULL;
    pointer return_value = NULL;
    int thid = -999;
    size_t nbytes;

    GETTHREADLOCK (thid, "FSM_MakePointer", NULL)
    FSM_DEBUG ((output, "Entering FSM_MakePointer (name=\"%s\", length=%d, type=%d, eltsize=%d)", name, *length, *type, *eltsize))
    ClearError (thid);

    // Validate input parameters

    len = strlen (name);
    if (len == 0) {
        SetError (thid, "Key name empty in FSM_MakePointer");
        UNLOCK (NULL)
        RETURN (NULL)
        }

    if (len > ESM_MAX_NAME-1) {
        SetError (thid, "Key name too long (%d > %d) in FSM_MakePointer", strlen (name), ESM_MAX_NAME-1);
        UNLOCK (NULL)
        RETURN (NULL)
        }

    switch (*type) {
        case FSM_INTEGER:
        case FSM_REAL:
        case FSM_DOUBLE_PREC:
        case FSM_DOUBLE_COMP:
        case FSM_COMPLEX:
        case FSM_LOGICAL:
        case FSM_CHARACTER:
            break;
        default:
            SetError (thid, "Invalid key type (%d) in FSM_MakePointer", *type);
            UNLOCK (NULL)
            RETURN (NULL)
        }

    // If the key already exists, return its pointer provided the existing and specified
    // type and length are the same.  If they're different, it's an error.

    if ((kindex = LookupKey (FSM.Threads[thid].keytable, name)) >= 0) {
        key = &FSM.Threads[thid].keytable[kindex];

        if (*length != key->length) {
            SetError (thid, "Trying to resize (%d != %d) existing key \"%s\" in FSM_MakePointer", *length, key->length, name);
            UNLOCK (NULL)
            RETURN (NULL)
            }

        if (*type != key->type) {
            SetError (thid, "Incompatible type (%d != %d) existing key \"%s\" in FSM_MakePointer", *type, key->type, name);
            UNLOCK (NULL)
            RETURN (NULL)
            }

        UNLOCK (NULL)
        RETURN (FSM.Pointer [kindex]);
        }

    // The key does not exist yet; create it.
	nbytes = (size_t)(*length) * (size_t)(*eltsize);
    ptr = (pointer) ESM_Alloc (FSM.Threads[thid].contextid, name, nbytes);
    if (ptr == NULL) {
        SetError (thid, "FSM_MakePointer cannot allocate key: %s", ESM_Error ());
        UNLOCK (NULL)
        RETURN (NULL)
        }

    //  Note: Memory allocated for Fortran must be aligned properly.
    //  Since FSM uses a base address and an offset, proper alignment
    //  is guaranteed if both base and offset are multiples of the
    //  basic alignment unit.
    //  The Fortran compiler should ensure this for the base addresses, but
    //  this is checked once at the start of the program just to be safe.
    //  The alignment varies by type from 4 to 16 bytes (on the architectures
    //  that FSM has been ported to!).
    //  Since ESM_Alloc is programmed to return pointers aligned on the
    //  maximum of this value, the final pointer will be valid for Fortran.
    //  End note.

    // Zero the allocated area.  This is done for two reasons:
    //  - the user of the area may assume it's zeroed, so make sure it is
    //  - this tests the actual usability of the entire area (no SEGV's)

    (void) memset ((void *) ptr, 0, (*length * *eltsize));

    // Update the key and pointer tables

    ptr = InsertKey (FSM.Threads[thid].keytable, name, *length, *type, *eltsize, ptr);
    if (ptr == NULL) {
        SetError (thid, "FSM_MakePointer cannot allocate key: Key Table full (%d keys)", FSM_MAX_REGIONS);
        }

    UNLOCK (NULL);
    RETURN (ptr);

    return_label: {
        FSM_DEBUG ((output, "Returning %ld from FSM_MakePointer (name=\"%s\", length=%d, type=%d, eltsize=%d)", (long) return_value, name, *length, *type, *eltsize))
        return return_value;
        }
    }


//------------------------------------------------------------------------------


pointer STDCALL
FSM_GetPointer (
#if defined (WIN32)
    char    *name,
    int     *type
#else
    char    *name,
    int     *type
#endif
    ) {

    size_t len = 0;
    int kindex;
    pointer return_value = NULL;
    int thid = -999;

    GETTHREADLOCK (thid, "FSM_GetPointer", NULL)
    FSM_DEBUG ((output, "Entering FSM_GetPointer (name=\"%s\", *type)", name))
    ClearError (thid);

    // Validate input parameters

    len = strlen (name);
    if (len == 0) {
        SetError (thid, "Key name empty in FSM_GetPointer");
        UNLOCK (NULL)
        RETURN (NULL)
        }

    if (len > ESM_MAX_NAME-1) {
        SetError (thid, "Key name too long (%d > %d) in FSM_GetPointer", strlen (name), ESM_MAX_NAME-1);
        UNLOCK (NULL)
        RETURN (NULL)
        }

    // If the key exists, return its type and pointer (possibly after asking ESM for pointer)


    if ((kindex = LookupKey (FSM.Threads[thid].keytable, name)) >= 0) {
        *type = FSM.Threads[thid].keytable[kindex].type;
        if (FSM.Pointer[kindex] == NULL) {
            FSM.Pointer[kindex] = (pointer) ESM_Alloc (FSM.Threads[thid].contextid, name, (size_t)(0));
            if (FSM.Pointer[kindex] == NULL) {
                SetError (thid, "ESM_Alloc does not know \"%s\" in FSM_GetPointer (%s)", name, ESM_Error ());
                }
            }

        UNLOCK (NULL)
        RETURN (FSM.Pointer[kindex])
        }

    // The key is not found

    SetError (thid, "Key \"%s\" not found in FSM_GetPointer", name);
    UNLOCK (NULL)
    RETURN (NULL)

    return_label: {
        FSM_DEBUG ((output, "Returning %ld from FSM_GetPointer (name=\"%s\" type=%d)", (long) return_value, name, *type))
        return return_value;
        }
    }


//------------------------------------------------------------------------------


pointer STDCALL
FSM_ReleasePointer (
#if ! defined (WIN32)
    char    *name,
    int     *type
#else
    char    *name,
    int     *type
#endif
    ) {

    size_t len = 0;
    int kindex;
    pointer ptr = NULL;
    pointer return_value = NULL;
    int thid = -999;

    GETTHREADLOCK (thid, "FSM_ReleasePointer", NULL)
    FSM_DEBUG ((output, "Entering FSM_ReleasePointer (name=\"%s\")", name))
    ClearError (thid);

    // Validate input parameters

    len = strlen (name);
    if (len == 0) {
        SetError (thid, "Key name empty in FSM_ReleasePointer");
        UNLOCK (NULL)
        RETURN (NULL)
        }

    if (len > ESM_MAX_NAME-1) {
        SetError (thid, "Key name too long (%d > %d) in FSM_ReleasePointer", strlen (name), ESM_MAX_NAME-1);
        UNLOCK (NULL)
        RETURN (NULL)
        }

    // Look the name up in the key table or return an error if not found

    if ((kindex = LookupKey (FSM.Threads[thid].keytable, name)) < 0) {
        SetError (thid, "Key \"%s\" unknown in FSM_ReleasePointer", name);
        UNLOCK (NULL)
        RETURN (NULL)
        }

    *type = FSM.Threads[thid].keytable[kindex].type;

    // Invalidate key and pointer table entires

    FSM.Threads[thid].keytable[kindex].key[0]  = (char) -1;
    FSM.Threads[thid].keytable[kindex].length  = -1;
    FSM.Threads[thid].keytable[kindex].type    = -1;
    FSM.Threads[thid].keytable[kindex].eltsize = -1;

    ptr = FSM.Pointer[kindex];
    FSM.Pointer[kindex] = NULL;

    // Release storage

    if (ESM_Free (FSM.Threads[thid].contextid, name) != ESM_OK) {
        SetError (thid, "FSM_ReleasePointer get error from ESM_Free: %s", ESM_Error ());
        UNLOCK (NULL)
        RETURN (NULL)
            }

    UNLOCK (NULL)
    RETURN (ptr);

    return_label: {
        FSM_DEBUG ((output, "Returning %ld from FSM_ReleasePointer (name=\"%s\")", (long) return_value, name))
        return return_value;
        }
    }


//------------------------------------------------------------------------------


int STDCALL
FSM_PrintKeys (
    void
    ) {

    int return_value = -999;
    int thid = -999;
    char    fmt[100];

    int contextid = 0;
    KeyTable * ktab = NULL;
    int     numkeys = 0;
    int     totalsize = 0;
    char * version = (char *) malloc(256*sizeof(char));

    GETTHREADLOCK (thid, "FSM_PrintKeys", 0)
    FSM_DEBUG ((output, "Entering FSM_PrintKeys ()"))
    ClearError (thid);

    contextid = FSM.Threads[thid].contextid;
    printf ("--------------------------------------------------------------------------------\n");
    getFullVersionString_ESMFSM(version);
    printf ("%s\n", &version[5]);
    printf ("Key/Pointer List for %s Context %d, Process %d\n\n", (contextid > 0) ? "Shared" : "Local", contextid, getpid ());
    sprintf (fmt, "%%-%ds %%-12s %%7s %%12s\n", ESM_MAX_NAME);
    printf (fmt, "Key Name", "Type", "Length", "   Address (decimal & hex)");
    sprintf (fmt, "%%-%ds %%-12s %%7d %%12d 0x%%lx\n", ESM_MAX_NAME);

    ktab = FSM.Threads[thid].keytable;
    {for (int i = 0 ; i < FSM_MAX_REGIONS ; i++) {
        if (ktab[i].key[0] != 0 && ktab[i].length > 0) {
            numkeys++;
            totalsize += ktab[i].length * fsm_type_size [ktab[i].type] + ESM_ALIGNMENT;

            printf (fmt,
                        ktab[i].key,
                        fsm_type_name [ktab[i].type],
                        ktab[i].length,
                        (pointer) FSM.Pointer[i],
                        (pointer) FSM.Pointer[i]
                        );
            }
        }}

    printf ("\nNumber of keys in context: %d\n", numkeys);
    printf ("Total storage used by these keys: %d KB\n", totalsize / 1024);
    printf ("--------------------------------------------------------------------------------\n");
    fflush (stdout);
    free(version);

    UNLOCK (0)
    RETURN (0)

    return_label: {
        FSM_DEBUG ((output, "Returning from FSM_PrintKeys ()"))
        return return_value;;
        }
    }


//------------------------------------------------------------------------------


int STDCALL
FSM_Err (
#if ! defined (WIN32)
    char *  message,
    int *   length
#else
    char *  message,
    int *   length
#endif
    ) {

    int return_value = -999;
    int thid = -999;

    GETTHREADLOCK (thid, "FSM_Error", 0)

    CstrToFortran (FSM.Threads[thid].error, *length, message);
    UNLOCK (0)
    RETURN (0)

    return_label: {
        return return_value;;
        }
    }


//------------------------------------------------------------------------------


int STDCALL
FSM_TraceFile (
#if ! defined (WIN32)
    char *  filename
#else
    char *  filename
#endif
    ) {

    int return_value = -999;
    int thid = -999;

    GETTHREADLOCK (thid, "FSM_TraceFile", FSM_ERROR)
    FSM_DEBUG ((output, "Entering FSM_TraceFile (filename=\"%s\")", filename))
    ClearError (thid);

    FILE * tracefile;
    if ((tracefile = fopen (filename, "a")) == NULL) {
        SetError (thid, "Cannot create/append to file \"%s\" in FSM_TraceFile", filename);
        UNLOCK (FSM_ERROR)
        RETURN (FSM_ERROR)
        }

    FSM.Threads[thid].tracefile = tracefile;

    if (ESM_TraceFile (filename) != ESM_OK) {
        SetError (thid, "ESM_TraceFile fails in FSM_TraceFile: %s", ESM_Error ());
        UNLOCK (FSM_ERROR)
        RETURN (FSM_ERROR)
        }

    UNLOCK (FSM_ERROR)
    RETURN (FSM_OK)

    return_label: {
        FSM_DEBUG ((output, "Returning %d from FSM_TraceFile (filename=\"%s\")", return_value, filename))
        return return_value;;
        }
    }


//------------------------------------------------------------------------------
//  Internal functions


static void
InitProcess (
    void
    ) {

    // This routine is called once to initialze FSM for this process

    FSM.InitFails   = 0;
    FSM.NumThreads  = 0;
    FSM.NumContexts = 0;
    FSM.Self        = (pthread_key_t) NULL;

    // Create mutual exclusion object for controlling access to global data

    if (pthread_mutex_init (&FSM.Mutex, NULL) != 0) {
    SetError (-1, "FSM_Init cannot create thread mutex");
        FSM.InitFails = 1;
        return;
        }

    // Create thread-specific pointer to thread ID

    if (pthread_key_create (&FSM.Self, NULL) != 0) {
    SetError (-1, "FSM_Init cannot create thread-specific key");
        FSM.InitFails = 1;
        return;
        }

    // Initialize key pointer table

    for (int i = 0 ; i < FSM_MAX_REGIONS ; i++)
        FSM.Pointer[i] = NULL;


    // Initialize thread table

    {for (int i = 0 ; i < ESM_MAX_THREADS ; i++) {
        FSM.Threads[i].flags     = 0;
        FSM.Threads[i].contextid = 0;
        FSM.Threads[i].keytable  = NULL;
        FSM.Threads[i].tracefile = NULL;
        FSM.Threads[i].error[0]  = '\0';
        }}
    }


void
SetError (
    int     thid,
    char *  message,
    ...
    ) {

    va_list     arguments;
    char * buf = (thid < 0) ? FSM.Error : FSM.Threads[thid].error;

    va_start (arguments, message);
    vsprintf (buf, message, arguments);
    va_end (arguments);

    int flags = (thid < 0) ? FSM.Flags : FSM.Threads[thid].flags;
    if (~flags & ESM_SILENT) {
        printf ("%s\n", buf);
        fflush (stdout);
        }
    }


static void
ClearError (
    int     thid
    ) {

    char * buf = (thid < 0) ? FSM.Error : FSM.Threads[thid].error;
    sprintf (buf, "");
    }



static pointer
InsertKey (
    KeyTable * ktab,
    char    *name,
    int     length,
    int     type,
    int     eltsize,
    pointer ptr
    ) {

    int     i, n;

    i = Hash (name);
    for (n = 0 ; n < FSM_MAX_REGIONS ; n++) {
        if (ktab[i].key[0] == 0 || ktab[i].key[0] == (char) -1) {        // free slot
            // free slot
            strcpy (ktab[i].key, name);
            break;
            }

        else if (strcmp (ktab[i].key, name) == 0) {
            if (ktab[i].length == -1)   // free slot, key used previously
            break;
            else                        // key already in table
            return ptr;
            }

        i = (i+1) % FSM_MAX_REGIONS;
        }

    if (n == FSM_MAX_REGIONS)
        return NULL;

    ktab[i].length  = length;
    ktab[i].type    = type;
    ktab[i].eltsize = eltsize;
    FSM.Pointer[i]  = ptr;

    return ptr;
    }


static int
LookupKey (
    KeyTable * ktab,
    char *  name
    ) {

    int     i, n;

    i = Hash (name);
    for (n = 0 ; n < FSM_MAX_REGIONS ; n++) {
        if (ktab[i].key[0] == 0)
            return -1;      // free slot => key not in table

        else if (strcmp (ktab[i].key, name) == 0)
            return i;       // key found

        i = (i+1) % FSM_MAX_REGIONS;
        }

    return -1;          // table full and key not found
    }


static int
Hash (
    char *  name
    ) {

    int     i;
    int     hc = 0; // hash code

    for (i = 0 ; i < (int) strlen (name) ; i++) {
        hc = (hc << 3) + (int) name[i];
        if (i % 3 == 0)
            hc %= FSM_MAX_REGIONS;
        }

    return hc % FSM_MAX_REGIONS;
    }


static void
CstrToFortran (
    char *  cstr,
    int     len,
    char *  fstr
    ) {

    //  Function to convert a null-terminated C string to a
    //  space-padded Fortran string.

    int     i;

    //  Copy up to but not including the C string terminator

    for (i = 0 ; i < len && cstr[i] != '\0' ; i++)
        fstr[i] = cstr[i];

    //  Pad the Fortran character array with spaces

    for ( ; i <= len ; i++)
        fstr[i] = ' ';
    }
