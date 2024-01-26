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
//  Local memory implemention (using malloc and free)
//
//  Irv.Elshoff@deltares.nl
//  13 oct 05
//
//------------------------------------------------------------------------------


#include "globals-esm.h"


#if (defined(WIN32))
static int getpid() { return 0; }
#endif

extern "C"
{
extern void getFullVersionString_ESMFSM( char * );
}

//------------------------------------------------------------------------------
//  Region and context descriptor type definitions


typedef struct RD_st {
    char    name [ESM_MAX_NAME];    // region name
    void *  addr;           // allocated address
    void *  ptr;        // pointer for users of region
    int     size;       // actual size in bytes

    struct RD_st * next;    // linked-list pointer
    struct RD_st * prev;    // linked-list pointer
    }
    RD;

typedef struct CD_st {
    int     contextid;      // context id
    RD  *   rdlist;         // list of region descriptors
    }
    CD;


//------------------------------------------------------------------------------



int
ESM_Local_Create (
    int thid
    ) {

    // Allocate a slot in the context table and generate negative context ID

    if (NumContexts >= ESM_MAX_CONTEXTS) {
        SetError (thid, "Too many contexts (%d) for process; ESM_Create fails", ESM_MAX_CONTEXTS);
        return 0;
        }

    int ci = NumContexts++;
    int contextid = - (ci + ESM_LOCALID_OFFSET);

    Contexts[ci].contextid = contextid;

    // Allocate and initialize context descriptor

    if ((Contexts[ci].cd = malloc (sizeof (CD))) == NULL) {
        SetError (thid, "ESM_Create cannot allocate a context descriptor");
        return 0;
        }

    CD * cd = (CD *) Contexts[ci].cd;
    cd->contextid = contextid;
    cd->rdlist = NULL;

    return contextid;
    }


int
ESM_Local_Delete (
    int thid,
    int ci,
    int contextid
    ) {

    if (ci == -1) {
        SetError (thid, "Unknown context ID %d in ESM_Delete", contextid);
        return ESM_ERROR;
        }

    // Free all regions and the context descriptor

    CD * cd = (CD *) Contexts[ci].cd;

    RD * rd;
    RD * rdnext;

    for (rd = cd->rdlist ; rd != NULL ; rd = rdnext) {
        rdnext = rd->next;
        free (rd->addr);
        free (rd);
        }

    free (cd);

    Contexts[ci].contextid = 0;
    Contexts[ci].cd = NULL;

    return ESM_OK;
    }


#include <stdlib.h>
void *
ESM_Local_Alloc (
    int thid,
    int ci,
    int contextid,
    char * name,
    size_t size
    ) {

    if (ci == -1) {
        SetError (thid, "Unknown context ID %d in ESM_Alloc", contextid);
        return NULL;
        }

    // Check if the region already exists in the context.
    // If so, return its local address

    CD * cd = (CD *) Contexts[ci].cd;
    RD * rd;

    for (rd = cd->rdlist ; rd != NULL ; rd = rd->next)
        if (strcmp (name, rd->name) == 0)
            break;

    if (rd == NULL) {
        if (size <= 0) return NULL;
        }

    else {      // found region
        if (size == 0 || size == rd->size)
            return rd->ptr;
        else
            return NULL;
        }

    // The region was not found but size is positive; create new region

    if ((rd = (RD *) malloc (sizeof (RD))) == NULL) {
        SetError (thid, "Cannot allocate region descriptor in ESM_Alloc");
        return NULL;
        }

    //  Some users of the region may require it to be aligned to certain
    //  boundaries.  FORTRAN is an example.  A small amount of extra space
    //  is added to the basic allocation.  This allows us to move the actual
    //  address returned to the calling routine up a bit to meet the alignment
    //  criteria.  This increment is at most the alignment unit minus a byte.
    //  If the address is already aligned, we lose some space, but this is
    //  tolerable since the number of regions is usually not more than 100's.

    void * addr;
    size_t nwsize = size + ESM_ALIGNMENT;
    if ((addr = (void *) malloc (nwsize)) == NULL) {
        SetError (thid, "Cannot allocate region (%d+%d bytes) in ESM_Alloc", size, ESM_ALIGNMENT);
        return NULL;
        }

    //  Adjust the pointer to a multiple of the alignment unit if necessary

    void * ptr = addr;
    if (((long long) ptr) % ESM_ALIGNMENT != 0)
        ptr = (void *) ((long long) ptr + ESM_ALIGNMENT - (((long long) ptr) % ESM_ALIGNMENT));

    //  Add then new region to front of the context's region list

    strcpy (rd->name, name);
    rd->addr = addr;
    rd->ptr  = ptr;
    rd->size = size;
    rd->next = cd->rdlist;
    if (rd->next != NULL)  rd->next->prev = rd;
    rd->prev = NULL;
    cd->rdlist = rd;

    memset (ptr, 0, size);
    return ptr;
    }


int
ESM_Local_Free (
    int thid,
    int ci,
    int contextid,
    char * name
    ) {

    if (ci == -1) {
        SetError (thid, "Unknown context ID %d in ESM_Free", contextid);
        return ESM_ERROR;
        }

    //  Look for the specified region.  If it is found, free storage and remove it
    //  from the context's region list.  Otherwise return an error.

    CD * cd = (CD *) Contexts[ci].cd;
    RD * rd;

    for (rd = cd->rdlist ; rd != NULL ; rd = rd->next) {
        if (strcmp (name, rd->name) == 0) {
            free (rd->addr);

            if (rd == cd->rdlist)
                cd->rdlist = rd->next;
            else if (rd->prev != NULL)
                rd->prev->next = rd->next;
            else {
                SetError (thid, "Internal linked list inconsistency in ESM_Free for region \"%s\"", name);
                return ESM_ERROR;
                }

            if (rd->next != NULL)
            rd->next->prev = rd->prev;

            free (rd);
            return ESM_OK;
            }
        }

    SetError (thid, "Cannot find region \"%s\" in ESM_Free", name);
    return ESM_ERROR;
    }


int
ESM_Local_ListRegions (
    int thid,
    int ci,
    int contextid,
    FILE * output
    ) {
    char * version = (char *) malloc(256*sizeof(char));

    if (ci == -1) {
        SetError (thid, "Unknown context ID %d in ESM_ListRegions", contextid);
        return ESM_ERROR;
        }

    fprintf (output, "--------------------------------------------------------------------------------\n");
    getFullVersionString_ESMFSM(version);
    fprintf (output, "%s\n", &version[14]);
    fprintf (output, "Region List for %s Context %d, Process %d\n\n", (contextid > 0) ? "Shared" : "Local", contextid, getpid ());
    fprintf (output, "%-12s%-12s%s\n", "Pointer", "Size", "Name");

    CD * cd = (CD *) Contexts[ci].cd;
    RD * rd;

    for (rd = cd->rdlist ; rd != NULL ; rd = rd->next) {
        fprintf (output, "%-12p%-12d%s\n", rd->ptr, rd->size, rd->name);
        }

    fprintf (output, "--------------------------------------------------------------------------------\n");
    free(version);

    return ESM_OK;
    }


