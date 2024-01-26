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
//  Shared memory implemention for UNIX
//
//  Irv.Elshoff@Deltares.NL
//  13 oct 05
//  22 sep 10
//
//------------------------------------------------------------------------------


#include "globals-esm.h"
#if defined (HAVE_CONFIG_H)
#include "config.h"
#endif

#if defined (HAVE_CONFIG_H)
#include <errno.h>
#include <sys/ipc.h>
#include <sys/shm.h>



//------------------------------------------------------------------------------
//  Region and context descriptor type definitions
//  The context descriptor is stored in a special shared memory page
//  associated with the context.  It is protected by a semaphore,
//  and contains lists of regions and pages.
//  There are two kinds of regions: allocated regions for user data
//  and hole regions, which are empty parts of mapped pages that can
//  be used to fulfill future allocation requests.  Region lists are
//  doubly-linked in ascending order by name (data) or size (holes).
//  The page table contains the size and ID of all shared memory
//  segments associated with the context (except for the context descriptor
//  itself), and is used primarily for destroying contexts.
//  In addition to these data structures, there is a per process
//  mapping table that contains the local address of the pages in the
//  processes address space.  This table is part of the "Contexts" array.
//  The index in this array is provided by the top-level ESM API routines.
//  The index in the page table is the same as for the page table in the
//  context descriptor.


#define GND (-1)        // null index

typedef struct RD_st {
    char    name [ESM_MAX_NAME];    // region name
    int     offset;     // pointer to region relative to beginning of page
    int     pagenum;        // page number [0..ESM_MAX_PAGES]
    int     size;       // actual size in bytes
    int     next;           // linked-list pointer
    int     prev;           // linked-list pointer
    }
    RD;

typedef struct PD_st {
    int     shmid;      // UNIX shared memory ID
    int     size;       // actual size in bytes (1024 * cd->pagesize or larger)
    }
    PD;

typedef struct CD_st {
    int     magic;          // for utility to list all ESM contexts on a given host
    int     contextid;      // context id
    int     mutex;          // mutual exclusion semaphore for this context

    // Region descriptor list
    int     nregions;       // number of allocated regions in context
    RD      region [ESM_MAX_REGIONS];
    int     rdlist;         // head of allocated region descriptors (ordered by name)
    int     rdempty;        // head of empty regions (ordered by size)
    int     rdfree;         // head of right-sided tree (list

    // Page list
    int     npages;         // number of pages in context
    PD      page [ESM_MAX_PAGES];  // page descriptor table
    int     pagesize;       // shared memory page size in 1K blocks
    }
    CD;


//------------------------------------------------------------------------------
//  Semaphore operation macros.


static struct sembuf Psem = { 0, -1, 0 };
static struct sembuf Vsem = { 0,  1, 0 };

#define P(S,E)  { \
    if (semop ((S), &Psem, 1) != 0) { \
        SetError (thid, "Semaphore P-operation on %d fails: %s", (S), strerror (errno)); \
        return (E); \
        } \
    }

#define V(S,E)  { \
    if (semop ((S), &Vsem, 1) != 0) { \
        SetError (thid, "Semaphore V-operation on %d fails: %s", (S), strerror (errno)); \
        return (E); \
        } \
    }


//------------------------------------------------------------------------------
//  Macro functions


//  Detach a mapped shared memory segment

#define DETACH(I,E)  { \
    if (shmdt ((I))) { \
        SetError (thid, "Cannot detach shared memory segment: %s", strerror (errno)); \
        return (E); \
        } \
    }

//  Destroy a shared memory segment

#define DESTROY(I,E)  { \
    struct shmid_ds buf; \
    if (shmctl ((I), IPC_RMID, &buf) != 0) { \
        SetError (thid, "Cannot destroy shared memory segment: %s", strerror (errno)); \
        return (E); \
        } \
    }

//  Convert a relative address within a page (A) to an absolute address
//  given a page base address (B) in the local address space

#define LOCALADDRESS(B,A) ( \
    (void *)((char *)(B) + (long)(A)) \
    )


//------------------------------------------------------------------------------
//  Internal function declarations


static int
AttachContext (
    int     thid,
    int     contextid
    );

static void
PrintContextSegmentInfo (
    struct shmid_ds * segment,
    CD *    cd,
    FILE *  output
    );

static int
GetRegionDescriptor (
    RD      regions[],
    int *   freehead
    );

static void
PutRegionDescriptor (
    RD      regions[],
    int *   freehead,
    int     index
    );

static int
SearchForRegion (
    RD      regions[],
    int *   head,
    char *  name
    );

static int
SearchForHole (
    RD      regions[],
    int *   head,
    int     size
    );

static void
InsertRegion (
    RD      regions[],
    int *   listhead,
    int     newreg
    );

static void
InsertHole (
    RD      regions[],
    int *   listhead,
    int     newreg
    );

static void
UnlinkElement (
    RD      regions[],
    int *   head,
    int     index
    );


//------------------------------------------------------------------------------


int
ESM_Shared_Create (
    int thid,
    int pagesize
    ) {

    // Allocate a slot in the per-process context table

    if (NumContexts >= ESM_MAX_CONTEXTS) {
        SetError (thid, "Too many contexts (%d) for process; ESM_Create fails", ESM_MAX_CONTEXTS);
        return 0;
        }

    int ci = NumContexts++;

    // Create a new context descriptor in shared memory for all processes

    int contextid = shmget (IPC_PRIVATE, sizeof (CD), IPC_CREAT | SHM_R | SHM_W);
    if (contextid < 0) {
        SetError (thid, "Cannot create context descriptor segment in ESM_Create: %s", strerror (errno));
        return 0;
        }

    // A context ID (= shared memeory segment ID) of zero is a valid in Linux, in fact the first segment
    // after a reboot will have ID 0.  This is not a valid ESM context ID, however.  If we get a zero,
    // create an additional segment and the delete the first one.  This way ESM context IDs are guaranteed
    // to be non-zero.  (A zero ID for the semaphore below is no problem; it's private to ESM.)

    if (contextid == 0) {
        int newcontextid = shmget (IPC_PRIVATE, sizeof (CD), IPC_CREAT | SHM_R | SHM_W);
        if (newcontextid < 0) {
            SetError (thid, "Cannot create second non-zero context descriptor segment in ESM_Create: %s", strerror (errno));
            return 0;
            }

        DESTROY (contextid, ESM_ERROR);
        contextid = newcontextid;
        }

    // Attach context descriptor to local address space (temporarily)

    CD * cd = (CD *) shmat (contextid, NULL, 0);
    if ((long) cd == -1) {
        SetError (thid, "Cannot attach context descriptor segment in ESM_Create: %s", strerror (errno));
        DESTROY (contextid, 0);
        return 0;
        }

    // Create a mutual exclusion semaphore for all processes

    int semid = semget (IPC_PRIVATE, 1, IPC_CREAT | SEM_A | SEM_R);
    if (semid < 0) {
        SetError (thid, "Cannot create context descriptor mutex semaphore in ESM_Create: %s", strerror (errno));
        DESTROY (contextid, 0);
        return 0;
        }

    // Initialize context descriptor

    cd->magic       = ESM_MAGIC;
    cd->contextid   = contextid;
    cd->pagesize    = pagesize;
    cd->npages      = 0;
    cd->mutex       = semid;

    V (cd->mutex, 0);

    // Initialize region table in context descriptor

    for (int i = 0 ; i < ESM_MAX_REGIONS ; i++) {
        RD * rd = &cd->region[i];
        memset ((void *) rd, 0, sizeof (RD));
        rd->prev = i-1;
        rd->next = i+1;
        }

    cd->nregions = 0;
    cd->region[0].prev = GND;
    cd->region[ESM_MAX_REGIONS-1].next = GND;
    cd->rdfree = 0;
    cd->rdlist = GND;
    cd->rdempty = GND;

    // Initialize page table in context descriptor

    for (int pi = 0 ; pi < ESM_MAX_PAGES ; pi++) {
        PD * pd = &cd->page[pi];
        memset ((void *) pd, 0, sizeof (PD));
        Contexts[ci].pageaddr[pi] = NULL;
        }

    Contexts[ci].contextid = contextid;
    Contexts[ci].cd = cd;
    Contexts[ci].nattached = 0;

    return contextid;
    }


int
ESM_Shared_Delete (
    int thid,
    int ci,
    int contextid
    ) {

    // If the context is not attached to the current process, attach it

    if (ci < 0) {
        ci = AttachContext (thid, contextid);
        if (ci < 0)
            return ESM_ERROR;
        }

    CD * cd = (CD *) Contexts[ci].cd;
    P (cd->mutex, ESM_ERROR);

    // Destroy all data pages

    for (int pi = 0 ; pi < cd->npages ; pi++) {
        DETACH (Contexts[ci].pageaddr[pi], ESM_ERROR);
        DESTROY (cd->page[pi].shmid, ESM_ERROR);
        }

    // Destroy mutual exclusion semaphore

    if (semctl (cd->mutex, 0, IPC_RMID) != 0) {
        SetError (thid, "Cannot remove semaphore (%d) associated with context descriptor (%d): %s",
                        cd->mutex,
                contextid,
                strerror (errno)
                );
        return ESM_ERROR;
        }

    // Destroy context descriptor

    DETACH (cd, ESM_ERROR);
    DESTROY (contextid, ESM_ERROR);

    // Clear entry in local contexts table

    Contexts[ci].contextid = 0;
    Contexts[ci].cd = NULL;
    Contexts[ci].nattached = 0;

    return ESM_OK;
    }


void *
ESM_Shared_Alloc (
    int     thid,
    int     ci,
    int     contextid,
    char *  name,
    size_t  size
    ) {

    // If the context is not attached to the current process, attach it

    if (ci < 0) {
        ci = AttachContext (thid, contextid);
        if (ci < 0)
            return NULL;
        }

    // Check if the region already exists in the context.
    // If so, return its local address.

    CD * cd = (CD *) Contexts[ci].cd;
    P (cd->mutex, NULL);

    int reg = SearchForRegion (cd->region, &cd->rdlist, name);
    if (reg == GND) {
    if (size <= 0) {
            V (cd->mutex, NULL);
            return NULL;    // did not find region; no allocation, lookup only
            }
        }

    else {      // found region
        if (size == 0 || size == cd->region[reg].size) {
            void * paddr;       // base address of page in local address space
            paddr = Contexts[ci].pageaddr[cd->region[reg].pagenum];
            if (paddr == NULL) {
                V (cd->mutex, NULL);
                return NULL;
                }
            else {
                V (cd->mutex, NULL);
                return LOCALADDRESS (paddr, cd->region[reg].offset);
                }
            }
        else {
            V (cd->mutex, NULL);
            return NULL;
            }
        }

    // The region was not found.  Find the smallest hole large enough to
    // accomodate the region in one of the existing pages.  The list of holes
    // (empty regions) is sorted by size in descending order.
    // If found a suitable hole is found, split it into a piece for the region
    // and a smaller empty hole left over for the next allocation.

    int hole = SearchForHole (cd->region, &cd->rdempty, size);
    if (hole == GND) {
        // No suitable hole found.  Make one

        if (cd->npages == ESM_MAX_PAGES) {
            SetError (thid, "Page limit has been reached; try increasing page size");
            V (cd->mutex, NULL);
            return NULL;
            }

        hole = GetRegionDescriptor (cd->region, &cd->rdfree);
        if (hole == GND) {
            SetError (thid, "Region descriptors have been exhausted; cannot make hole");
            V (cd->mutex, NULL);
            return NULL;
            }

        int pi = cd->npages++;
        cd->page[pi].size = max (size, 1024 * cd->pagesize);
        cd->page[pi].shmid = shmget (IPC_PRIVATE, cd->page[pi].size, IPC_CREAT | SHM_R | SHM_W);
        if (cd->page[pi].shmid <= 0) {
            SetError (thid, "Cannot create new page in ESM_Alloc: %s", strerror (errno));
            cd->npages--;
            V (cd->mutex, NULL);
            return NULL;
            }

        void * pageaddr = shmat (cd->page[pi].shmid, NULL, 0);
        if ((long) pageaddr == -1) {
            SetError (thid, "Cannot attach new page in ESM_Alloc: %s", strerror (errno));
            DESTROY (cd->page[pi].shmid, NULL);
            cd->npages--;
            V (cd->mutex, NULL);
            return NULL;
            }

        Contexts[ci].pageaddr[pi] = pageaddr;
        Contexts[ci].nattached++;

        cd->region[hole].pagenum = pi;
        cd->region[hole].offset = 0;
        cd->region[hole].size = cd->page[pi].size;
        }

    else
        UnlinkElement (cd->region, &cd->rdempty, hole);

    // At this point we have a suitable hole.
    // Split it in two, allocate one piece to the new region and put the remaining
    // piece back on the empty list.

    int newreg = GetRegionDescriptor (cd->region, &cd->rdfree);
    if (newreg == GND) {
        SetError (thid, "Region descriptors have been exhausted");
        V (cd->mutex, NULL);
        return NULL;
        }

    strcpy (cd->region[newreg].name, name);
    cd->region[newreg].size = size;
    cd->region[newreg].pagenum = cd->region[hole].pagenum;
    cd->region[newreg].offset = cd->region[hole].offset;
    InsertRegion (cd->region, &cd->rdlist, newreg);
    cd->nregions++;

    if ((cd->region[hole].size -= size) > 0) {
        cd->region[hole].offset += size;
        InsertHole (cd->region, &cd->rdempty, hole);
        }
    else
        PutRegionDescriptor (cd->region, &cd->rdfree, hole);

    // Convert relative offset in hole to absolute address in local address space
    // and zero all bytes in region

    void * addr = LOCALADDRESS (
                    Contexts[ci].pageaddr[cd->region[newreg].pagenum],
                    cd->region[newreg].offset
                    );

    memset (addr, 0, size);

    V (cd->mutex, NULL);
    return addr;
    }


int
ESM_Shared_Free (
    int thid,
    int ci,
    int contextid,
    char * name
    ) {

    // If the context is not attached to the current process, attach it

    if (ci < 0) {
        ci = AttachContext (thid, contextid);
        if (ci < 0)
            return ESM_ERROR;
        }

    CD * cd = (CD *) Contexts[ci].cd;
    P (cd->mutex, ESM_ERROR);

    // Look for the region's descriptor

    int reg = SearchForRegion (cd->region, &cd->rdlist, name);
    if (reg == GND) {
        SetError (thid, "Cannot find region \"%s\" in ESM_Free", name);
        V (cd->mutex, ESM_ERROR);
        return ESM_ERROR;
        }

    // Found it.  Move the region descriptor to the empty (hole) list

    UnlinkElement (cd->region, &cd->rdlist, reg);
    memset ((void *) cd->region[reg].name, 0, ESM_MAX_NAME);
    InsertHole (cd->region, &cd->rdempty, reg);
    cd->nregions--;

    // Coalesce the new hole with any directly adjoining holes.
    // There can be at most two of these: one just before in new hole
    // in the same page, and one just after.

    // Look for holes directly after the new hole

    for (int r = cd->rdempty ; r != GND ; r = cd->region[r].next) {
        if (cd->region[r].pagenum != cd->region[reg].pagenum)
            continue;
        if (cd->region[reg].offset + cd->region[reg].size != cd->region[r].offset)
            continue;

        // Make the new hole larger by increasing its size and remove the old hole
        cd->region[reg].size += cd->region[r].size;
        UnlinkElement (cd->region, &cd->rdempty, r);
        UnlinkElement (cd->region, &cd->rdempty, reg);
        PutRegionDescriptor (cd->region, &cd->rdfree, r);
        InsertHole (cd->region, &cd->rdempty, reg);
        }

    // Look for holes directly before the new hole

    for (int r = cd->rdempty ; r != GND ; r = cd->region[r].next) {
        if (cd->region[r].pagenum != cd->region[reg].pagenum)
            continue;
        if (cd->region[r].offset + cd->region[r].size != cd->region[reg].offset)
            continue;

        // Make the old hole larger by increasing its size and remove the new hole
        cd->region[r].size += cd->region[reg].size;
        UnlinkElement (cd->region, &cd->rdempty, r);
        UnlinkElement (cd->region, &cd->rdempty, reg);
        PutRegionDescriptor (cd->region, &cd->rdfree, reg);
        InsertHole (cd->region, &cd->rdempty, r);
        }

    V (cd->mutex, ESM_ERROR);
    return ESM_OK;
    }


int
ESM_Shared_ListRegions (
    int thid,
    int ci,
    FILE * output
    ) {

    SetError (thid, "Shared memory ESM_ListRegions not yet implemented");
    return ESM_ERROR;
    }


//------------------------------------------------------------------------------
//  Internal functions


static int
AttachContext (
    int     thid,
    int     contextid
    ) {

    // Map a context descriptor and all of its pages to the address space
    // of the local process and return an index of a slot in the local
    // context table containing the mapped addresses.

    if (NumContexts >= ESM_MAX_CONTEXTS) {
        SetError (thid, "Too many contexts (%d) for process; Cannot attach context", ESM_MAX_CONTEXTS);
        return -1;
        }

    int ci = NumContexts++;
    Contexts[ci].contextid = contextid;
    Contexts[ci].nattached = 0;

    // Attach context descriptor

    Contexts[ci].cd = shmat (contextid, NULL, 0);
    if ((long) Contexts[ci].cd == -1) {
        SetError (thid, "Cannot attach context descriptor to process: %s", strerror (errno));
        return -1;
        }

    // Attach all pages listed in the context descriptor

    CD * cd = (CD *) Contexts[ci].cd;
    for (int pi = 0 ; pi < cd->npages ; pi++) {
        Contexts[ci].pageaddr[pi] = shmat (cd->page[pi].shmid, NULL, 0);
        if ((long) Contexts[ci].pageaddr[pi] == -1) {
            SetError (thid, "Cannot attach context page to process: %s", strerror (errno));
            return -1;
            }

        Contexts[ci].nattached++;
        }

    return ci;
    }


//------------------------------------------------------------------------------
//  Functions to manage region lists
//  The region (by name) and hole (by size) routines are very similar;
//  we really should combine them using C++.


static int
GetRegionDescriptor (
    RD      regions[],
    int *   freehead
    ) {

    // Peel off an empty region descriptor from the free list and return its index

    int reg = *freehead;
    if (reg == GND)
        return GND;   // no more empty descriptors

    *freehead = regions[reg].next;
    if (*freehead != GND)
        regions[*freehead].prev = GND;

    regions[reg].prev = GND;
    regions[reg].next = GND;

    return reg;
    }


static void
PutRegionDescriptor (
    RD      regions[],
    int *   freehead,
    int     index
    ) {

    // Put a cleared region descriptor at the head of a list (the free list)

    memset ((void *) &regions[index], 0, sizeof (RD));
    regions[index].prev = GND;
    regions[index].next = *freehead;
    *freehead = index;
    }


static int
SearchForRegion (
    RD      regions[],
    int *   head,
    char *  name
    ) {

    // Search for a region with the given name in the ordered list of regions

    int reg = *head;
    while (reg != GND) {
        if (strcmp (regions[reg].name, name) == 0)
            break;

        reg = regions[reg].next;
        }

    return reg;
    }


static int
SearchForHole (
    RD      regions[],
    int *   head,
    int     size
    ) {

    // Search for the smallest hole large enough to accomodate the requested size.
    // The hole list is order by size (ascending).

    int reg = *head;
    while (reg != GND) {
        if (regions[reg].size >= size)
            break;

        reg = regions[reg].next;
        }

    return reg;
    }


static void
InsertRegion (
    RD      regions[],
    int *   listhead,
    int     newreg
    ) {

    // If the region list is empty, make the new descriptor the first and last element

    if (*listhead == GND) {
        *listhead = newreg;
        regions[newreg].prev = GND;
        regions[newreg].next = GND;
        return;
        }

    // Insert the new descriptor in the list ordered by name (ascending)

    int prevreg = GND;
    int reg = *listhead;
    while (reg != GND) {
        if (strcmp (regions[reg].name, regions[newreg].name) >= 0) {    // found insertion place
            regions[newreg].next = reg;
            if (regions[reg].prev == GND) { // insert before 1st element of list
                regions[newreg].prev = GND;
                *listhead = newreg;
                }
            else {  // insert somewhere in the middle
                regions[newreg].prev = regions[reg].prev;
                regions[regions[reg].prev].next = newreg;
                }

            regions[reg].prev = newreg;
            break;
            }

        prevreg = reg;
        reg = regions[reg].next;
        }

    if (reg == GND) {   // insert after last element
        regions[newreg].prev = prevreg;
        regions[newreg].next = GND;
        regions[prevreg].next = newreg;
        }
    }


static void
InsertHole (
    RD      regions[],
    int *   listhead,
    int     newreg
    ) {

    // If the region list is empty, make the new descriptor the first and last element

    if (*listhead == GND) {
        *listhead = newreg;
        regions[newreg].prev = GND;
        regions[newreg].next = GND;
        return;
        }

    // Insert the new descriptor in the list ordered by size (ascending)

    int prevreg = GND;
    int reg = *listhead;
    while (reg != GND) {
        if (regions[reg].size >= regions[newreg].size) {    // found insertion place
            regions[newreg].next = reg;
            if (regions[reg].prev == GND) { // insert before 1st element of list
                regions[newreg].prev = GND;
                *listhead = newreg;
                }
            else {  // insert somewhere in the middle
                regions[newreg].prev = regions[reg].prev;
                regions[regions[reg].prev].next = newreg;
                }
            regions[reg].prev = newreg;
            break;
            }

        prevreg = reg;
        reg = regions[reg].next;
        }

    if (reg == GND) {   // insert after last element
        regions[newreg].prev = prevreg;
        regions[newreg].next = GND;
        regions[prevreg].next = newreg;
        }
    }


static void
UnlinkElement (
    RD      regions[],
    int *   head,
    int     index
    ) {

    // Remove the element with the given index from a doubly-linked list

    if (regions[index].prev == GND)     // remove first element from the list
        *head = regions[index].next;
    else
        regions[regions[index].prev].next = regions[index].next;

    if (regions[index].next != GND)
    regions[regions[index].next].prev = regions[index].prev;

    regions[index].prev = GND;
    regions[index].next = GND;
    }


//------------------------------------------------------------------------------
//  The following routines are used by the esm_info command to print information
//  about all contexts in the local host readable by the current user.  It is
//  not formally part of the ESM API.


int
ESM_Shared_Info (
    FILE * output
    ) {

    int thid = -1;

#if defined (HAVE_STRUCT_SHM_INFO)
    struct shm_info shm_info;

    // Get count of shared memory segments

    // TODO: make this portable
    // On OSX this does not return the number of shared memory segments but 0/-1

    // OSX:
    // http://developer.apple.com/library/mac/#documentation/darwin/reference/manpages/man2/shmctl.2.html
    // LINUX:
    // http://linux.die.net/man/2/shmctl
    // POSIX:
    // http://pubs.opengroup.org/onlinepubs/009695399/functions/shmctl.html
    // Discussion
    // http://lists.apple.com/archives/darwin-dev/2008/Apr/msg00001.html

    int maxid = shmctl (0, SHM_INFO, (struct shmid_ds *) &shm_info);
    if (maxid < 0) {
        SetError (thid, "Cannot get shared memory info: %s", strerror (errno));
        return ESM_ERROR;
        }

    //// fprintf (output, "maxid = %d\n", maxid);

    // Look for readable shared memory segments and when found attach them
    // and extract information

    for (int id = 0 ; id <= maxid ; id++) {
        struct shmid_ds segment;
        int shmid = shmctl (id, SHM_STAT, &segment);
        if (shmid < 0)
            continue;

        // Attach segment to local address space

        CD * cd = (CD *) shmat (shmid, NULL, 0);
        if ((long) cd == -1) {
            SetError (thid, "Cannot attach context descriptor segment in ESM_Shared_Info: %s", strerror (errno));
            DESTROY (shmid, 0);
            return (0);
            }

            // If the segment is an ESM context descriptor, print information about it

        if (cd->magic == ESM_MAGIC)
            PrintContextSegmentInfo (&segment, cd, output);

        // Detach segment from local address space

        if (shmdt ((shmem_t) cd) != 0)
            SetError (thid, "Cannot detach context descriptor segment in ESM_Shared_Info: %s", strerror (errno));
        }

    return ESM_OK;

#else
    SetError (thid, "ESM_Info not implemented on this platform");
    return ESM_ERROR;

#endif
    }


static void
PrintContextSegmentInfo (
    struct shmid_ds * segment,
    CD *    cd,
    FILE *  output
    ) {

    fprintf (output, "ESM Context %d\n", cd->contextid);
    fprintf (output, "\tsemaphore=%d\n", cd->mutex);
    fprintf (output, "\tattaches=%d\n", (int) segment->shm_nattch);

    int nholes = 0;
    for (int hole = cd->rdempty ; hole != GND ; hole = cd->region[hole].next) nholes++;
    fprintf (output, "\tholes=%d", nholes);
    if (nholes == 0)
        fprintf (output, "\n");

    nholes = 0;
    for (int hole = cd->rdempty ; hole != GND ; hole = cd->region[hole].next) {
        if (nholes % 10 == 0)  fprintf (output, "\n\t\t");
        fprintf (output, "%d ", cd->region[hole].size);
        nholes++;
        }

    fprintf (output, "\n\tregions=%d\n", cd->nregions);
    for (int reg = cd->rdlist ; reg != GND ; reg = cd->region[reg].next) {
        fprintf (output, "\t\tname=\"%s\", size=%d, pagenum=%d, offset=%d\n",
                cd->region[reg].name,
                cd->region[reg].size,
                cd->region[reg].pagenum,
                cd->region[reg].offset
            );
        }

    fprintf (output, "\n");
    }

#endif
