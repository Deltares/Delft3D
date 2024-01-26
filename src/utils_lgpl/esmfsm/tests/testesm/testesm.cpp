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
//  Test Program
//
//
//  Irv.Elshoff@deltares.nl
//  31 aug 04
//
//------------------------------------------------------------------------------


#include "esm.h"

#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>


#define NUMPROCESSES    10      // number of worker processes to fork
#define NUMROUNDS       100     // number of work rounds per process
#define NUMCELLS        1000    // number memory cells to test
#define SLEEP           10000   // sleep time between attempts in microseconds

#define ACTIONS         (2 * NUMCELLS / 3)
#define ESMFLAGS        0 // ESM_TRACE

#define RANDOMSIZE      (1000 + (10 * (random () % 200)))
//#define RANDOMSIZE    (20000)

    // problem: the bigger the size gets, the fewer cells we
    // can find in worker processes.
    // what's happening is that the master causes a new page to
    // be attached, but it is not attached to the worker.


static void     WorkerProcess   (int, int);
static void     Allocate        (int, int);
static void     Validate        (int, int);
static void     Free            (int);


int ContextID = 0;


//------------------------------------------------------------------------------


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    // Process command-line arguments

    if (argc != 2) {
        fprintf (stderr, "Usage: %s <contextID>\n", argv[0]);
        exit (1);
        }

    if ((ContextID = atoi (argv[1])) <= 0) {
        fprintf (stderr, "%s: Invalid contextID (%s)\n", argv[0], argv[1]);
        exit (1);
        }

    // Fork worker processes; they will wait until
    // the baton is created before proceeding.

    for (int i = 0 ; i < NUMPROCESSES ; i++) {
        if (fork () == 0) {
            WorkerProcess (NUMPROCESSES, i);
            }
        }

    // Initialize ESM

    srandom (getpid ());
    if (ESM_Init (ESMFLAGS) != ESM_OK) {
        fprintf (stderr, "ESM initialization fails for master process: %s\n", ESM_Error ());
        exit (1);
        }

    // Allocate the cell size table

    int * cellsize = (int *) ESM_Alloc (ContextID, "Sizes", NUMCELLS * sizeof (int));
    if (cellsize == NULL) {
        fprintf (stderr, "Cannot allocate cell size table: %s\n", ESM_Error ());
        exit (1);
        }

    // Initialize cells

    for (int i = 0 ; i < NUMCELLS ; i++)
        cellsize[i] = 0;

    // Allocate a random set of cells

    for (int visited = 0 ; visited < ACTIONS ; ) {
        int cell = random () % NUMCELLS;
        if (cellsize[cell] == 0) {
            int size = RANDOMSIZE;
            Allocate (cell, size);
            cellsize[cell] = size;
            visited++;
            }
        }

    // Allocate the rest of the cells

    for (int i = 0 ; i < NUMCELLS ; i++) {
        if (cellsize[i] == 0) {
            int size = RANDOMSIZE;
            Allocate (i, size);
            cellsize[i] = size;
            }
        }

    // Validate all allocated cells

    for (int i = 0 ; i < NUMCELLS ; i++)
        Validate (i, cellsize[i]);

    // Create a variable for the round number (and synchronization),
    // which triggers workers

    int * baton = (int *) ESM_Alloc (ContextID, "Baton", sizeof (int));
    if (baton == NULL) {
        fprintf (stderr, "Cannot allocate baton: %s\n", ESM_Error ());
        exit (1);
        }

    // Wait for childern to exit

    for (int i = 0 ; i < NUMPROCESSES ; i++)
        wait (NULL);

    // Free all cells

    for (int i = 0 ; i < NUMCELLS ; i++)
        Free (i);

    exit (0);
    }


//------------------------------------------------------------------------------
//  Child process routine


static void
WorkerProcess (
    int     nprocs,
    int     procid
    ) {

    printf ("Process %d is starting\n", procid); fflush (stdout);

    srandom (getpid ());
    if (ESM_Init (ESMFLAGS) != ESM_OK) {
        fprintf (stderr, "ESM initialization fails for process %d: %s\n", procid, ESM_Error ());
        exit (1);
        }

    // Wait for baton to appear in context

    int * baton = NULL;
    while (baton == NULL) {
        usleep (SLEEP);
        baton = (int *) ESM_Alloc (ContextID, "Baton", 0);
        }

    // Get address of cell size table

    int * cellsize = (int *) ESM_Alloc (ContextID, "Sizes", 0);
    if (cellsize == NULL) {
        fprintf (stderr, "Cannot find cell size table in process %d: %s\n", procid, ESM_Error ());
        exit (1);
        }

    // Start rounds

    for (int round = 0 ; round < NUMROUNDS ; round++) {
        // Wait for my turn

        //// printf ("Process %d is waiting for turn\n", procid); fflush (stdout);
        while ((*baton % nprocs) != procid)
            usleep (SLEEP);

        printf ("Process %d is active, round=%d, baton=%d\n", procid, round, *baton); fflush (stdout);

        // Validate all allocated cells, replace a random set of cells,
        // and then validate everything again

        for (int i = 0 ; i < NUMCELLS ; i++)
            Validate (i, cellsize[i]);

        for (int i = 0 ; i < ACTIONS ; i++) {
            int cell = random () % NUMCELLS;
            Free (cell);
            int size = RANDOMSIZE;
            Allocate (cell, size);
            cellsize[cell] = size;
            }

        for (int i = 0 ; i < NUMCELLS ; i++)
            Validate (i, cellsize[i]);

        // Pass control to next process

        (*baton)++;
        }

    printf ("Process %d is terminating\n", procid); fflush (stdout);
    exit (0);
    }


//------------------------------------------------------------------------------
//  Routines for the core operations


static void
Allocate (
    int     number,
    int     size
    ) {

    char name[20];
    sprintf (name, "Cell-%04d", number);

    void * ptr = ESM_Alloc (ContextID, name, size * sizeof (int));
    if (ptr == NULL) {
        fprintf (stderr, "Process %d cannot allocate \"%s\"\n", getpid (), name);
        exit (1);
        }

    int * integer = (int *) ptr;
    for (int i = 0 ; i < size ; i++)
        integer[i] = 100 * number + size;

    //// printf ("Allocated (\"%s\", %d)\n", name, size);
    }


static void
Validate (
    int     number,
    int     size
    ) {

    char name[20];
    sprintf (name, "Cell-%04d", number);

    void * ptr = ESM_Alloc (ContextID, name, 0);
    if (ptr == NULL) {
        fprintf (stderr, "Process %d cannot find \"%s\"\n", getpid (), name);
        exit (1);
        }

    int * integer = (int *) ptr;
    for (int i = 0 ; i < size ; i++) {
        if (integer[i] != 100 * number + size) {
            fprintf (stderr, "Process %d gets read error in \"%s\"\n", getpid (), name);
            exit (1);
            }
        }

    //// printf ("Validated (\"%s\", %d)\n", name, size);
    }


static void
Free (
    int     number
    ) {

    char name[20];
    sprintf (name, "Cell-%04d", number);

    if (ESM_Free (ContextID, name) != ESM_OK) {
        fprintf (stderr, "Process %d cannot free \"%s\"\n", getpid (), name);
        exit (1);
        }

    //// printf ("Freed (\"%s\")\n", name);
    }

