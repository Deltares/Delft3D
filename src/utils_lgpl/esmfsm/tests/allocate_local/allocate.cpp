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
//  Allocate a bunch of non-shared memory to see how far we can go
//
//  Irv.Elshoff@deltares.nl
//  31 jan 06
//
//------------------------------------------------------------------------------


#include "esm.h"

#include <stdlib.h>

#if defined(HAVE_CONFIG_H)
#   include <sys/wait.h>
#   include <unistd.h>
#endif


//#define ESMFLAGS      0
#define ESMFLAGS        ESM_TRACE


//------------------------------------------------------------------------------


int
main (
    int     argc,
    char *  argv[],
    char *  envp[]
    ) {

    int blocksize = 0;
    int numblocks = 0;

    // Process command-line arguments

    if (argc != 3) {
        fprintf (stderr, "Usage: %s <blocksize> <numblocks>\n", argv[0]);
        exit (1);
        }

    if ((blocksize = atoi (argv[1])) <= 0) {
        fprintf (stderr, "%s: Invalid block size (%s)\n", argv[0], argv[1]);
        exit (1);
        }

    if ((numblocks = atoi (argv[2])) <= 0) {
        fprintf (stderr, "%s: Invalid number of blocks (%s)\n", argv[0], argv[2]);
        exit (1);
        }

    // Initialize ESM

    if (ESM_Init (ESMFLAGS) != ESM_OK) {
        fprintf (stderr, "ESM initialization fails: %s\n", ESM_Error ());
        exit (1);
        }

    int context = ESM_Create (0, 0);    // local memory

    // Allocate blocks

    int block;
    for (block = 0 ; block < numblocks ; block++) {
        char name [100];
        sprintf (name, "block-%d", block);
        if (ESM_Alloc (context, name, blocksize) == 0) {
            break;
            }
        }

    printf ("%d/%d blocks of %d bytes allocated\n", block, numblocks, blocksize);
    printf ("%ld/%ld bytes total allocated\n", (long) block * blocksize, (long) numblocks * blocksize);

    return 0;
    }
