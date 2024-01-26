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
/*------------------------------------------------------------------------------
 *  Delft-ESM (Easy Shared Memory)
 *  Utility program to create a shared memory context from the shell.
 *
 *  Irv.Elshoff@deltares.nl
 *  12 nov 04
 *----------------------------------------------------------------------------*/


#include "esm.h"

#include <stdlib.h>

#define PAGESIZE    10240           /* Default page size in 1K-blocks */

#define ESMFLAGS   ESM_SILENT
/*#define ESMFLAGS   ESM_TRACE */


static void
usage (
    char    *cmd
    ) {

    fprintf (stderr, "Usage: %s [<pagesize>]\n", cmd);
    printf ("0\n");
    exit (1);
    }


int
main (
    int     argc,
    char    *argv[],
    char    *envp[]
    ) {

    int     contextid;
    int     pagesize;
    char    dummy [1000];

    if (argc == 1)
        pagesize = PAGESIZE;
    else if (argc == 2) {
        if (sscanf (argv[1], "%d%s", &pagesize, dummy) != 1)
            usage (argv[0]);
        }
    else
        usage (argv[0]);

    fprintf (stderr, "Page size is %d KB\n", pagesize);
    fprintf (stderr, "Maximum number of pages in context is %d\n", ESM_MAX_PAGES);
    fprintf (stderr, "Maximum size of context is %d MB\n", ESM_MAX_PAGES * pagesize / 1024);
    fprintf (stderr, "Maximum number of regions (allocated variables) is %d\n", ESM_MAX_REGIONS);

    if (ESM_Init (ESMFLAGS) != 0) {
        fprintf (stderr, "ESM fails: %s\n", ESM_Error ());
        printf ("0\n");
        exit (1);
        }

    contextid = ESM_Create (1, pagesize);
    if (contextid == 0) {
        fprintf (stderr, "ESM initialization fails: %s\n", ESM_Error ());
        printf ("0\n");
        exit (1);
        }

    fprintf (stderr, "\nContext ID is %d (0x%x)\n", contextid, contextid);
    printf ("%d\n", contextid);
    exit (0);
    }

