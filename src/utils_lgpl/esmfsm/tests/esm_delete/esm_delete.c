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
 *  Utility program to delete a shared memory context from the shell.
 *
 *  Irv.Elshoff@deltares.nl
 *  30 aug 04
 *----------------------------------------------------------------------------*/


#include "esm.h"

#include <stdlib.h>


int
main (
    int     argc,
    char    *argv[],
    char    *envp[]
    ) {

    int     contextid;

    if (argc != 2) {
        fprintf (stderr, "Usage: %s <contextid>\n", argv[0]);
        exit (1);
        }

    if (sscanf (argv[1], "%d", &contextid) != 1) {
        fprintf (stderr, "Context ID \"%s\" is not a decimal integer\n", argv[1]);
        exit (1);
        }

    if (contextid < 10) {
        fprintf (stderr, "Context ID %d looks strange and will not be deleted\n", contextid);
        exit (1);
        }

    if (ESM_Init (ESM_SILENT) != 0) {
        fprintf (stderr, "ESM initialization fails: %s\n", ESM_Error ());
        printf ("0\n");
        exit (1);
        }

    if (ESM_Delete (contextid) != 0) {
        fprintf (stderr, "Unable to delete context %d (0x%x): %s\n", contextid, contextid, ESM_Error ());
        exit (1);
        }

    fprintf (stderr, "Context %d (0x%x) has been deleted\n", contextid, contextid);
    exit (0);
    }
