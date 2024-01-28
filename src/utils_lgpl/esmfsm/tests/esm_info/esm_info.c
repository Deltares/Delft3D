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
/*  Utility program to list infomation about ESM contexts. */
/*  UNIX only. */
/* */
/*  Irv.Elshoff@deltares.nl */
/*  30 aug 04 */
/*------------------------------------------------------------------------------ */


#include "esm.h"

#include <stdlib.h>


static void
usage (
    char    *cmd
    ) {

    fprintf (stderr, "Usage: %s\n", cmd);
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

    if (argc != 1)
        usage (argv[0]);

    if (ESM_Init (0) != ESM_OK) {
        fprintf (stderr, "ESM initialization fails: %s\n", ESM_Error ());
        exit (1);
        }

    if (ESM_Shared_Info (stdout) != ESM_OK) {
        fprintf (stderr, "ESM fails: %s\n", ESM_Error ());
        exit (1);
        }

    exit (0);
    }

