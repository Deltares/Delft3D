//-------------------------------------------------------------------------------
//  DelftOnline -- Program to Print Table of Contents of a DOL Server
//
//  Irv.Elshoff@Deltares.NL
//  27 apr 12
//-------------------------------------------------------------------------------
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


#include "DelftOnline.h"

#include <stdlib.h>

int
main (
    int    argc,
    char * argv[],
    char * envp[]
    ) {

    // Process command-line arguments

    if (argc != 2) {
        fprintf (stderr, "Usage: %s <url>\n", argv[0]);
        exit (1);
        }

    // Print contents

    DOL::Client * dol;
    try {
        dol = new DOL::Client (argv[1], DOL::SILENT, NULL);
        dol->PrintContents (stdout);
        delete dol;
        }
    catch (DOL::Exception * ex) {
        fprintf (stderr, "DOL Exception: %s\n", ex->message);
        }

    return 0;
    }
