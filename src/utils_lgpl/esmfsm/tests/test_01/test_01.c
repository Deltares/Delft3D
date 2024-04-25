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
#if defined(WIN32)
#  include <io.h>
#else
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <unistd.h>
#endif

#include "esm.h"

#define DATASIZE        400
#define TRYS            10000
#define SLEEP           200000


void
JustDoIt (
    int     putter,
    int     esmContext
    ) {

    const char *  role = (putter == 0) ? "getter" : "putter";
    int     n;
    char    fn [100];
    FILE *  fh;
    char *  name = "data";
    void *  dataHandle;


    sprintf (fn, "results.%s", role);

    if (NULL == (fh = fopen (fn, "w"))) {
        printf ("%s cannot open output file\n", role);
        exit (1);
        }

    if (esm_connect (esmContext) != 0) {
        printf ("%s not connected to context %d\n", role, esmContext);
        exit (1);
        }

    if (putter) {
        dataHandle = esm_alloc (esmContext, name, DATASIZE);

        printf ("%s %screated at 0x%x\n", name, (dataHandle == NULL) ? "NOT " :
"", dataHandle);
        fflush (stdout);
        }

    else {      // getter
        int s = DATASIZE;
        /*int s = 0; */

        usleep (SLEEP);

        for (n = 0 ; n < TRYS ; n++) {
            printf ("Attempting access to %s\n", name);
            fflush (stdout);

            if (NULL != (dataHandle = esm_alloc (esmContext, name, s)))
                break;

            printf ("Wait #%d for %d usec\n", n+1, SLEEP);
            fflush (stdout);
            usleep (SLEEP);
            }

        if (n >= TRYS) {
            printf ("Too many waits...\n");
            goto end;
            }

        printf ("%s found at 0x%x\n", name, dataHandle);
        fflush (stdout);
        }

    end: {
        esm_prinfo (esmContext, fh);
        fclose (fh);
        return;
        }
    }


int
main (
    int     argc,
    char *  argv[]
    ) {

    int esmContext = -1;

    if (-1 == (esmContext = esm_create (4000000, 0))) {
        printf ("Cannot create context\n");
        exit (1);
        }

    printf ("Created context %d\n", esmContext);

    if (fork () == 0) {                 // child
        JustDoIt (0, esmContext);
        exit (0);
        }

    else {                              // parent
        JustDoIt (1, esmContext);
        wait (NULL);
        esm_delete (esmContext);
        printf ("Deleted context %d\n", esmContext);
        exit (0);
        }

    return 0;
    }
