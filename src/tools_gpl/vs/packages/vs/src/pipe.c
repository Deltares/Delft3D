//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
/*

        Viewer/Selector dos pipe emulation functions
        --------------------------------------------
 */

#ifdef MSDOS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "au.h"

static BChar   Fg_process[120];
static BChar   Fg_tempfile[120];
static BInt2   Fg_pipe_is_open=False;


/*  @@  the functions emulate a simple version of the unix
        popen (only for write) and pclose for DOS environment.
        Only one pipe can be open at the same time.
 */

FILE * popen (
    const BText command,
    const BText type
    )
{
    FILE * stream;
    BText  tmp;

    if ( !Fg_pipe_is_open ) {
        if ( strcmp ( type, "w" ) == 0 ) {

            /* create temp file */
            tmp = tempnam ( NULL, "vs_" );
            (void)strcpy ( Fg_tempfile, tmp );
            (void)free( tmp );

            /* save name of process to start */
            (void) sprintf ( Fg_process, "%s <%s", command,
                             Fg_tempfile );

            stream = fopen ( Fg_tempfile, "w" );

            Fg_pipe_is_open = (stream != NULL);

            return stream;
        }
        else {
            return NULL;
        }
    }
    else {
        /* no two pipes at the same time */
        return NULL;
    }
}

/*  @@
 */
BInt4 pclose ( FILE * stream )
{
    if ( Fg_pipe_is_open ) {

        /* close stream file and execute process */
        (void)fclose ( stream );
        (void)system ( Fg_process );

        /* remove tempfile */
        (void)remove ( Fg_tempfile );
        Fg_pipe_is_open = False;
    }
}
#endif
