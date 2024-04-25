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

        Viewer/Selector File Management Functions
        -----------------------------------------
 */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "au.h"

typedef BInt4 boolean;

static boolean open_file = FALSE;
static BChar dtflnm[MAX_FILE_NAME];
static BChar dfflnm[MAX_FILE_NAME];
static BChar coding[]=" ";
BInt4 neffds;


/*  @@  Convenience function for others to get pointer
 *      to data file descriptor (see Nefis Manual)
 */
BInt4 FM_get_datafile_descriptor( BVoid )
{
    return neffds;
}


/*  @@  Convenience function for others to get pointer
 *      to definition file descriptor (see Nefis Manual)
 */
BInt4 FM_get_deftfile_descriptor( BVoid )
{
    return neffds;
}


/*  @@  Display the status of current opne Nefis files.
 */
BVoid FM_display_nefis_file_status( const BText filename )
{
    if ( open_file ) {
        FILE * tfp;

        /* get streampointer of output file */
        if ( filename != NULL ) {
            if (( tfp = fopen ( filename, "a" )) == NULL) {
                (BVoid)GEN_message_to_errorfile ( 102, filename );
                return;
            }
        }
        else {
            tfp = GEN_get_output_stream ();
        }

        GEN_print ( tfp, "datafile:%s\n"  , dtflnm );
        GEN_print ( tfp, "def.file:%s\n\n", dfflnm );

        PR_groups_print ( tfp );

        if (filename != NULL ) {
            (BVoid)fclose ( tfp );
        }
        else {
            GEN_close_output_stream ();
        }

    }
}


/*  @@  Check if file can be opened for reading and writing.
        return True if so, else False.
 */
static boolean FM_can_be_opened ( const BText file )
{
    FILE * stream;

    if (( stream = fopen ( file, "r" )) != NULL ) {
        (BVoid)fclose ( stream );
        return TRUE;
    }
    return FALSE;
}


/*  @@  Close open Nefis files if there are any.
 */
BVoid FM_close_nefis_files ( BVoid )
{
    if ( open_file ){
        (BVoid)Clsnef ( &neffds );
        open_file = FALSE;
        SM_remove_file_info ();
    }
}


/*  @@  Open Nefis files.
 */
BInt4 FM_open_nefis_files (
    const BText datfile,
    const BText deffile
    )
{
    BInt4 error = -1;

    /* if there are open files, close these files */
    FM_close_nefis_files ();

    /* first check if files can be opened for reading */
    if ( FM_can_be_opened ( datfile ) &&
         FM_can_be_opened ( deffile )) {

        error = Crenef( &neffds, datfile, deffile, ' ', 'r' );

        if( error == 0 ) {
               (BVoid)strcpy ( dtflnm, datfile );
               (BVoid)strcpy ( dfflnm, deffile );
               open_file = TRUE;
        }
        else {
            (BVoid)GEN_message_to_errorfile ( 4 );
        }
    }
    else {

        (BVoid)GEN_message_to_errorfile ( 5 );
    }

    return error;
}
