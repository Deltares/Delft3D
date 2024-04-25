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

        Viewer/Selector Process Execution Functions
        -------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "au.h"

static BVoid EX_read_pipe_vars ( BText tempfile )
{
    VsVarData   pn;
    FILE      * file;

    /* get new-vars from temporarily file */
    file = fopen (tempfile , "r");
    if (file != NULL) {
        while((pn = PP_read_var_from_pipe (file)) != NULL) {
            VR_release_variable (pn->varnam);
            VR_add_variable_to_memo (pn);
        }
        (BVoid) fclose ( file ) ;
        (BVoid) remove (tempfile);
    }
}


static BText EX_make_command_string (
        BText path,
        BText parms[])
{
    static BChar commandstring[COMMAND_STRING_LENGHT+1];
           BInt4 i=0;

    (BVoid)sprintf (commandstring, "%s", path);

    while ( parms[i] != NULL ) {
        (BVoid) strcat (commandstring, " ");
        (BVoid) strcat (commandstring, parms[i]);
        i++;
    }

    return commandstring;
}


/*  @@
 */
BVoid EX_process (
        BText process,
        BInt4 retflag)
{
    EX_process_with_vars ( process, NULL, retflag );

}

BVoid EX_process_with_parms (
        BText path,
        BText parms[],
        BInt4 retflag)

{

    EX_process_with_vars ( EX_make_command_string ( path, parms )
                          , NULL, retflag );
}


BVoid EX_write_variables_to_pipe (
        FILE * pipe,
        BText  varname[] )
{
    if ( varname != NULL ) {
        VsVarData p;
        BInt4     i=0;

        while (varname[i] != NULL ) {
            p = VR_get_pointer_to_variable (varname[i]);
            PP_write_var_to_pipe( pipe, p );
            i++;
        }
    }
}


BVoid EX_process_with_vars (
        BText path,
        BText varnames[],
        BInt4 retflag)
{
    FILE * pipe;


    /* first check if variables exist (this function prints
       also the error messages */
    if ( VR_chck_existnce_of_variables ( varnames ) == 0 ) {

        BText tempfile;
        BChar command[COMMAND_STRING_LENGHT+1];

        /* make temp file name */
        tempfile = tmpnam (NULL);

        /* open pipe */
        if (retflag != 1) {
            (BVoid)sprintf ( command, "%s", path);
        }
        else {
            (BVoid)sprintf (command, "%s >%s", path, tempfile);
        }
#if defined WIN32
        pipe = _popen ( command, "w" );
#else
        pipe =  (FILE *) popen ( command, "w" );
#endif

        if ( pipe != NULL ) {

            (BVoid)EX_write_variables_to_pipe ( pipe, varnames );
#if defined WIN32
            (BVoid) _pclose ( pipe );
#else
            (BVoid) pclose ( pipe );
#endif

            if (retflag == 1) {
                EX_read_pipe_vars ( tempfile );
            }
        }
        else {
            GEN_message_to_errorfile ( 201, path );
        }
    }
}

BVoid EX_process_with_vars_and_parms (
        BText path,
        BText varnames[],
        BText parnms[],
        BInt4 retflag)
{
    EX_process_with_vars (
            EX_make_command_string( path, parnms ), varnames, retflag );
}
