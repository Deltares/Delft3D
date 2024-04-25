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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "au.h"

/*  @@
 */
void FU_set_value (
    BText varnam,
    BRea4 value
    )
{
    VsVarData p;

    /* check if variable not already exists */
    if ( VR_get_pointer_to_variable ( varnam ) != NULL ) {
        GEN_message_to_errorfile ( 9 );
        return;
    }

    /* allocate memory for a variable structure */
    if (( p = VR__alloc_memory_for_variable ()) != NULL ) {

        BInt4 i;

        (void)strcpy ( p->varnam, varnam );
        (void)strcpy ( p->elmtyp, "REAL" );
        p->nbytsg = 4;
        (void)strcpy ( p->grpnam, "--User Defined--" );
        (void)strcpy ( p->elmnam, "--" );
        p->grpndm = 1;
        p->elmndm = 1;
        for ( i = 0 ; i < 5 ; i++) {
            p->grpdms[i] = 1;
            p->elmdms[i] = 1;
        }
        p->varpnt = (void *)GEN_malloc ( sizeof ( float ));

        if ( p->varpnt != NULL ) {
            ((float *)p->varpnt)[0] = value;
            VR_add_variable_to_memo ( p ) ;
        }
        else {
            GEN_message_to_errorfile ( 1 );
            GEN_free ( p );
        }
    }
    else {
        GEN_message_to_errorfile ( 1 );
    }
}
