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

        Viewer/Selector Defintion Management Functions
        ----------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "au.h"

/*  @@  allocate memory for a defintion structure
 */
static VsDefData DF_alloc_memory_for_definition ( BVoid )
{
    int       i;
    VsDefData p;
    p = (VsDefData) GEN_malloc ( sizeof( VsDef ));

    p->grpndm = -1;
    strcpy (p->grpdef,"");
    strcpy (p->celnam,"");
    for ( i=0; i<MAX_DIM; i++)
    {
      p->grpord[i] = -1;
    }
    p->left  = NULL;
    p->right = NULL;

    return p;
}

/*  @@  Add a definition-structure to binary tree
 *
 *      - find the proper place for new structre in tree
 *      - allocate memory for a new defintion;
 *      - if memeory available, fill items of structure.
 */
VsDefData DF_add_definition_to_tree (
    VsDefData p,
    const BText     grpdef
    )
{
    BInt4 cond;

    if ( p == NULL ) {
        /* here we are at the right spot in the tree,
           so allocate memory */
        p = DF_alloc_memory_for_definition ();

        if ( p != NULL ) {
            /* there is room, so place info in structure */
            (BVoid)strcpy ( p->grpdef, grpdef );
            p->left  = NULL;
            p->right = NULL;
        }
        else {
            /* no memory available */
            (BVoid)GEN_message_to_errorfile ( 1 );
        }
    }

    else {
        cond = GEN_string_compare ( grpdef, p->grpdef );

        if (cond < 0 ) {
            p->left = DF_add_definition_to_tree ( p->left, grpdef );
        }

        else if ( cond > 0 ) {
            p->right = DF_add_definition_to_tree ( p->right, grpdef );
        }

        else {
            /* DO NOTHING */
        }
    }
    return p;
}


/*  @@  find definition with name grpdef in tree of
 *      definition-structures.
 *      If found, return pointer to that structure, else NULL.
 */
VsDefData DF_find_definition_in_tree (
    VsDefData p,
    const BText     grpdef
    )
{
    VsDefData retval=NULL;

    if ( p != NULL ) {

        BInt4 cond;

        cond = GEN_string_compare ( grpdef, p->grpdef );

        if (cond == 0 ) {
            retval = p;
        }

        else if ( cond < 0 ) {
            /* go to left branch */
            retval = DF_find_definition_in_tree ( p->left, grpdef );
        }

        else {
            /* go to right branch */
            retval = DF_find_definition_in_tree ( p->right, grpdef );
        }
    }
    /* not found */
    return retval;
}


/*  @@  remove a complete branch of defintions,
 *      pointed at by p
 */
BVoid DF_remove_definition_branche (
    VsDefData p
    )
{
    if ( p != NULL ) {
        DF_remove_definition_branche ( p->left );
        DF_remove_definition_branche ( p->right );
        GEN_free ( p );
    }
}
