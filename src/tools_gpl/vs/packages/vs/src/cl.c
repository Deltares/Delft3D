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

        Viewer/Selecor Cell Management Functions
        ----------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "au.h"

/*  @@  allocate memory for a cell structure and initialize
 */
static VsCelData CL_alloc_memory_for_cell ( BVoid )
{
    int       i;
    VsCelData p;
    p = ( VsCelData ) GEN_malloc ( sizeof ( VsCel ));

    strcpy (p->celnam,"");
    p->nelems = MAX_CEL_DIM;
    for ( i=0; i<MAX_CEL_DIM; i++)
    {
      strcpy (p->elmnms[i],"");
    }
/*
    p->left  = NULL;
    p->right = NULL;
*/

    return p;
}


/*  @@  add a cell-structure to binary tree and fill this
 *      structure
 */
VsCelData CL_add_cell_to_tree (
          VsCelData p,
    const BText     celnam
    )
{
    if ( p == NULL ) {
        /* here we are at the right spot in the tree,
           so allocate memory */
        p = CL_alloc_memory_for_cell ();

        if ( p != NULL ) {
            /* there is room, so place info in structure */
            (BVoid)strcpy ( p->celnam, celnam );
            p->left  = NULL;
            p->right = NULL;

        }
        else {
            /* no memory available */
            (BVoid)GEN_message_to_errorfile ( 1 );
        }

    }

    else {
        BInt4 cond;

        cond = GEN_string_compare ( celnam, p->celnam );
        if ( cond < 0 ) {
            p->left = CL_add_cell_to_tree ( p->left, celnam );
        }

        else if ( cond > 0 ) {
            p->right = CL_add_cell_to_tree ( p->right, celnam );
        }
        else {
            /* DO NOTHING */
        }
    }

    return p;
}


/*  @@  find cell with name celnam in tree of cell-structures.
 *      if found, return pointer to that structure, else NULL.
 */
VsCelData CL_find_cell_in_tree (
          VsCelData p,
    const BText     celnam
    )
{
    VsCelData retval=NULL;

    if ( p != NULL ) {

        BInt4 cond;

        cond = GEN_string_compare ( celnam, p->celnam );
        if ( cond == 0 ) {
            retval = p;
        }

        else if ( cond < 0 ) {
            /* go to the left branche */
            retval = CL_find_cell_in_tree ( p->left, celnam );
        }

        else {
            /* go to the right branche */
            retval = CL_find_cell_in_tree ( p->right, celnam );
        }
    }
    /* not found */
    return retval;
}


/*  @@  remove a complete branch of cells
 */
BVoid CL_remove_cell_branche ( VsCelData p )
{
    if ( p != NULL ) {
        CL_remove_cell_branche ( p->left );
        CL_remove_cell_branche ( p->right );
        GEN_free ( p );
    }
}
