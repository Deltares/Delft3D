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

        Viewer/Selector Element Management Functions
        --------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>

#include "au.h"

/*  @@  allocate memory for an element strcuture
 */
static VsElmData EL_alloc_memory_for_element ( BVoid )
{
    int       i;
    VsElmData p;
    p = (VsElmData) GEN_malloc ( sizeof ( VsElm ));

    p->elmndm = MAX_DIM;
    p->nbytsg = -1;
    strcpy (p->elmnam,"");
    strcpy (p->elmtyp,"");
    strcpy (p->elmqty,"");
    strcpy (p->elmunt,"");
    strcpy (p->elmdes,"");
    for ( i=0; i<MAX_DIM; i++)
    {
      p->elmdms[i] = -1;
    }
    p->left  = NULL;
    p->right = NULL;

    return p;
}


/*  @@  Add an element-structure to binary tree
 */
VsElmData EL_add_element_to_tree (
          VsElmData p,
    const BText     elmnam )
{
    if ( p == NULL ) {
        /* here we are at the right spot in the tree,
           so allocate memory */
        p = EL_alloc_memory_for_element ();

        if ( p != NULL ) {
            /* there is room, so place info in strcuture */
            (void)strcpy ( p->elmnam, elmnam );
            p->left  = NULL;
            p->right = NULL;

        }
        else {
            /* no memory available */
            (void)GEN_message_to_errorfile ( 1 );
        }

    }
    else {

        BInt4 cond;

        cond = GEN_string_compare ( elmnam, p->elmnam );

        if ( cond < 0 ) {
            p->left = EL_add_element_to_tree ( p->left, elmnam );
        }

        else if ( cond > 0 ) {
            p->right = EL_add_element_to_tree ( p->right, elmnam );
        }
        else {
            /* DO NOTHING */
        }
    }

    return p;
}


/*  @@  find element with name elmnam in tree of element-structures.
 *      If found, return pointer to structure, else NULL.
 */
VsElmData EL_find_element_in_tree (
          VsElmData p,
    const BText     elmnam )
{
    VsElmData retval=NULL;

    if ( p != NULL ) {

        BInt4 cond;

        /* not at end of branche, so compare */
        cond = GEN_string_compare ( elmnam, p->elmnam );

        if (cond == 0 ) {
            /* found !!!!!! */
            retval = p;

        }
        else if ( cond < 0 ) {
            /* traverse the left branche */
            retval = EL_find_element_in_tree ( p->left, elmnam );
        }

        else {
            /* traverse the right branche */
            retval = EL_find_element_in_tree ( p->right, elmnam );
        }
    }
    /* not found */
    return retval;
}


/*  @@  remove a complete branche of elements.
 */
BVoid EL_remove_element_branche ( VsElmData p )
{
    if ( p != NULL ) {
        EL_remove_element_branche ( p->left );
        EL_remove_element_branche ( p->right );
        GEN_free ( p );
    }
}
