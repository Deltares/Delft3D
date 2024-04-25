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

        Viewer/Selector Group Management Functions
        ------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "au.h"

/*  @@  Allocate memory for a group structure.
 */
static VsGrpData GR_alloc_memory_for_group ( BVoid )

{
    BInt4     i;
    VsGrpData p;
    p = (VsGrpData)GEN_malloc ( sizeof( VsGrp ));

    strcpy(p->grpdef,"");
    strcpy(p->grpnam,"");
    p->nintat = MAX_DIM;
    p->nreaat = MAX_DIM;
    p->nstrat = MAX_DIM;
    for ( i=0; i<MAX_DIM; i++)
    {
      p->grpdms[i] = -1;

      p->IntAttrValue[i] = 999;
      strcpy(p->IntAttrName [i],"");

      p->RealAttrValue[i] = (BRea4) 999.999;
      strcpy(p->RealAttrName [i],"");

      strcpy(p->StringAttrValue[i],"");
      strcpy(p->StringAttrName [i],"");
    }
    p->next = NULL;

    return p;
}

/*  @@  Add a group-structure to end of chain.
 *
 *  - find the end of the chained list, pointed at by p;
 *  - allocate memory for a new group;
 *  - if memory available, fill items of structure.
 *
 *  Note: a group with groupname already in chain, will not be
 *        added or updated.
 */
VsGrpData GR_add_group_to_chain (
          VsGrpData p,              /* pointer to a struct in chain */
    const BText     grpnam,         /* name of group to add */
    const BText     grpdef,         /* group def. name of this group */
    const BInt4     grpdms[],
          VsAtrData att
    )

{
    /* try to find end of chain */
    if ( p == NULL ) {
        /* end of chain, so allocate memory */
        p = GR_alloc_memory_for_group ();

        if ( p != NULL ) {
            BInt4 i;

            /* there is room, so place info in structure */
            (BVoid)strcpy ( p->grpnam, grpnam );
            (BVoid)strcpy ( p->grpdef, grpdef );

            for (i = 0 ; i < 5 ; i++ ) {
                p->grpdms[i] = grpdms[i] ;
            }

            /* place attributes in structure */
            for ( p->nintat = 0 ; p->nintat < att->nintat ;
                  p->nintat++ ) {
                (BVoid)strcpy (  p->IntAttrName[p->nintat],
                               att->IntAttrName[p->nintat] );
                p->IntAttrValue[p->nintat] = att->IntAttrValue[p->nintat];
            }

            for ( p->nreaat = 0 ; p->nreaat < att->nreaat ;
                  p->nreaat++ ) {
                (BVoid)strcpy (  p->RealAttrName[p->nreaat],
                               att->RealAttrName[p->nreaat] );
                p->RealAttrValue[p->nreaat] = att->RealAttrValue[p->nreaat];
            }

            for ( p->nstrat = 0 ; p->nstrat < att->nstrat ;
                  p->nstrat++ ) {
                (BVoid)strcpy (  p->StringAttrName[p->nstrat],
                               att->StringAttrName[p->nstrat]);
                (BVoid)strcpy (  p->StringAttrValue[p->nstrat],
                               att->StringAttrValue[p->nstrat]);

            }

            /* indicate end of chain */
            p->next = NULL;

        }
        else
        {
            /* no memory available */
            (BVoid)GEN_message_to_errorfile ( 1 );
        }

    }
    else if ( GEN_string_compare ( grpnam, p->grpnam ) != 0 )
    {
        /* try next structure in chain */
        p->next = GR_add_group_to_chain ( p->next, grpnam, grpdef
                                        , grpdms, att );
    }
    else
    {
       /* group found */
    }
    return p;
}


/*  @@  Find group with name grpnam in chain of groupstructures.
 *      If found, return pointer, else return NULL.
 */
VsGrpData GR_find_group_in_chain (
          VsGrpData p,      /* pntr to a structure in chain */
    const BText     grpnam  /* name of group to find */
    )

{
    if ( p != NULL ) {
        /* not at end of chain, so compare */
        if ( GEN_string_compare ( grpnam, p->grpnam ) == 0 ) {
            return p;
        }
        else {
            /* to next structure in chain */
            return GR_find_group_in_chain ( p->next, grpnam );
        }

    }
    /* not found */
    return NULL;
}


/*  @@  remove group-structures from chain from p to end
 *
 *      Note: if p points to first, all groups disapear.
 */
BVoid GR_remove_groups_from_chain(
    VsGrpData p   /* pointer to group-struct in chain */
    )
{
    if ( p != NULL ) {
        GR_remove_groups_from_chain ( p->next );
        GEN_free ( p );
    }
}
