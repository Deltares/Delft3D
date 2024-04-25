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

        Viewer/Selector Structure Management Functions
        ----------------------------------------------
 */

#include <stdio.h>
#include <string.h>

#include "au.h"
#include "nefis.h"

extern BInt4 neffds;

static VsGrpData grpinf = NULL;
static VsDefData definf = NULL;
static VsCelData celinf = NULL;
static VsElmData elminf = NULL;

BChar   Nefstr[200];
BInt4   ierror     ;

/*  @@  return pointer to begin of group-chain
 */
VsGrpData SM_get_group_pointer ( BVoid )
{
    return grpinf;
}


/*  @@  return pointer to begin of definition-tree
 */
VsDefData SM_get_definition_pointer ( BVoid )
{
    return definf;
}


/*  @@  return pointer to begin of cell-tree
 */
VsCelData SM_get_cell_pointer ( BVoid )
{
    return celinf;
}


/*  @@  return pointer to begin of group-tree
 */
VsElmData SM_get_element_pointer ( BVoid )
{
    return elminf;
}


/*  @@ remove all file info from memory.
 */
BVoid SM_remove_file_info ( BVoid )
{
    GR_remove_groups_from_chain ( grpinf );
    grpinf = NULL;

    DF_remove_definition_branche ( definf );
    definf = NULL;

    CL_remove_cell_branche ( celinf );
    celinf = NULL;

    EL_remove_element_branche ( elminf );
    elminf = NULL;
}


/*  @@  Adjust element dimensions.
 */
static BVoid SM_adjust_element_dimensions ( VsElmData p )
{
    /* if ndim = 1 and p->elmdms[0] = 0, make it 1 */
    if ( p->elmndm == 1 && p->elmdms[0] == 0 )
    {
        p->elmdms[0] = 1;
    }

    /* if ndim = 0 , make ndim 1 and the dimension 1 */
    else if ( p->elmndm == 0 )
    {
        p->elmndm = 1;
        p->elmdms[0] = 1;
    }
}



/*  @@ Read info of element with name p->elmnam from def-file and
       place values in structure pointed at by p.
 */
static BVoid SM_read_element_info (
    BInt4     neffds,
    VsElmData p )
{
    BInt4 error = -1;

    if ( p != NULL )
    {
        SM_read_element_info ( neffds, p->left );

        error = Inqelm ( &neffds, p->elmnam, p->elmtyp, &p->nbytsg,
                                 p->elmqty, p->elmunt,  p->elmdes,
                                &p->elmndm, p->elmdms );
        SM_adjust_element_dimensions ( p );

        if ( error == 0 )
        {
            SM_read_element_info ( neffds, p->right );
        }
        else
        {
            (BVoid)GEN_message_to_errorfile ( 2, error );
        }
    }
}


/*  @@ Read info of cell with name p->celnam from def-file and
       place values in structure pointed at by p.
 */
static BVoid SM_read_cell_info (
    BInt4     neffds,
    VsCelData p )
{
    if ( p != NULL )
    {
        BInt4 error = -1;

        SM_read_cell_info ( neffds, p->left );

        error = Inqcel ( &neffds, p->celnam, &p->nelems, p->elmnms );

        if ( error == 0 )
        {
            int i;
            for ( i = 0 ; i <p->nelems ; i++ )
            {
                elminf = EL_add_element_to_tree ( elminf, p->elmnms[i] );
            }
            SM_read_cell_info ( neffds, p->right );
        }
        else
        {
            (BVoid)GEN_message_to_errorfile ( 2, error );
        }
    }
}

/*  @@  Adjust group dimensions.
 */
static BVoid SM_adjust_group_dimensions ( VsDefData p )
{
    /* if ndim = 0 , make ndim 1 and the dimension 1 */
    if ( p->grpndm == 0 )
    {
        p->grpndm = 1;
    }
}

static int SM_get_groupdimensions (
          BInt4 neffds,
    const BText grpnam,
          BText grpdef,
          BInt4 grpdms[]
    )
{
    BInt4 grpord[MAX_DIM];
    BInt4 grpndm;
    BChar celnam[MAX_NAME+1];
    BInt4 error;
    BInt4 i;

    error = Inqgrp ( &neffds, grpdef, celnam, &grpndm, grpdms, grpord );

    for ( i = 0 ; i < max(grpndm, 1) ; i++ )
    {

        if ( grpdms[i] == 0 ) {

            BInt4 maxind = 0;

            error = Inqmxi ( &neffds, grpnam, &maxind );

            grpdms[i] = maxind;
            break;
        }
    }

    /* if ndim = 0 , make ndim 1  */
    if ( grpndm == 0 )
    {
        grpndm = 1;
    }

    return 0;
}


/*  @@ Read info of groupdef with name p->grpdef from def-file and
       place values in structure pointed at by p.
 */
static BVoid SM_read_definition_info (
          BInt4     neffds,
    const BText     grpnam,
          VsDefData p
    )
{
    if ( p != NULL )
    {
        BInt4 error = -1;
        BInt4 grpdms[MAX_DIM];

        SM_read_definition_info ( neffds, grpnam, p->left );

        error = Inqgrp ( &neffds, p->grpdef, p->celnam, &p->grpndm,
                                  grpdms, p->grpord );

        SM_adjust_group_dimensions ( p );

        celinf = CL_add_cell_to_tree ( celinf, p->celnam );

        SM_read_definition_info ( neffds, grpnam, p->right );
    }
    return;
}


/* @@  get attributes of group with groupname p->group from file
 *     and add this values to the structure
 */
static BVoid SM_get_groupattribs_from_file (
          BInt4     neffds,
    const BText     grpnam,
          VsAtrData p       /* pointer to structure */
    )
{
    BChar  attnam[MAX_NAME+1] = "\0";
    BInt4  iatval = 0;
    BRea4  ratval = 0.0;
    BChar  satval[MAX_NAME+1] = "\0";
    BInt4  error = -1;

    /* retrieve all attributes of this group and put them in p */

    /* first the integer attributes */
    p->nintat = 0;

    error = Inqfia ( &neffds, grpnam, attnam, &iatval );

    while ( error == 0 )
    {
        (BVoid)strcpy ( p->IntAttrName[p->nintat], attnam );
        p->IntAttrValue[p->nintat++] = iatval;

        error = Inqnia ( &neffds, grpnam, attnam, &iatval );
    }

    /* then the real attributes */
    p->nreaat = 0;

    error = Inqfra ( &neffds, grpnam, attnam, &ratval );

    while ( error == 0 )
    {
        (BVoid)strcpy ( p->RealAttrName[p->nreaat], attnam );
        p->RealAttrValue[p->nreaat++] = ratval;

        error = Inqnra ( &neffds, grpnam, attnam, &ratval );
    }

    /* then the string attributes */
    p->nstrat = 0;

    error = Inqfsa ( &neffds, grpnam, attnam, satval );

    while (error == 0)
    {
        (BVoid)strcpy ( p->StringAttrName[p->nstrat  ], attnam );
        (BVoid)strcpy ( p->StringAttrValue[p->nstrat++], satval );

        error = Inqnsa ( &neffds, grpnam, attnam, satval );
    }
}

/*  @@  Read all meta-info from opened nefis files.
 */
BVoid SM_read_nefis_meta_data( BInt4 neffds)
{
    BChar   grpnam[]  = "                ";
    BChar   grpdef[]  = "                ";
    BInt4   error;

    error = Inqfst ( &neffds, grpnam, grpdef );
    while ( error == 0 )
    {
        BInt4 grpdms[] = { -1, -1, -1, -1, -1 };

        /* get the group dimensions */
        if (SM_get_groupdimensions ( neffds, grpnam, grpdef
                                   , grpdms ) == 0 )
        {
            VsAtr att;

            /* collect the attributes */
            SM_get_groupattribs_from_file ( neffds, grpnam, &att );

            grpinf = GR_add_group_to_chain ( grpinf, grpnam, grpdef
                                           , grpdms, &att );

            /* already make the structures for the definitions
               and fill them with the names, the rest is done later */

            definf = DF_add_definition_to_tree ( definf, grpdef );
        }
        error = Inqnxt ( &neffds, grpnam, grpdef );
    }

/*
 *  get the other info from the file and make the structures
 *  only if Inqnxt returns a negative error signal
 *  Negative error signal means that the search process of Inqnxt
 *  has ended
 */

    if ( error <= 0 )
    {
      SM_read_definition_info ( neffds, grpnam, definf );
      SM_read_cell_info       ( neffds, celinf );
      SM_read_element_info    ( neffds, elminf );
    }
    return;

}
/*  @@ Print info of a group pointed at by p.
 */
BVoid PR_print_group_info (
    FILE      * outfile,
    VsGrpData   p
    )
{
    BInt4     i=0;
    BInt4     j=0;
    VsDefData p_def;
    VsCelData p_cel;
    VsElmData p_elm;

    if ( p != NULL )
    {
        FILE * tfp;

        tfp = outfile;
        if ( outfile == NULL )
        {
            tfp = GEN_get_output_stream ();
        }

        /* there are still groups, so find their defntn & cell info. */
        p_def = DF_find_definition_in_tree ( definf, p->grpdef );
        p_cel = CL_find_cell_in_tree ( celinf, p_def->celnam );

        GEN_print ( tfp, "Groupname:%s Dimensions:(",
                     p->grpnam );

        for (i = 0 ; i < p_def->grpndm-1 ; i++) {
            GEN_print ( tfp,"%ld,", p->grpdms[i] );
        }
        GEN_print ( tfp,"%ld)\n", p->grpdms[p_def->grpndm-1] );

        /* print the attributes */
        if ( p->nstrat + p->nreaat + p->nintat )
        {
            GEN_print ( tfp, "  Attributes:\n" );
            for ( i = 0 ; i < p->nintat ; i++ )
            {
                GEN_print ( tfp, "  %24s=%14ld\n", p->IntAttrName[i],
                        p->IntAttrValue[i] );
            }

            for ( i = 0 ; i < p->nreaat ; i++ )
            {
                GEN_print ( tfp, "  %24s=%14.7f\n", p->RealAttrName[i],
                        p->RealAttrValue[i] );
            }

            for ( i = 0 ; i < p->nstrat ; i++ )
            {
                    GEN_print ( tfp, "  %24s=%s\n", p->StringAttrName[i],
                            p->StringAttrValue[i] );
            }
        } else {
            GEN_print ( tfp,  "  No attributes\n" );
        }

        /*** Print the elements and the elementinfo ***/
        for ( i = 0 ; i < p_cel->nelems; i++)
        {
            p_elm = EL_find_element_in_tree (
                    elminf, p_cel->elmnms[i] );
            GEN_print ( tfp, "    %s %s*%3ld %s %s ",
                    p_cel->elmnms[i], p_elm->elmtyp, p_elm->nbytsg,
                    p_elm->elmqty, p_elm->elmunt );
            GEN_print ( tfp, "( ");
            for (j=0 ; j < p_elm->elmndm; j++)
            {
                GEN_print ( tfp, "%ld ", p_elm->elmdms[j] );
            }
            GEN_print ( tfp, ")\n%72s\n", p_elm->elmdes );
        }
        GEN_print ( tfp,"\n" );
        if ( outfile == NULL )
        {
            GEN_close_output_stream ();
        }
    }
    else
    {
        (BVoid)GEN_message_to_errorfile ( 10 );
    }
}


/*  @@  print all group information from chain of group-structures,
        pointed at by p.
*/
BVoid PR_groups_print ( FILE * outfile )
{
    VsGrpData   tmp;
    FILE      * tfp;

    tfp = outfile;
    if ( tfp == NULL ) {
        tfp = GEN_get_output_stream ();
    }

    tmp = grpinf;

    while ( tmp ) {
        (BVoid) PR_print_group_info ( tfp, tmp );
        tmp = tmp->next;
    }

    if ( outfile != NULL ) {
        GEN_close_output_stream ();
    }
}



/*  @@  print a branch of cells.
 */
static BVoid PR_cells_print ( VsCelData p )
{
    FILE * tfp;

    tfp = GEN_get_output_stream ();

    if ( p != NULL )
    {

        /* first traverse the left side */
        PR_cells_print ( p->left );
        GEN_print ( tfp, "Celname %s %ld %s \n",
                  p->celnam, p->nelems, p->elmnms[0] );

        /* then traverse the right side */
        PR_cells_print ( p->right );
    }
}
