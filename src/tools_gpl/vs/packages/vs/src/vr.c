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

        Viewer/Selector Variable Management Functions
        ---------------------------------------------
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "au.h"

extern  BInt4 neffds;

static VsVarData varinf = NULL;


/*  @@  Allocate memory for data to read.
 */
BData VR__alloc_memory_for_data ( BInt4 nbytes )
{
    return GEN_malloc(nbytes);
}


/*  @@ Allocate memory for a variable structure.
 */
VsVarData VR__alloc_memory_for_variable ( BVoid )

{
    return (VsVarData) GEN_malloc(sizeof(VsVar));
}

/*
 */
BInt4 VR_same_structure (
    VsVarData pa,
    VsVarData pb )
{
    if ( pa->grpndm == pb->grpndm &&
         pa->elmndm == pb->elmndm ) {

        BInt4 i;

        for ( i = 0 ; i < pa->grpndm ; i ++ ) {
            if ( pa->grpdms[i] != pb->grpdms[i] ) {
                return -1;
            }
        }
        for ( i = 0 ; i < pa->elmndm ; i ++ ) {
            if ( pa->elmdms[i] != pb->elmdms[i] ) {
                return -1;
            }
        }
        return 0;
    }
    return -2;
}


/*  @@  locate memo-variable <varnam> and release it.
 *      This is a recursive function, so pointer to
 *      begin of variable-chain must be given. This pointer
 *      may be updated.
 */
static VsVarData VR_free_variable (
               VsVarData p,
        const  BText     varnam)
{
    VsVarData retval;

    if ( p != NULL ) {
        if ( GEN_string_compare(varnam, p->varnam) == 0 ) {
            GEN_free ( p->varpnt );
            retval = p->next;
            GEN_free ( p );
            return retval;
        }

        else {
            p->next = VR_free_variable( p->next, varnam);
            return p;
        }

    }
    return p;
}

/*  @@
 */
static VsVarData VR_add_var_to_chain (
    VsVarData p_var,
    VsVarData p )
{
    if (p_var == NULL) {
        p_var = p;
        p_var->next = NULL;
    }
    else {
        p_var->next = VR_add_var_to_chain ( p_var->next, p );
    }
    return p_var;
}

BVoid VR_add_variable_to_memo ( VsVarData p )
{
    varinf = VR_add_var_to_chain ( varinf, p );
}


/*  @@  Find memo-variable in chain, return pointer.
 */
static VsVarData VR_find_var_in_chain (
              VsVarData p,
        const BText     varnam )
{
    if ( p != NULL ) {
        if ( GEN_string_compare( varnam, p->varnam ) == 0 ) {
            return p ;
        }

        else {
            return( VR_find_var_in_chain( p->next, varnam ));
        }

    }
    /* not found */
    return NULL;
}


/*  @@
 */
static VsVarData VR_make_variable_struct (
        const BText     varnam,
        const BText     grpnam,
        const BText     elmnam,
              BInt4     elm_uindex[][3],
              BInt4     uindex[][3],
              BData     varpnt,
              VsDefData p_def,
              VsElmData p_elm )

{
    BInt4     i;
    BInt4     j;
    BInt4     dim;
    VsVarData p;

    p = VR__alloc_memory_for_variable();

    (BVoid)strcpy ( p->varnam, varnam );
    (BVoid)strcpy ( p->grpnam, grpnam );
    (BVoid)strcpy ( p->elmnam, elmnam );

    p->varpnt = varpnt;

    for ( i=0, j=0 ; i < p_def->grpndm ; i++ ) {
        dim = (uindex[i][1]-uindex[i][0]+uindex[i][2])/
               uindex[i][2];

        if ( dim > 1 ) {
            p->grpdms[j++] = dim;
        }
    }
    p->grpndm = max(j,1);

    for ( i=j ; i < MAX_DIM ; i++ ) {
        p->grpdms[i] = 1;
    }

    (BVoid)strcpy ( p->elmqty, p_elm->elmqty );
    (BVoid)strcpy ( p->elmunt, p_elm->elmunt );
    (BVoid)strcpy ( p->elmdes, p_elm->elmdes );
    (BVoid)strcpy ( p->elmtyp, p_elm->elmtyp );

    p->nbytsg = p_elm->nbytsg;

    for ( i=0, j=0 ; i < p_elm->elmndm ; i++ ) {
        dim = (elm_uindex[i][1]-elm_uindex[i][0]+elm_uindex[i][2])/
               elm_uindex[i][2];

        if ( dim > 1 ) {
           p->elmdms[j++] = dim;
        }
    }
    p->elmndm = max( j, 1 );

    for ( i=j ; i < 5 ; i++ ) {
        p->elmdms[i] = 1;
    }

    return p;
}


/*  @@  Store data in to the memeory area.
 */
static BVoid VR_store_data_in_buffer (
        BText     varpnt,
        BText     buffer,
        BInt4     elm_uindex[][3],
        VsElmData p4)


{
    BInt4 i;
    BInt4 l_uindex[][3] = {{0,0,1},{0,0,1},{0,0,1},{0,0,1},{0,0,1}};
    BInt4 offset[5], offs[5];

    /* calculate the offset for every dimension */
    offset[0] = 1;
    for ( i = 1 ; i < 5; i++ ) {
        offset[i] = p4->elmdms[i-1] * offset[i-1];
    }

    for (l_uindex[4][0]=l_uindex[4][1]=elm_uindex[4][0];
         l_uindex[4][0]<=elm_uindex[4][1] ;
         l_uindex[4][1]=l_uindex[4][0]+=elm_uindex[4][2]) {

        offs[4] = (l_uindex[4][0]-1)*offset[4];

        for (l_uindex[3][0]=l_uindex[3][1]=elm_uindex[3][0];
             l_uindex[3][0]<=elm_uindex[3][1] ;
             l_uindex[3][1]=l_uindex[3][0]+=elm_uindex[3][2]) {

            offs[3] = offs[4] + (l_uindex[3][0]-1)*offset[3];

            for (l_uindex[2][0]=l_uindex[2][1]=elm_uindex[2][0];
                 l_uindex[2][0]<=elm_uindex[2][1] ;
                 l_uindex[2][1]=l_uindex[2][0]+=elm_uindex[2][2]) {

                offs[2] = offs[3] + (l_uindex[2][0]-1)*offset[2];

                for (l_uindex[1][0]=l_uindex[1][1]=elm_uindex[1][0];
                     l_uindex[1][0]<=elm_uindex[1][1] ;
                     l_uindex[1][1]=l_uindex[1][0]+=elm_uindex[1][2]) {

                    offs[1] = offs[2] + (l_uindex[1][0]-1)*offset[1];

                    for (l_uindex[0][0]=l_uindex[0][1]=elm_uindex[0][0];
                         l_uindex[0][0]<=elm_uindex[0][1] ;
                         l_uindex[0][1]=l_uindex[0][0]+=elm_uindex[0][2]) {

                        offs[0] = offs[1] + (l_uindex[0][0]-1)*offset[0];

                        for ( i = 0 ; i <p4->nbytsg ; i++) {
                            *varpnt++ = buffer[offs[0]*p4->nbytsg+i];

                        }
                    }
                }
            }
        }
    }
}


/* @@
 */
static BInt4 VR_read_variable_from_file(
        const BText     varnam,
        const BText     grpnam,
        const BText     elmnam,
              BInt4     elm_uindex[][3],
              BInt4     uindex[][3],
              VsDefData p2,
              VsElmData p4,
              BData     varpnt)
{
    BInt4 error;
    BInt4 i;
    BInt4 nelems;
    BInt4 nactelems;
    BInt4 count;
    BInt4 nbytecel;
    BInt4 buflen;
    BInt4 usrord[] = { 1 , 2 , 3 , 4, 5 };
    BInt4 l_uindex[][3] = {{0,0,1},{0,0,1},{0,0,1},{0,0,1},{0,0,1}};
    BData tmp;

    VsVarData p;
/*
  DisplayMessage("Names  :\n%s\n%s\n%s",
                  varnam, grpnam, elmnam);
  DisplayMessage("group index:\n%d %d %d\n%d %d %d \n%d %d %d \n%d %d %d \n%d %d %d",
    uindex[0][0], uindex[0][1], uindex[0][2],
    uindex[1][0], uindex[1][1], uindex[1][2],
    uindex[2][0], uindex[2][1], uindex[2][2],
    uindex[3][0], uindex[3][1], uindex[3][2],
    uindex[4][0], uindex[4][1], uindex[4][2]);
  DisplayMessage( "Element index:\n%d %d %d\n%d %d %d \n%d %d %d \n%d %d %d \n%d %d %d",
    elm_uindex[0][0], elm_uindex[0][1], elm_uindex[0][2],
    elm_uindex[1][0], elm_uindex[1][1], elm_uindex[1][2],
    elm_uindex[2][0], elm_uindex[2][1], elm_uindex[2][2],
    elm_uindex[3][0], elm_uindex[3][1], elm_uindex[3][2],
    elm_uindex[4][0], elm_uindex[4][1], elm_uindex[4][2]);
  DisplayMessage("l_uindex:\n%d %d %d\n%d %d %d \n%d %d %d \n%d %d %d \n%d %d %d",
  l_uindex[0][0], l_uindex[0][1], l_uindex[0][2],
  l_uindex[1][0], l_uindex[1][1], l_uindex[1][2],
  l_uindex[2][0], l_uindex[2][1], l_uindex[2][2],
  l_uindex[3][0], l_uindex[3][1], l_uindex[3][2],
  l_uindex[4][0], l_uindex[4][1], l_uindex[4][2]);
*/
    /* Calculate # of elements in a cel */
    for (i=0, nelems=1 ; i < p4->elmndm; i++){
        nelems *= p4->elmdms[i];
    }

    /* Calculate number of bytes te be retrieved from a cel */
    for (i=0, nactelems=1 ; i < p4->elmndm; i++){
        nactelems *= (elm_uindex[i][1]-elm_uindex[i][0]+elm_uindex[i][2])/
                   elm_uindex[i][2];
    }

    nbytecel = nelems*p4->nbytsg;

    /* allocate temporary storage space */
    tmp = VR__alloc_memory_for_data ( nbytecel );

    if (tmp == NULL) {
        /* Not enough memory */
        return -1;
    }

    /* Now travel through all cells, as specified in uindex */
    count = 0;

    for (l_uindex[4][0]=l_uindex[4][1]=uindex[4][0];
         l_uindex[4][0]<=uindex[4][1] ;
         l_uindex[4][1]=l_uindex[4][0]+=uindex[4][2]) {
        for (l_uindex[3][0]=l_uindex[3][1]=uindex[3][0];
             l_uindex[3][0]<=uindex[3][1] ;
             l_uindex[3][1]=l_uindex[3][0]+=uindex[3][2]) {
            for (l_uindex[2][0]=l_uindex[2][1]=uindex[2][0];
                 l_uindex[2][0]<=uindex[2][1] ;
                 l_uindex[2][1]=l_uindex[2][0]+=uindex[2][2]) {
                for (l_uindex[1][0]=l_uindex[1][1]=uindex[1][0];
                     l_uindex[1][0]<=uindex[1][1] ;
                     l_uindex[1][1]=l_uindex[1][0]+=uindex[1][2]) {
                    for (l_uindex[0][0]=l_uindex[0][1]=uindex[0][0];
                         l_uindex[0][0]<=uindex[0][1] ;
                         l_uindex[0][1]=l_uindex[0][0]+=uindex[0][2]) {

                        /* retrieve data from one cel */
                        buflen = nbytecel;
                        error = Getelt(
                            &neffds,
                             grpnam   , elmnam   ,
                             l_uindex , usrord   ,
                            &buflen  , tmp );
                        if (error != 0) {
                            return error;
                        }

                        /* now filter data from temp, according to
                           elm_unidex, and put them in varpnt */
                    (BVoid) VR_store_data_in_buffer (
                        (BText )varpnt+ count  *nactelems*p4->nbytsg,
                        (BText )tmp, elm_uindex, p4);
                    count++;
                    }
                }
            }
        }
    }


    /* Free the temporarily storage area */
    if (tmp) GEN_free (tmp);

    p = VR_make_variable_struct ( varnam, grpnam, elmnam,
                                elm_uindex, uindex, varpnt, p2, p4);
    VR_add_variable_to_memo ( p );
    return 0;
}


/*  @@
 */
static BInt4 VR_is_element_in_group (
        const BText       elmnam,
        const BText       grpnam,
              VsDefData * p2)

{
    BInt4     error=-1;
    VsGrpData p1;

    if (( p1=GR_find_group_in_chain (
                SM_get_group_pointer(),grpnam)) != NULL ) {

        VsCelData p3;
        BInt4     i=0;

        *p2 = DF_find_definition_in_tree (
                SM_get_definition_pointer(), p1->grpdef );
        p3 = CL_find_cell_in_tree (
                SM_get_cell_pointer(), (*p2)->celnam );

        while ( error != 0 && i < p3->nelems &&
              ( error = GEN_string_compare(elmnam,p3->elmnms[i++]))!=0){
        }
    }
    return error;
}


/*  @@
 */
static BInt4 VR_calc_number_of_variables(
        BInt4 grpndm,
        BInt4 elmndm,
        BInt4 elm_uindex[][3],
        BInt4 uindex[][3])

{
    BInt4 nvariabs;
    BInt4 i;

    for ( i=0 , nvariabs=1 ; i < elmndm ; i++ ){
        nvariabs *= (elm_uindex[i][1]-elm_uindex[i][0]+elm_uindex[i][2])/
                   elm_uindex[i][2];
    }

    for ( i=0 ; i < grpndm ; i++ ){
        nvariabs *= (uindex[i][1]-uindex[i][0]+uindex[i][2])/
                   uindex[i][2];
    }
    return nvariabs;
}


/*  @@
 */
static BInt4 VR_adjust_suppl_grp_indices(
        VsGrpData p1,
        VsDefData p2,
        BInt4     uindex[][3])
{
    BInt4 i;

    for ( i=0 ; i < p2->grpndm ; i++ ) {

        /* check start index first */
        if ( uindex[i][0] < 1 ) {
            uindex[i][0]= 1;
        }

        /* check end index */
        if ( uindex[i][1] < 1 ) {
            uindex[i][1]= p1->grpdms[i];
        }

        /* check step */
        if ( uindex[i][2] < 1 ) {
            uindex[i][2]= 1;
        }
        if (uindex[i][0] > p1->grpdms[i] ) {
            (BVoid)GEN_message_to_errorfile ( 13 );
            return 1;
        }
        if (uindex[i][1] > p1->grpdms[i] ) {
            (BVoid)GEN_message_to_errorfile ( 13 );
            return 1;
        }
        if (uindex[i][0] > uindex[i][1] ) {
            (BVoid)GEN_message_to_errorfile ( 13 );
            return 1;
        }
    }

    /* make extra indices eq. 1 */
    for ( i = p2->grpndm ; i < MAX_DIM ; i++ ) {
        uindex[i][0] = uindex[i][1] = uindex[i][2] = 1;
    }
    return 0;
}


/*  @@
 */
static BInt4 VR_adjust_suppl_elm_indices (
        VsElmData p4,
        BInt4     elm_uindex[][3])
{
    BInt4 i;

    for ( i=0 ; i < p4->elmndm; i++ ) {

         if ( elm_uindex[i][0] < 1 ) {
             elm_uindex[i][0] = 1 ;
         }
         if ( elm_uindex[i][1] < 1 ) {
             elm_uindex[i][1] = p4->elmdms[i];
         }
         if ( elm_uindex[i][2] < 1 ) {
             elm_uindex[i][2] = 1 ;
         }

        /* start index must be <= elmdms */
        if ( elm_uindex[i][0] > p4->elmdms[i] ) {
            (BVoid)GEN_message_to_errorfile (12);

            return 1;
        }

        /* stop index must be <= elmdms */
        if ( elm_uindex[i][1] > p4->elmdms[i] ) {
            (BVoid)GEN_message_to_errorfile (12);

            return 1;
        }

        /* check if start index .lt. end index */
        if ( elm_uindex[i][0] > elm_uindex[i][1] ) {
            (BVoid)GEN_message_to_errorfile (12);

            return 1;
        }
    }
    /* make extra indices eq. 1 */
    for ( i = p4->elmndm ; i < MAX_DIM; i++ ) {
        elm_uindex[i][0] = elm_uindex[i][1] = elm_uindex[i][2] = 1;
    }
    return 0;
}


/*  @@
 */
BVoid PR_print_variable_info (
        FILE      * tfp,
        VsVarData   p)

{
    BInt4 i;

    GEN_print (tfp,"VARIABLE %s from %s(", p->varnam, p->grpnam);

    for ( i = 0 ; i < p->grpndm-1 ; i++ ) {
        GEN_print ( tfp, "%ld,", p->grpdms[i] );
    }
    GEN_print ( tfp,"%ld)", p->grpdms[p->grpndm-1] );

    GEN_print ( tfp, " ELEMENT %s(", p->elmnam );

    for ( i = 0 ; i <p->elmndm-1 ; i++ ) {
       GEN_print(tfp,"%d,",p->elmdms[i]);
    }
    GEN_print ( tfp, "%ld)", p->elmdms[p->elmndm-1] );

    GEN_print ( tfp, " %s *%ld\n", p->elmtyp, p->nbytsg );

}


/*  @@
 */
BInt4 VR_read_var_from_file (
        const BText varnam,
        const BText elmnam,
        const BText grpnam,
              BInt4 elm_uindex[][3],
              BInt4 uindex[][3])

{
    BInt4     error = -1;
    VsDefData p2    = NULL;

    /* check if variable already exists */
    if (VR_find_var_in_chain(varinf, varnam) != 0) {
        (BVoid)GEN_message_to_errorfile (9);
        return error;
    }

    /* first check if element exists in this group */
    if (VR_is_element_in_group(elmnam, grpnam,&p2) == 0) {

        BData     varpnt = NULL;
        VsGrpData p1     = NULL;
        VsElmData p4     = NULL;
        BInt4     nbytes = -1;

        p1 = GR_find_group_in_chain (
                SM_get_group_pointer(), grpnam );

        p4 = EL_find_element_in_tree (
                SM_get_element_pointer(),elmnam);

        /* adjust user supplied indexes */
        if (VR_adjust_suppl_elm_indices(p4, elm_uindex)) {
            return error;
        }

        if (VR_adjust_suppl_grp_indices(p1, p2, uindex)) {
            return error;
        }


        /* calculate storage length */
        nbytes = VR_calc_number_of_variables(
                             p2->grpndm, p4->elmndm, elm_uindex, uindex)
                         * p4->nbytsg;
        varpnt = VR__alloc_memory_for_data ( nbytes );

        if ( varpnt != NULL )
        {

            if ((error = VR_read_variable_from_file(
                          varnam, grpnam, elmnam,
                          elm_uindex, uindex, p2, p4,
                          varpnt)) == 0) {
                GEN_mallinfo ();
            }

            else {
                /* something went wrong will reading data */
                if (varpnt) {
                    GEN_free(varpnt);
                }
                (BVoid)GEN_message_to_errorfile(8,error);
            }
        }

        else {
            (BVoid)GEN_message_to_errorfile(1);
        }

    }

    else {
        (BVoid)GEN_message_to_errorfile(6, elmnam);
    }
    return error;
}


/*  @@
 */
BVoid PR_variables_info_print ( BVoid )

{
    VsVarData   p;
    FILE      * tfp;

    tfp = GEN_get_output_stream();

    p = varinf;
    if (p) {
        while (p) {
            (BVoid) PR_print_variable_info( tfp, p );
            p = p->next;
        }
    }
    GEN_close_output_stream();
}


/*  @@
 */
BVoid VR_release_variable ( const BText varname )
{
    varinf = VR_free_variable ( varinf, varname );
}


/*  @@ This function will release all memory variables
 *     and updates the pointer to begin of chain (varinf)
 */
BVoid VR_release_all_variables ( BVoid )
{
    while ( varinf != NULL )
    {
        varinf = VR_free_variable ( varinf, varinf->varnam );
    }
}


/* @@
 */
BInt4 VR_chck_existnce_of_variables (
        BText  varnam[] )
{
    if ( varnam != NULL ) {
        BInt4 i = 0;
        while ( varnam[i] != NULL ) {
            if ( VR_find_var_in_chain(varinf, varnam[i]) == NULL ) {
                GEN_message_to_errorfile( 101, varnam[i] );
              return -1;
            }
            i++;
        }
    }
    return 0;
}

VsVarData VR_get_pointer_to_variable ( const BText varnam )
{
    return VR_find_var_in_chain( varinf, varnam );
}

/*  @@
 */
NfDtp VR_get_data_type ( VsVarData p )
{
    NfDtp type;

    type = UNDEF;
    if( strncmp( p->elmtyp, "COMPLEX", 7) == 0 ) {
        type = DBLCMPL;
        if(p->nbytsg == 8) {
            type = COMPLEX;
        }
    }
    else if (strncmp(p->elmtyp, "REAL", 4) == 0) {
        type = FLOAT;
        if(p->nbytsg == 8) {
            type = DOUBLE;
        }
    }
    else if (strncmp(p->elmtyp, "INTEGER", 7) == 0 ) {
        type = SHORT;
        if (p->nbytsg == 4) {
            type = INT;
        }
    }
    else if( strncmp( p->elmtyp, "LOGICAL", 7) == 0 ) {
        type = BOOL;
        if(p->nbytsg == 2) {
            type = BOOLSHRT;
        }
    }
    else if( strncmp( p->elmtyp, "CHARACTE", 8) == 0 ) {
        type = CHAR;
    }
    return type;
}
