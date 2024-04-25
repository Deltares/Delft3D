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

static BChar Fg_ident[] = "--ViEwSeL--";


/*  @@
 */
BInt4 PP_calc_number_of_variables ( VsVarData p )
{
    BInt4 nvariabs;
    BInt4 i;

    for ( i=0 , nvariabs=1 ; i < p->elmndm ; i++ ) {
        nvariabs *= p->elmdms[i];
    }

    for ( i=0  ; i < p->grpndm ; i++ ) {
        nvariabs *= p->grpdms[i];
    }
    return nvariabs;
}


/*  @@
 */
VsVarData PP_create_new_var (
        BText varnam,
        BText elmtyp,
        BInt4 nbytsg,
        BInt4 grpndm,
        BInt4 grpdms[],
        BInt4 elmndm,
        BInt4 elmdms[])
{
    VsVarData p;

    p = VR__alloc_memory_for_variable ();

    if ( p != NULL) {
        BInt4 nbytes ;
        BInt4 i;

        (void)strncpy ( p->varnam, varnam, 16 );
        (void)strncpy ( p->elmtyp, elmtyp,  8 );

        p->nbytsg = nbytsg;
        p->grpndm = grpndm;
        p->elmndm = elmndm;
        for ( i=0 ; i < grpndm ; i++ ) {
            p->grpdms[i] = grpdms[i];
        }
        for ( i=0 ; i < elmndm ; i++ ) {
            p->elmdms[i] = elmdms[i];
        }

        nbytes = PP_calc_number_of_variables (p) * nbytsg;

        p->varpnt = VR__alloc_memory_for_data ( nbytes );

        if (p->varpnt == NULL) {
            GEN_free (p);
            return NULL;
        }
    }
    return p;
}


/*  @@
 */
BVoid PP_write_var_to_pipe(
        FILE      * file,
        VsVarData   p )
{
    BInt4 nbytes;

    /* First of all, write a magic number,
       so the reader can recognize it as send by this function */
    (void)fwrite((void *)Fg_ident, sizeof(Fg_ident), 1, file );

    (void)fwrite((void *)p, sizeof(VsVar), 1, file );

    nbytes = PP_calc_number_of_variables ( p ) * p->nbytsg;


    (void)fwrite( (void *)(p->varpnt), nbytes, 1, file );
}



VsVarData PP_read_var_from_pipe( FILE * file )
{

    VsVarData p;
    BInt4     nitems;
    BInt4     nbytes;
    BChar     l_ident[20];

    /* First read the magic number */
    nitems = fread ((void *)l_ident, sizeof(Fg_ident), 1, file );
    if (nitems != 1) {
        return NULL;
    }

    if (strncmp (l_ident, Fg_ident, sizeof(Fg_ident)) != 0 ) {
        return NULL;
    }


    /* allocate memory for a structure VsVar */
    if ((p = VR__alloc_memory_for_variable ()) == NULL) {

        /* Not enough memory */
        return  NULL;
    }

    /* Try to read structure info from file */
    nitems = fread ((void *)p, sizeof(VsVar), 1, file);

    if (nitems != 1) {

        /* no more variables */
        GEN_free (p);
        return  NULL;
    }

    nbytes = PP_calc_number_of_variables ( p ) * p->nbytsg;

    /* allocate space for data */
    p->varpnt = VR__alloc_memory_for_data ( nbytes );

    if (p->varpnt == NULL) {

        /* not enough memory */
        GEN_free (p);
        return NULL;
    }

    nitems =fread ( (void *)(p->varpnt), nbytes, 1, file );

    if (nitems != 1) {

        /* error, not enough data ? */
        GEN_free (p->varpnt);
        GEN_free (p);
        return NULL;
    }

    return p;
}
