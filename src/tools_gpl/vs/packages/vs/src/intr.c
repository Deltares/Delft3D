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

        Viewer/Selector Average, Mimimum and Maximum
        functions.
        --------------------------------------------
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
/*
#include <limits.h>
*/

#include "au.h"

#define AVGFUNC(typeid, type) \
    static BInt2 avg ## typeid ( \
        const VsVarData   p, \
        const BInt4       n, \
        const BRea8     * excl_val, \
              BRea8     * retval \
        ) \
    { \
        BInt4   i; \
        BInt4   ntot; \
        BRea8   sum; \
        type  * varpnt; \
 \
        if ( n <= 0 ) { \
            return -1; \
        } \
 \
        varpnt = (type *)p->varpnt; \
        sum = 0.0; \
        ntot = 0; \
        for ( i = 0 ; i < n ; i++ ) { \
            if (excl_val == NULL || varpnt[i] != *excl_val ) { \
                sum += varpnt[i]; \
                ntot++ ; \
            } \
        } \
 \
        if ( ntot > 0 ) { \
            *retval = sum/n; \
        } \
        else { \
            *retval = UNDEFVALUE ; \
        } \
        return 0; \
    }

#define MAXFUNC(typeid, type) \
    static BInt2 max ## typeid ( \
        const VsVarData   p, \
        const BInt4       n, \
        const BRea8     * excl_val, \
              BRea8     * retval \
        ) \
    { \
        BInt4   i; \
        BRea8   max; \
        type  * varpnt; \
 \
        if ( n <= 0 ) { \
            return -1; \
        } \
 \
        varpnt = (type *)p->varpnt; \
        for ( i = 0, max = -DBL_MAX ; i < n ; i++ ) { \
            if (excl_val == NULL || varpnt[i] != *excl_val ) { \
                max = max (max, varpnt[i]) ; \
            } \
        } \
 \
        *retval = max; \
        return 0; \
    }

#define MINFUNC(typeid, type) \
    static BInt2 min ## typeid ( \
        const VsVarData   p, \
        const BInt4       n, \
        const BRea8     * excl_val, \
              BRea8     * retval \
        ) \
    { \
        BInt4   i; \
        BRea8   min; \
        type  * varpnt; \
 \
        if ( n <= 0 ) { \
            return -1; \
        } \
 \
        varpnt = (type *)p->varpnt; \
        for ( i = 0, min = DBL_MAX ; i < n ; i++ ) { \
            if (excl_val == NULL || varpnt[i] != *excl_val ) { \
                 min = min (min,  varpnt[i]) ; \
            } \
        } \
 \
        *retval = min; \
        return 0; \
    }

/* This is a dummy function to catch intrinsics
   on chars, bools, etc. */
static BInt2 intr_error (
    const VsVarData   p1,
    const BInt4       n,
    const BRea8     * dum1,
          BRea8     * dum2
    )
{
    GEN_message_to_errorfile ( 110 ) ;
    return -1;
}

/* the following statements are used to generate code for
   functions which caculate the avarage, maximum and
   mimumum for different data types
 */

AVGFUNC( DOUBLE, BRea8 )
AVGFUNC( FLOAT,  BRea4 )
AVGFUNC( INT,    BInt4 )
AVGFUNC( SHORT,  BInt2 )

MAXFUNC( DOUBLE, BRea8 )
MAXFUNC( FLOAT,  BRea4 )
MAXFUNC( INT,    BInt4 )
MAXFUNC( SHORT,  BInt2 )

MINFUNC( DOUBLE, BRea8 )
MINFUNC( FLOAT,  BRea4 )
MINFUNC( INT,    BInt4 )
MINFUNC( SHORT,  BInt2 )

/*  @@  This function is used to perform the ViewSel intrinsic
 *      functions AVG (avarage), MAX (maximum) and MIN (minimum)
 */
void FU_intrinsic (
          BInt2   which,    /* range: INTR_TYPE (see gen.h) */
    const BText   name_in,  /* name of memory variable to use */
    const BText   name_out, /* name for memo-var to create */
          BRea8 * excl_val  /* pntr to value to exclude
                               from function */
    )
{
    /* intrfunc is a two-dimensional array with pointers
       to functions returning an int. The functions it points
       to are functions generated above */
    static BInt2 (*intrfunc[3][9])(
    const VsVarData, BInt4, const BRea8 *, BRea8 * ) =
        {
            intr_error, intr_error, avgDOUBLE, avgFLOAT, avgINT,
            avgSHORT, intr_error, intr_error, intr_error,
            intr_error, intr_error, maxDOUBLE, maxFLOAT, maxINT,
            maxSHORT, intr_error, intr_error, intr_error,
            intr_error, intr_error, minDOUBLE, minFLOAT, minINT,
            minSHORT, intr_error, intr_error, intr_error
        } ;
    static BText  elmnam[] = { "--Average--", "--Maximum--",
                               "--Minimum--" } ;
    BInt2     retcod;
    BRea8     retval;
    VsVarData pa;
    VsVarData pn;

    /* first check if variable name_in  exists */
    if (( pa = VR_get_pointer_to_variable ( name_in )) == NULL ) {
        GEN_message_to_errorfile ( 101, name_in );
        return;
    }

    /* check if variable name_out not already exists */
    if ( VR_get_pointer_to_variable ( name_out ) != NULL ) {
        GEN_message_to_errorfile ( 9 );
        return;
    }

    retcod = (*intrfunc[which][VR_get_data_type(pa)])(
                    pa, PP_calc_number_of_variables( pa ),
                    excl_val, &retval );

    if ( retcod == 0 ) {
        if (( pn = VR__alloc_memory_for_variable ()) != NULL ) {
            (void)strcpy ( pn->varnam, name_out );
            (void)strcpy ( pn->elmtyp, "REAL" );
            (void)strcpy ( pn->grpnam, pa->grpnam );
            (void)strcpy ( pn->elmnam, elmnam[which] );
            pn->grpndm = 1;
            pn->elmndm = 1;
            pn->grpdms[0] = 1;
            pn->elmdms[0] = 1;

            /* allocate memory for variable */
            if ( VR_get_data_type(pa) == DOUBLE ) {
                pn->nbytsg = 8;
                pn->varpnt = (void *)GEN_malloc (sizeof ( double ));

            }
            else {
                /* always a float */
                pn->nbytsg = 4;
                pn->varpnt = (void *)GEN_malloc (sizeof ( float ));
            }

            if ( pn->varpnt != NULL ) {
                if ( VR_get_data_type(pa) == DOUBLE ) {
                    ((double *)pn->varpnt)[0] = retval;
                }
                else {
                    ((float *)pn->varpnt)[0] = (float)retval;
                }
                VR_add_variable_to_memo ( pn ) ;
            }
            else {
                GEN_message_to_errorfile ( 1 );
                GEN_free ( pn );
            }
        }
    }
}
