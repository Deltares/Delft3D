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

        Viewer/Selector write functions
        -------------------------------
 */

#include <stdio.h>
#include <string.h>

#include "au.h"

static BInt2  Fg_to_file;   /* Output to file(True) or screen (False) */
static BInt2  Fg_ncp;       /* number of coulmns per row */
static FILE * Fg_file;      /* output file */
static BChar  Fg_blcknm[5];

#define PRINTSIMPL(typeid, type) \
    static BVoid print ## typeid ( \
        const BText     format, \
        const VsVarData p, \
        const BInt4     offset \
        ) \
    { \
        GEN_print ( Fg_file, format, ((type *)p->varpnt)[offset] ) ; \
    }

#define PRINTCOMPL(typeid, type) \
    static BVoid print ## typeid ( \
        const BText     format, \
        const VsVarData p, \
        const BInt4     offset \
        ) \
    { \
        GEN_print ( Fg_file, format, ((type *)p->varpnt)[offset*2], \
                                         ((type *)p->varpnt)[offset*2+1] ); \
    }

#define PRINTCHAR(typeid, type) \
    static BVoid print ## typeid ( \
        const BText     format, \
        const VsVarData p, \
        const BInt4     offset \
        ) \
    { \
        BChar form[20]; \
        if (Fg_to_file) { \
            (BVoid)sprintf ( form , "%%0.%ds ", p->nbytsg ) ; \
        } \
        else { \
            (BVoid)sprintf ( form , "%%14.%ds ", min(p->nbytsg, 14 ) ) ; \
        } \
        GEN_print ( Fg_file, form, \
                (type *)p->varpnt+offset*p->nbytsg ); \
    }

/* the next statements are used to generate the
   print functions for different types of data
*/
PRINTCOMPL ( DBLCMPL , BRea8 )
PRINTCOMPL ( COMPLEX , BRea4 )
PRINTSIMPL ( DOUBLE  , BRea8 )
PRINTSIMPL ( FLOAT   , BRea4 )
PRINTSIMPL ( INT     , BInt4 )
PRINTSIMPL ( SHORT   , BInt2 )
PRINTSIMPL ( BOOL    , BInt4 )
PRINTSIMPL ( BOOLSHRT, BInt4 )
PRINTCHAR  ( CHAR    , BChar )

/* Fg_prfunc is an aray with pointers to the printfunctions,
   generated above
*/
static BVoid (* Fg_prfunc[])
        ( const BText, const VsVarData, const BInt4) = {
            printDBLCMPL, printCOMPLEX, printDOUBLE, printFLOAT,
            printINT,     printSHORT  , printBOOL  , printBOOLSHRT,
            printCHAR
          } ;

/* Fg_format is an array with pointers to formats to be used
   in the printfunctions. printCHAR has an emtpy format, this
   is determined in the function self
*/
static BText Fg_format[] = {
    "(% 13.6e,% 13.6e) ", "(% 13.6e,% 13.6e) ", "% 13.6e ", "% 13.6e ",
    "% 13ld ", "% 13d ", "% 13ld ", "% 13d ", ""
    } ;

/*  @@
 */
static BVoid WR_two_dim_write (
    VsVarData p ,
    BInt4     offset,
    BInt4     type,
    BInt4     nrow,
    BInt4     ncol
    )
{
    BInt4 row;
    BInt4 col;
    BInt4 fc;

    for ( fc = 0 ; fc < ncol ; fc += Fg_ncp ) {

        /* print column numbers */
        if ( ! Fg_to_file ) { /* Only if output to screen */
            GEN_print ( Fg_file, "\n**  COLUMN: " );

            for ( col = fc ; col < min(ncol, fc+Fg_ncp) ; col ++ ) {
                GEN_print ( Fg_file, "%14d ", col+1 );
            }

            GEN_print ( Fg_file, "\n**     ROW\n" );
        }

        for ( row = 0 ; row < nrow ; row ++ ) {

            if ( ! Fg_to_file ) {
                GEN_print ( Fg_file, "%10d  ", row+1 );
            }

            for ( col = fc ; col < min(ncol, fc+Fg_ncp) ; col ++ ) {
                (*Fg_prfunc[type])( Fg_format[type], p, offset+nrow*col+row );
            }

            GEN_print ( Fg_file, "\n" );
        }
    }
}


/*  @@
 */
static BVoid WR_tre_dim_write (
    VsVarData p,
    NfDtp     type,
    BInt4     nrow,
    BInt4     ncol,
    BInt4     nplane
    )
{
    BInt4 plane;

    GEN_print ( Fg_file, "**%8s %10s %10s\n", "NROW", "NCOL",
                             "NPLANE" );
    if ( Fg_to_file ) {

        GEN_print ( Fg_file, "%s\n", Fg_blcknm );
        GEN_print ( Fg_file, "%10d %10d %10d\n", nrow*nplane, ncol,
                        nplane );
    }
    else {
        GEN_print ( Fg_file, "%10d %10d %10d\n", nrow, ncol,
                        nplane );
    }
    for ( plane = 0 ; plane < nplane ; plane ++ ) {

        if ( ! Fg_to_file ) {
            GEN_print ( Fg_file, "\n**   PLANE: %d\n", plane+1 );
        }

        WR_two_dim_write ( p, plane*nrow*ncol, type, nrow, ncol );
    }
}


/*  @@
 */
static BVoid WR_make_write_dimensions (
    VsVarData   p,
    BInt2     * ndim,
    BInt4     * nrow,
    BInt4     * ncol,
    BInt4     * npla
    )
{
    BInt4 dim[MAXNUMBEROFVARS];
    BInt4 ndims;
    BInt2 i;

    /* collect all the dimensions */

    dim[0] = 1;
    ndims  = 0;
    for ( i = 0 ; i < p->elmndm ; i ++ ) {
        if ( p->elmdms[i] > 1 ) {
            dim[ndims] = (int)(p->elmdms[i]);
            ndims ++;
        }
    }
    for ( i = 0 ; i < p->grpndm ; i ++ ) {
        if ( p->grpdms[i] > 1 ) {
            dim[ndims] = (int)(p->grpdms[i]);
            ndims ++;
        }
    }
    /* first examine the element dimensions */

    switch ( ndims ) {
        case 0:
        case 1:
            *nrow = dim[0];
            *ncol = 1;
            *npla = 1;
            *ndim = 1;
            break;

        case 2:
            *nrow = dim[0];
            *ncol = dim[1];
            *npla = 1;
            *ndim = 2;
            break;

        default:
            for ( i = 0, *nrow = 1 ; i < ndims - 2 ; i ++ ) {
                *nrow *= dim[i];
            }
            *ncol = dim[ndims - 2];
            *npla = dim[ndims - 1];
            *ndim = 3;
            break;
    }
}


/*  @@
 */
 static BVoid WR_write_var_info ( VsVarData p )
{
    GEN_print ( Fg_file, "** ");
    (BVoid)PR_print_variable_info ( Fg_file, p );
}


/*  @@
 */
static BVoid WR_write_one_variable ( BText varname )
{
    BInt4     nrow;
    BInt4     ncol;
    BInt4     npla;
    BInt2     ndim;
    VsVarData p;

    p = VR_get_pointer_to_variable ( varname );

    /* get the maximum three dimensions */
    WR_make_write_dimensions ( p, &ndim, &nrow, &ncol, &npla );

    /* print the header of this variable */
    WR_write_var_info ( p );

    WR_tre_dim_write ( p, VR_get_data_type ( p ), nrow, ncol, npla);

}


/*  @@
 */
static BVoid WR_write_multi_variables ( BText varnames[] )
{
    VsVarData p[MAXNUMBEROFVARS];
    NfDtp     type[MAXNUMBEROFVARS];
    BInt4     nitems[MAXNUMBEROFVARS];
    BInt4     nvar;
    BInt4     row;
    BInt4     col;
    BInt4     fc;
    BInt4     i;

    nvar = 0;
    while ( varnames[nvar] != NULL ) {
        p[nvar]      = VR_get_pointer_to_variable ( varnames[nvar] );
        nitems[nvar] = PP_calc_number_of_variables ( p[nvar] );
        type[nvar]   = VR_get_data_type ( p[nvar] );
        nvar++;
    }

    /* check if all variables have the same number of items */
    for ( i = 1 ; i < nvar ; i++ ) {
        if ( nitems[i-1] != nitems[i] ) {
            (BVoid)GEN_message_to_errorfile ( 111 );
            return;
        }
    }

    /* print the variables */

    /* first varibale info */
    for ( col = 0 ; col < nvar ; col++ ) {
        WR_write_var_info ( p[col] );
    }


    GEN_print ( Fg_file, "**%8s %10s\n", "NROW", "NCOL" );
    if ( Fg_to_file ) {
        GEN_print ( Fg_file, "%s\n", Fg_blcknm );
    }
    GEN_print ( Fg_file, "%10d %10d\n", nitems[0], nvar );

    for ( fc = 0 ; fc < nvar ; fc += Fg_ncp ) {

        if ( ! Fg_to_file ) {
            /* print column numbers */
            GEN_print ( Fg_file, "\n**  COLUMN: " );
            for ( col = fc ; col < min(nvar, fc+Fg_ncp) ; col ++ ) {
                GEN_print ( Fg_file, "%14d ", col+1 );
            }
            GEN_print ( Fg_file, "\n**     ROW\n" );
        }

        for ( row = 0 ; row < nitems[0] ; row++ ) {

            if ( ! Fg_to_file ) {
                GEN_print ( Fg_file, "%10d  ", row+1 );
            }
            for ( col = fc ; col < min(nvar, fc+Fg_ncp) ; col ++ ) {
                (*Fg_prfunc[type[col]])(
                        Fg_format[type[col]], p[col], row );
            }
            GEN_print ( Fg_file, "\n" );
        }
    }
}


/*  @@  Function to write contents of memory variables
 *      either to screen or ascii-file
 */
BVoid WR_write_variables (
    BText filename ,        /* file to write to, may be NULL */
    BText blockname ,       /* tekal file blockname, may be NULL */
    BText varnames[]        /* memory vars to write */
    )
{
    /* first check if variables exist (this function prints
       also the error messages */
    if ( VR_chck_existnce_of_variables ( varnames ) == 0 ) {

        /* open file to write to */
        if ( filename != NULL ) {
            if (( Fg_file = fopen ( filename, "a" )) == NULL) {
                (BVoid)GEN_message_to_errorfile ( 102, filename );
                return;
            }
            Fg_to_file = TRUE;
            Fg_ncp     = 10000;

            if ( blockname == NULL ) {
                (BVoid)strcpy ( Fg_blcknm, "XXXX" );
            }
            else {
                (BVoid)strncpy ( Fg_blcknm, blockname, 4 );
            }

        ;
        }
        else {
            /* write to screen */
            Fg_file    = GEN_get_output_stream ();
            Fg_to_file = FALSE;
            Fg_ncp     = 4;
        }

        /* check if number of variables = 1 */
        if ( varnames[1] == NULL ) {
            WR_write_one_variable ( varnames[0] );
        }
        else {
            WR_write_multi_variables ( varnames );
        }

        if ( ! Fg_to_file ) {
           GEN_close_output_stream ();
        }
        else {
            (BVoid)fclose ( Fg_file );
        }
    }
}
