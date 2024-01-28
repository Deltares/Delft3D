//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
/* @begin@ */
/*
 *  tekasc.c  -  ODS functions for standard TEKAL ASCII 1d and 2d files
 *
 *  Pleun Koole
 */

/*
 *  $Author: Markus $
 *  $Date: 8-07-03 10:43 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/tekasc.c,v $
 *
 */

 /* General information:
  * For detail design .../gpp/doc/memos/document s-do-001.0pk
  * This file contains the following functions:
  *     -        TVoid skip_tekal_record
  *     -        TVoid skip_tekal_blocname_record
  *     - static TVoid read_tekal_dimension_record
  *     -        TVoid skip_tekal_comment_records
  *     -        TVoid skip_tekal_values_records
  *     -        TVoid skip_tekal_block
  *     -        TVoid extract_idx_name
  *     -        TVoid ODSGetParAnyTekAscHis
  *     -        TVoid ODSGetDimAnyTekAscHis
  *     -        TVoid ODSGetTmeAnyTekAscHis
  *     -        TVoid ODSGetLocAnyTekAscHis
  *     -        TVoid ODSGetMatAnyTekAscHis
  *     -        Tvoid ODSGetGrdAnyTekAscHis
  *     NOTE:
  *     In this file the counting is not very consistent.
  *     Sometimes the do-loops starts with 1, sometimes with 0
  *     For clarity: this must be changed!
  */
/* @end@ */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

/* #include "portable.h" */
#include "ods.h"
#include "odsmodel.h"

/* #include "utils.h"
#include "itrans.h"
#include "julian.h"
#include "opnclose.h" */

#ifndef FALSE
#define FALSE   0
#define TRUE    1
#endif


/* @@---------------------------------------------------
    Function:    skip_tekal_record
    Author:      Pleun Koole
    Purpose:     skip rest of record
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/

 TVoid skip_tekal_record ( FILE *iunit)
{

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    while ( ( getc ( iunit) ) != '\n' )
    {
        if ( ferror ( iunit) || feof ( iunit) )
        {
            return ;
        }
    }

    return ;

} /* End of skip_tekal_record */


/* @@---------------------------------------------------
    Function:    skip_tekal_blocname_record
    Author:      Pleun Koole
    Purpose:     skip tekal blocname record
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/

 TVoid skip_tekal_blocname_record ( FILE *iunit)
{
    skip_tekal_record ( iunit);

    return ;

} /* End of skip_tekal_blocname_record */


/* @@---------------------------------------------------
    Function:    read_tekal_dimension_record
    Author:      Pleun Koole
    Purpose:     read tekal dimension record
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/

static TVoid read_tekal_dimension_record ( FILE *iunit, TInt4 *nrows, TInt4 *ncols)
{

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    *nrows = 0 ;
    *ncols = 0 ;

    /* read number of rows and number of columns */
    fscanf ( iunit, "%ld%*[ ,\t]%ld", nrows, ncols ) ;
    /* skip rest of record */
    skip_tekal_record ( iunit) ;

    return ;

} /* End of read_tekal_dimension_record */


/* @@---------------------------------------------------
    Function:    skip_tekal_comment_records
    Author:      Pleun Koole
    Purpose:     skip tekal comment records
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/

 TVoid skip_tekal_comment_records ( FILE *iunit)
{
    TInt4   c ;

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    while ( ( ( c = getc ( iunit) ) == '*' ) && ( ! ferror ( iunit) ) )
    {
        skip_tekal_record ( iunit);

        if ( ferror ( iunit) || feof ( iunit) )
        {
            return ;
        }
    }

    if ( ferror ( iunit) )
    {
        return ;
    }
    else
    {
        ungetc ( c, iunit ); /* put back first character of blockname */
    }

    return ;

} /* End of skip_tekal_comment_records */


/* @@---------------------------------------------------
    Function:    skip_tekal_values_records
    Author:      Pleun Koole
    Purpose:     skip tekal values records
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/

 TVoid skip_tekal_values_records ( FILE *iunit, TInt4 nrows )
{
    TInt4   i , pos , length;
    TChar   c               ;

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    for (i=1L; i<=nrows; i++)
    {
        skip_tekal_record ( iunit);

        if ( ferror ( iunit) || feof ( iunit) )
        {
            return ;
        }
    }

/* Now take care of empty records and records where the only
   character is a ^Z.
   The records that are valid have at least one non-blank
   character within 20 columns.
   We need to be very careful!

   The original code fails on Windows if you work with UNIX files.
   Windows files on UNIX are another problem, but not as severe,
   I think.
*/
    while( !ferror( iunit ) && !feof( iunit ) )
    {
/* ----
       pos = ftell( iunit ) ;
       fgets( string , sizeof( string ) , iunit ) ;
       length = strlen( string ) ;
       for ( i = 0 ; i < length ; i ++ )
       {
          if ( string[i] != ' '  && string[i] != '\n' &&
               string[i] != '\t' && string[i] != (char) 26 )
          {
             fseek( iunit , pos , SEEK_SET ) ;
             return ;
          }
       }
       if ( string[length-1] != '\n' )
       {
          skip_tekal_record( iunit ) ;
       }
---- */

      c = fgetc( iunit ) ;
      if ( c != ' '  && c != '\n' && c != '\r' &&
           c != '\t' && c != (char) 26 )
      {
         break ;
      }
    }
    ungetc( c, iunit ) ;

    return ;

} /* End of skip_tekal_values_records */


/* @@---------------------------------------------------
    Function:    skip_tekal_block
    Author:      Pleun Koole
    Purpose:     skip tekal block
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code: skip_tekal_comment_records
       skip_tekal_blocname_record
       read_tekal_dimension_record
                 skip_tekal_values_records

  ------------------------------------------------------*/

 TVoid skip_tekal_block ( FILE *iunit)
{
    TInt4   nrows, ncols ;

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    skip_tekal_comment_records ( iunit) ;
    skip_tekal_blocname_record ( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
    skip_tekal_values_records( iunit, nrows ) ;

    return ;

} /* End of skip_tekal_block */

/* @@---------------------------------------------------
    Function:    extract_idx_name
    Author:      Arjen Markus
    Purpose:     Extract the index/name from a comment line
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:
                 Read a line, extract the required
                 information. Read on the next line
  ------------------------------------------------------*/

 TVoid extract_idx_name ( FILE *iunit , TInt4 *idx, TString name )
{
    TChar   line[80]    ;
    TChar   keyword[20] ;
    TString pstr        ;

    *idx = -1 ;

    fgets( line, sizeof(line), iunit ) ;

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    sscanf( line+1, "%10s %d", keyword, idx ) ;

    if ( strcmp( keyword, "column" ) == 0 ||
         strcmp( keyword, "Column" ) == 0 ||
         strcmp( keyword, "COLUMN" ) == 0   )
    {
       pstr = strchr( line, ':' ) ;
       if ( pstr != NULL )
       {
          sscanf( pstr+1, "%*[ ]%19[A-Za-z_0-9- .()+%]", name ) ;
       }
       else
       {
          *idx = -1 ;
       }
    }
    else
    {
       *idx = -1 ;
    }

    if ( strchr( line, '\n' ) == NULL )
    {
       skip_tekal_record ( iunit) ;
    }
}

/* @@---------------------------------------------------
    Function:    ODSGetParAnyTekAscHis
    Author:      Pleun Koole
    Purpose:     generate parameterlist
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetParAnyTekAscHis (
                     TString fname,
                     TInt4   *ftype,
                     TString pardef,
                     TInt4   maxdef,
                     TInt4   timdep,
                     TInt4   locdep,
                     TInt4   maxlst,
                     TInt4   lang,
                     TString parlst,
                     TString paruni,
                     TInt4   *partyp,
                     TInt4   *parcod,
                     TInt4   *nrlst,
                     TInt4   *ierror,
                     TString option)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Parameters from TEKAL ASCII file                     */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  -------------------------------          */
/*    fname   -          I   Full filename, including extension       */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    ierror  -          O   Errorcode. See ods.h for definition.     */
/*    lang    -          I   language code                            */
/*    locdep  -          I   parameter dependency for location        */
/*    maxdef  -          I   Number of parameter filters.             */
/*    maxlst  -          I   Number of parameters to expected         */
/*    nrlst   -          O   Nr of parameters returned.               */
/*    option  -        I/O   option not used                          */
/*    pardef  maxdef     I   List of parameter filters.               */
/*    parcod  maxlst     O   List of parameter codes                  */
/*    parlst  maxlst     O   List of parameters found.                */
/*    partyp  maxlst     O   List of types of parameters found.       */
/*    paruni  maxlst     O   List of units of parameters found.       */
/*    timdep  -          I   parameter dependency for time            */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    FILE    *iunit ;
    TChar   *pstr ;
    TChar   parnam [ 21] ;
    TInt4   i, nrows, ncols, ldum ;

    *ierror = IEOK ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
        if ( *ierror != IEOK )
        {
            return;
        }

    /* loop over number of tekal blocks in file */
    *nrlst = 0 ;
    while ( ! feof ( iunit ) )
    {
        skip_tekal_comment_records( iunit ) ;
            if ( feof ( iunit) )
            {
                break ;
            }
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return;
            }

        /* read extended tekal blockname */
        if ( fgets ( parnam, sizeof( parnam ), iunit) == NULL )
        {
            if ( feof ( iunit) )
            {
                break ;
            }
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
        }

        /* strip of '\n', fill up with ' ', add '\0' */
        pstr = strchr( parnam, '\n' ) ;
        if ( pstr != NULL )
        {
           *pstr = '\0' ;
        }
        else
        {
           skip_tekal_record( iunit) ;
           *(parnam + 20 ) = '\0' ;
        }

        if ( *nrlst < maxlst )
        {
            strcpy ( parlst + 21 * (*nrlst) , parnam ) ;
            (*nrlst)++ ;
        }
        else
        {
            /* too much parameters for array space, return error */
            *ierror = IEPMNY ;
            CLOSFL ( fname, &ldum) ;
            return ;
        }

        read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
        skip_tekal_values_records( iunit, nrows ) ;
/*
            if ( feof ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return;
            }
*/
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return;
            }
    } /* end while */

    /* set parameter type, unit and indexcodes */
    for (i=0L; i<*nrlst; i++)
    {
        strcpy ( paruni + 21 * ( i ) , "                    " ) ;
        *(partyp+i) = ODS_PT_LOC_DEP + ODS_PT_LOC_LIST ;
        *(parcod+i) = i + 1 ;
    } /* end for */

    /* add parameter for 1-D modelgrid;
       this parameter has a default name - QX_AXIS and the
       parcode is 9999 */

    if ( *nrlst < maxlst )
    {
        strcpy ( parlst + 21 * (*nrlst) , "QX_AXIS" ) ;
        parcod[*nrlst] = 9999;
        (*nrlst)++ ;
    }
    else
    {
        /* too much parameters for array space, return error */
        *ierror = IEPMNY ;
        CLOSFL ( fname, &ldum) ;
        return ;
    }

    CLOSFL ( fname, ierror) ;
    return ;

} /* End of ODSGetParAnyTekAscHis */

/* @@---------------------------------------------------
    Function:    ODSGetDimAnyTekAscHis
    Author:      Pleun Koole
    Purpose:     generate parameterlist
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetDimAnyTekAscHis (
                     TString fname,
                     TInt4   *ftype,
                     TString dimtyp,
                     TInt4   pardep,
                     TInt4   timdep,
                     TInt4   locdep,
                     TInt4   *ndim,
                     TInt4   *ierror,
                     TString option)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Dimensions from TEKAL ASCII file                     */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    dimtyp  -          I   Dimension wanted (loc, par, tim)         */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    ierror  -          O   Errorcode. See ods.h for definitions.    */
/*    locdep  -          I   location  dependency type                */
/*    ndim    4          O   dimension. [0]=# dim [1]=x [2]=y [3]=z   */
/*    option  -        I/O   option not used                          */
/*    pardep  -          I   parameter dependency type                */
/*    timdep  -          I   time      dependency type                */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    FILE    *iunit ;
    TInt4   i, ncols, nrows , nblocs , block, selected_col;
    TInt4   ldum;
    TChar * colstr;

    nblocs = 0 ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    switch (dimtyp[0])
    {
    case 'p':  /* par */
    case 'P':
        /* loop over number of tekal blocks in file */
        /* Note:
           There is a problem if the last line does not end in a
           new line - IEPMNY in getpar()
           This should be solved!
        */
        while ( ! feof ( iunit ) )
        {
            skip_tekal_comment_records( iunit ) ;
            skip_tekal_blocname_record( iunit) ;
                if ( feof   ( iunit) )
                {
                    break ;
                }
            read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
            skip_tekal_values_records( iunit, nrows ) ;
/*
                if ( feof ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
*/
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return;
                }

            nblocs++;
        } /* end while */
        ndim[0] = (TInt4) 1;
        ndim[1] = nblocs; /* number of blocs in file fname */
        ndim[1] = ndim[1]+1; /* add one for 1-D gridparameter */
        break;

    case 'l':  /* loc */
    case 'L':
        block = -1 ; /* HACK: do not return columns for 1-D grid */
        if ( pardep < 0 || pardep == 9999 )
        {
            /* HACK: dimension of 1-D grid is assumed, block and column nr is
               in third filename */
            colstr = &fname[ODS_FILNAMLEN+ODS_FILNAMLEN];
            sscanf( colstr, "Grid:block-%d_column-%d", &block, &selected_col );
            pardep = block;
        }

        /* skip pardep blocks of tekal file, keep last nrows and ncols */
        for (i=1L; i<=pardep; i++)
        {
            skip_tekal_comment_records( iunit ) ;
                if ( feof ( iunit) )
                {
                    break ;
                }
            skip_tekal_blocname_record( iunit) ;
            read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
            skip_tekal_values_records( iunit, nrows ) ;
/*
                if ( feof ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
*/
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
        } /* end for */
        ndim[0] = (TInt4) 4;
        ndim[1] = 1 ;
        if ( block == -1 )
        {
           ndim[1] = ncols; /* number of columns in block pardep */
        }
        ndim[2] = 1;
        ndim[3] = nrows;
        ndim[4] = nrows;
        break;

    case 't':  /* tim */
    case 'T':

        /* skip pardep blocks of tekal file, keep last nrows and ncols */
        for (i=1L; i<=pardep; i++)
        {
            skip_tekal_comment_records( iunit ) ;
                if ( feof ( iunit) )
                {
                    break ;
                }
            skip_tekal_blocname_record( iunit) ;
            read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
            skip_tekal_values_records( iunit, nrows ) ;
/*
                if ( feof ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
*/
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
        } /* end for */
        ndim[0] = (TInt4) 1;
        ndim[1] = nrows; /* number of rows in block pardep */
        break;

    default:
        *ierror = IEOTHR;
        CLOSFL (fname, &ldum);
        return;
   } /* end switch */

   CLOSFL (fname, ierror);
   return ;

} /* End of ODSGetDimAnyTekAscHis */

TVoid ODSGetTmeAnyTekAscHis (
                      TString fname,
                      TInt4  *ftype,
                      TReal8 *timdef,
                      TInt4   maxdef,
                      TInt4   pardep,
                      TInt4   locdep,
                      TReal8  *timlst,
                      TInt4   maxlst,
                      TInt4   *nrlst,
                      TInt4   *ierror,
                      TString option)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Times from TEKAL ASCII file                              */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    ierror  -          O   Errorcode. See ods.h for definition.     */
/*    locdep  -          I   Location  dependency code                */
/*    maxdef  -          I   Nr. of time intervals in input.          */
/*    maxlst  -          I   Max. nr. of time intervals in output.    */
/*    nrlst   -          O   Nr. of time intervals in output.         */
/*    option  -        I/O   option not used                          */
/*    pardep  -          I   Parameter dependency code                */
/*    timlst  maxlst     O   Times found.                             */
/*    timdef  maxdef     I   Asked times.                             */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    FILE    *iunit ;
    TInt4   i ,j , nrows, ncols ;
    TInt4   jjjjmmdd, hhmmss ;
    TInt4   ldum ;
    TInt4   iyear0, imonth0, iday0, ihour0, imin0, isec0 ;
    TReal8  ftime ;

    *ierror = IEOK ;
    *nrlst = 0 ;


    switch ( *ftype )
    {
    case ITDATHE : /* equidistant */

        /* Generate list of times */
        for (i=0L; i<maxlst; i++)
        {
            *(timlst + *nrlst) = timdef[0]+i*timdef[2] ;
            (*nrlst)++ ;
        } /* end for i */

        break ;

    case ITDATH  : /* read specified column */
        OdsOpenFile (fname, ftype, &iunit, ierror);
        if ( *ierror != IEOK )
        {
            return;
        }

        /* HACK: the column can not be saved properly. So store it
           in the third filename
           This is also used to make the grid specific for the dataset
           - ARS 3545, 6210
        */
        if ( strncmp( &fname[2*ODS_FILNAMLEN] , "COLUMN" , 6 ) != 0 )
        {
           sprintf( &fname[2*ODS_FILNAMLEN] , "COLUMN%ld" , locdep ) ;
        }
        else
        {
           sscanf(  &fname[2*ODS_FILNAMLEN+6] , "%ld" , &locdep ) ;
        }

        /* skip pardep-1 blocks of tekal file, keep last nrows and ncols */
        for (i=1L; i<=(pardep-1); i++)
        {
            skip_tekal_block( iunit ) ;
                if ( feof ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return;
                }
        } /* end for */

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof   ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }

        /* loop over all rows in selected block pardep */
        for (i=0L; i<nrows; i++)
        {
            /* skip locdep columns */
            for (j=0L; j<locdep; j++)
            {
                *ierror = fscanf ( iunit, "%*e%*[ ,\t]" ) ;
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return ;
                }
                if ( feof   ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return ;
                }
            } /* end for j */

            /* read date and time columns */
            *ierror = fscanf ( iunit, "%ld%*[ ,\t]%ld", &jjjjmmdd, &hhmmss ) ;
            /* skip rest of record */
            skip_tekal_record ( iunit) ;
                if ( ferror ( iunit) )
                {
                    itrans ( ferror ( iunit), ierror) ;
                    CLOSFL ( fname, &ldum);
                    return ;
                }
                if ( feof   ( iunit) )
                {
                    *ierror = IEUEOF ;
                    CLOSFL ( fname, &ldum);
                    return ;
                }

            /* transform to julian day number */
            iyear0  =  jjjjmmdd                               / 10000 ;
            imonth0 = (jjjjmmdd - iyear0*10000              ) /   100 ;
            iday0   = (jjjjmmdd - iyear0*10000 - imonth0*100)         ;
            ihour0  =  hhmmss                                 / 10000 ;
            imin0   = (hhmmss   - ihour0*10000              ) /   100 ;
            isec0   = (hhmmss   - ihour0*10000 - imin0*100  )         ;
            julian ( &iyear0, &imonth0, &iday0, &ihour0, &imin0, &isec0 , &ftime ) ;

            if ( *nrlst < maxlst)
            {
                *(timlst + *nrlst) = ftime ;
                (*nrlst)++ ;
            }
            else
            {
                *nrlst  = maxlst ;
                *ierror = IETMNY ;
                CLOSFL ( fname, &ldum) ;
                return;
            }
        } /* end for i */

        CLOSFL ( fname, ierror) ;

        break ;

    default :
        *ierror = IEOTHR ;

        break ;

    } /* end switch */

    return;

} /* End of ODSGetTmeAnyTekAscHis */

TVoid ODSGetLocAnyTekAscHis (
                     TString fname,
                     TInt4   *ftype,
                     TString locdef,
                     TInt4   maxdef,
                     TInt4   pardep,
                     TInt4   timdep,
                     TString loclst,
                     TInt4   *loctyp,
                     TInt4   *locnr,
                     TInt4   maxlst,
                     TInt4   *nrlst,
                     TInt4   *ierror,
                     TString option)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Location from TEKAL ASCII file                       */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  -------------------------------          */
/*    fname   3          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    ierror  -          O   Errorcode. See ods.h for definition.     */
/*    locdef  maxdef     I   List of parameter filters.               */
/*    loclst  maxlst     O   List of parameters found.                */
/*    locnr   maxlst     O   List of index numbers of locations found.*/
/*    loctyp  maxlst     O   List of types of found locations         */
/*    maxdef  -          I   Number of parameter filters.             */
/*    maxlst  -          I   Number of parameters to expected         */
/*    nrlst   -          O   Nr of parameters returned.               */
/*    option  -        I/O   option not used                          */
/*    pardep  -          I   access index of parameter location       */
/*    timdep  -          I   access index of parameter time           */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    FILE    *iunit  ;
    TInt4   i, ldum ;
    TInt4   c , col ;
    TInt4   nrows , ncols  ;
    TChar   recordpart[41] ;
    TChar   *pstr ;
    TChar   colnam[21] ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    *nrlst = 0 ;
    *ierror = IEOK ;

    /* Skip pardep-1 tekal blocks  */
    for ( i = 1L; i <=(pardep-1); i++)
    {
        skip_tekal_block( iunit) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
    } /* end for */

    /* Set default column names */
    for ( i = 0L; i <maxlst; i++)
    {
        sprintf ( loclst + 21 * i, "column %ld", i+1 ) ;
        locnr [i] = i+1 ;
    }

    /* Overwrite column names if names are given in comment block */
    while ( ( c = getc ( iunit) ) == '*' )
    {
        ungetc ( c, iunit ) ; /* put back first character */
        extract_idx_name( iunit, &col, colnam ) ;
        if ( col > 0 && col <= maxlst )
        {
           strcpy( loclst + 21 * (col-1) , colnam ) ;
        }
#if 0
        if ( (fgets ( recordpart, (size_t) 40, iunit)) == NULL )
        {
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
        }
        pstr = strchr( recordpart, '\n' ) ;
        if ( pstr != NULL )
        {
           *pstr = '\0' ;
        }
        else
        {
            skip_tekal_record ( iunit) ;
        }

        /* Note:
           This way, a fixed format is implemented. It should be
           friendlier though!
        */
        if ( strncmp ( "* column ", recordpart, (size_t) 9 ) == 0 )
        {
           sscanf( recordpart +  9  , "%ld", &col ) ;

           /* Watch out for invalid column numbers */
           if ( col >= 1 && col <= maxlst )
           {
              strncpy( colnam , recordpart + 16 , 20 ) ; /* Note: 20 chars */
              colnam[20] = '\0' ;
              for ( i = 19 ; i >= 0 ; i -- )
              {
                 if ( colnam[i] != '\n' && colnam[i] != ' ' )
                 {
                    break ;
                 }
                 colnam[i] = '\0' ;
              }
              strcpy( loclst + 21 * (col-1) , colnam ) ;
           }
        }
#endif
    }

/* Set the actual number of "locations" to the number of columns
*/
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;

    *nrlst = ncols ;

    CLOSFL ( fname, ierror);
    return ;

} /* End of ODSGetLocAnyTekAscHis */

TVoid ODSGetMatAnyTekAscHis (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   parcod,
                      TReal8  *tim,
                      TInt4   *loc,
                      TReal4  misval,
                      TInt4   maxdim,
                      TReal4  *xdata,
                      TInt4   *ierror)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Matrix from TEKAL ASCII file                         */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    parcod  -          I   Parameter (code) of data to be retrieved.*/
/*    tim     3          I   date start, stop, step (Julian)          */
/*    loc     3*3        I   location of data subset to be retrieved: */
/*                                     [0..2,0] X start, stop, step   */
/*                                     [0..2,1] Y start, stop, step   */
/*                                     [0..2,2] Z start, stop, step   */
/*    misval  -          I   Missing value.                           */
/*    maxdim  -          I   Size of values array.                    */
/*    xdata   -          O   Retreived data.                          */
/*    ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    FILE    *iunit ;
    TInt4   i, j, ldum;
    TInt4   nrows, ncols ;
    TInt4   selected_col,
       block ;
    TReal4  colval;
    TChar * colstr;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    if ( parcod == 9999 )
    {
       /* get X-values; column nr is in fname(3) */
       colstr = &fname[ODS_FILNAMLEN+ODS_FILNAMLEN];
       sscanf( colstr, "Grid:block-%d_column-%d", &block, &selected_col );
       parcod = block;
    }
    else
    {
        /* {precon: loc[0]=loc[1], loc[2]=1} */
        selected_col = loc[0] - 1 ; /* HACK: location code not
                                       consistently used */
    }

    /* fill up value array with missing value */
    for (i = 0L ; i < maxdim ; i ++ )
    {
       *(xdata + i) = misval ;
    }

    /* skip parcod-1 blocks of tekal file */
    for (i=1L; i<=(parcod-1); i++)
    {
        skip_tekal_block ( iunit ) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
    }

    skip_tekal_comment_records( iunit ) ;
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
        if ( ferror ( iunit) )
        {
            itrans ( ferror ( iunit), ierror) ;
            CLOSFL ( fname, &ldum);
            return ;
        }
        if ( feof   ( iunit) )
        {
            *ierror = IEUEOF ;
            CLOSFL ( fname, &ldum);
            return ;
        }

    /* Check selected parameters */
    if ( ( nrows > maxdim ) || ( selected_col > ncols ) )
    {
        *ierror = IEOTHR ;
        CLOSFL ( fname, &ldum);
        return ;
    }

    /* loop over all rows in selected block parcod */
    for (i = 0L ; i < nrows ; i ++ )
    {
        /* skip selected_col-1 columns */
        for (j=0L; j<selected_col; j++)
        {
            *ierror = fscanf ( iunit, "%*e%*[ ,\t]" ) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof   ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
        } /* end for j */

        /* read value */
        colval = misval ;
        *ierror = fscanf ( iunit, "%e", &colval ) ;
        /* skip rest of record */
        skip_tekal_record ( iunit) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
/*
            if ( feof   ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
*/

        *(xdata + i ) = colval ;
    }

    CLOSFL (fname, ierror);
    return ;

} /* End of ODSGetMatAnyTekAscHis */

void ODSGetGrdAnyTekAscHis(  TString fname,
                             TInt4  itype,
                             TInt4  *indloc ,
                             TInt4  *indx ,
                             TInt4  *nocell ,
                             TInt4  *igisty ,
                             TInt4  *ierror )
/*************************************************************************/
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       itype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       indloc  -          I   Locations indices that are wanted (ignored) */
/*       indx    -          O   Administration array                     */
/*       nocell  -          O   Number of cells associated with grid     */
/*       igisty  -          O   Type of grid/GIS-information             */
/*                                                                       */
/*************************************************************************/

{
    FILE    *iunit ;
    TInt4   i, ldum;
    TInt4   nrows, ncols ;
    TInt4   selected_col,
            block ;
    TChar * colstr;

    OdsOpenFile (fname, &itype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    /* get position of grid; block nr and column nr are in fname(3) */
    colstr = &fname[ODS_FILNAMLEN+ODS_FILNAMLEN];
    sscanf( colstr, "Grid:block-%d_column-%d", &block, &selected_col );

    /* skip block-1 blocks of tekal file */
    for (i=1L; i<=(block-1); i++)
    {
        skip_tekal_block ( iunit ) ;
            if ( ferror ( iunit) )
            {
                itrans ( ferror ( iunit), ierror) ;
                CLOSFL ( fname, &ldum);
                return ;
            }
            if ( feof ( iunit) )
            {
                *ierror = IEUEOF ;
                CLOSFL ( fname, &ldum);
                return ;
            }
    }

    skip_tekal_comment_records( iunit ) ;
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;
        if ( ferror ( iunit) )
        {
            itrans ( ferror ( iunit), ierror) ;
            CLOSFL ( fname, &ldum);
            return ;
        }
        if ( feof   ( iunit) )
        {
            *ierror = IEUEOF ;
            CLOSFL ( fname, &ldum);
            return ;
        }

    for ( i = 0; i < nrows; i++ )
    {
        indx[i] = i;
    }
    *nocell = nrows;

    CLOSFL (fname, ierror);
    return;
}
/*------------------------- end of tekasc.c --------------------------*/
