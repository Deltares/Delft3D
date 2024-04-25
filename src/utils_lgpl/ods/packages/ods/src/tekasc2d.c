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
 *  tekasc2d.c  -  ODS functions for standard TEKAL ASCII 2d files
 *
 */

 /* General information:
  * For detail design .../gpp/doc/memos/document s-do-001.0pk
  * This file contains the following functions:
  *     -        TVoid  ODSGetDimAnyTekAscMap
  *     -        TVoid  ODSGetParAnyTekAscMap
  *     -        TVoid  ODSGetTmeAnyTekAscMap
  *     -        TVoid  ODSGetLocAnyTekAscMap
  *     -        Tvoid  ODSGetGrdAnyTekAscMap
  *     -        TVoid  ODSGetMatAnyTekAscMap
  *     -        TVoid  read_tekal_dimension_record
  *     -        TVoid  Get_tekal_block_time
  *              TReal8 Julian_from_date_time
  * uses from tekasc.c :
  *              TVoid skip_tekal_record
  *              TVoid skip_tekal_block
  *              TVoid skip_tekal_comment_records
  *              TVoid skip_tekal_blocname_record
  *              TVoid skip_tekal_values_records
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

static TVoid read_tekal_dimension_record ( FILE *, TInt4 *, TInt4 *, TInt4 *);
static TVoid Get_tekal_block_time( TInt4 *, TInt4 *, TString, FILE *);



/* @@---------------------------------------------------
    Function:    ODSGetDimAnyTekAscMap
    Author:      Pleun Koole
    Purpose:     generate parameterlist
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetDimAnyTekAscMap (
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
    TInt4   i, ncols, nrows , rowlng, nblocs , block, selected_col;
    TInt4   ldum;
    TChar * colstr;
    TChar   string[60] ;


    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    switch (dimtyp[0])
    {
    case 'p':  /* par */
    case 'P':
        /* = nof data columns in first block */
        /* Note: It is assumed that all blocks are identical */

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng) ;

        ndim[0] = (TInt4) 1;
        ndim[1] = ncols  ; /* number of data columns in block */
        break;

    case 'l':  /* loc */
    case 'L':

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;

        ndim[0] = (TInt4) 2; /* width and height of grid */
        if ( rowlng != 0 )
        {
           ndim[1] = rowlng ;
           ndim[2] = nrows / rowlng ;
        }
        else
        {
           ndim[0] = 0 ;
           *ierror = IETYPE ; /* We did not get the number of rows in matrix */
        }
        break;

    case 't':  /* tim */
    case 'T':

/***    printf( "\nGetDim times = nof data blocs\n") ;    ***/
        /* number of datablocs */

        nblocs = 0 ;
        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;

        while ( ! feof( iunit) )
        {
          read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;
          if ( rowlng == 0 )
          {
             *ierror = IETYPE ; /* Do not accept the file in this case! */
             CLOSFL( fname , &ldum ) ;
             return ;
          }

          skip_tekal_values_records( iunit, nrows) ;

          if ( ferror ( iunit) )
            {
              itrans ( ferror ( iunit), ierror) ;
              CLOSFL ( fname, &ldum);
              return;
            }
          nblocs++ ;

          skip_tekal_comment_records( iunit ) ;
          skip_tekal_blocname_record( iunit) ;

        } /* end for */

        ndim[0] = (TInt4) 1;
        ndim[1] = nblocs ; /* number of blocks */
/***    printf( "GetDim times : %i timeblocs\n", nblocs ) ; ***/

        break;

    default:
        *ierror = IEOTHR;
        CLOSFL (fname, &ldum);
        return;
   } /* end switch */

   CLOSFL (fname, ierror);
   return ;

} /* End of ODSGetDimAnyTekAscMap */


/* @@---------------------------------------------------
    Function:    ODSGetParAnyTekAscMap
    Author:      Pleun Koole
    Purpose:     generate parameterlist
    Context:     Any_TEKAL_ASCII_1d interface
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetParAnyTekAscMap (
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
    FILE   *iunit  ;
    TChar  *pstr   ;
    TChar   c      ;
    TChar   name[20] ;
    TChar   parnam [ 21] ;
    TInt4   i, idx, nrows, ncols, rowlng, ldum ;

    *ierror = IEOK ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
        if ( *ierror != IEOK )
        {
            return;
        }

    *nrlst = 0 ;

    /* set default names */

    *nrlst = maxlst ;

    for ( i = 0L; i < maxlst; i++ )
    {
      if ( i == 0 )
        sprintf( parlst + 21*i, "XCOR") ;
      else if ( i == 1 )
        sprintf( parlst + 21*i, "YCOR") ;
      else
        sprintf( parlst + 21*i, "column %1d", i+1) ;
    }

    /* Overwrite column names if names are given in comment block */
    while ( ( c = getc ( iunit) ) == '*' )
    {
        ungetc ( c, iunit ) ; /* put back first character */
        extract_idx_name( iunit, &idx, name ) ;
        if ( idx > 2 && idx <= (*nrlst) )
        {
           strcpy( parlst + 21 * (idx-1) , name ) ;
        }
    }
    skip_tekal_blocname_record( iunit ) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;

    /* set parameter type, unit and indexcodes */
    for (i=0L; i<*nrlst; i++)
    {
        strcpy ( paruni + 21 * ( i ) , "                    " ) ;
        *(partyp+i) = ODS_PT_TIME_DEP ;
        *(parcod+i) = i + 1 ;
    } /* end for */

    CLOSFL ( fname, ierror) ;
    return ;

} /* End of ODSGetParAnyTekAscMAp */

/* @@---------------------------------------------------
    Function:    ODSGetTmeAnyTekAscMap
    Author:      Pleun Koole
    Purpose:     get times
    Context:     Any_TEKAL_ASCII_2d interface
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetTmeAnyTekAscMap (
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
    TInt4   i ,j , nrows, ncols, rowlng ;
    TInt4   defdate, deftime, jjjjmmdd, hhmmss ;
    TInt4   ldum , idef;
    TReal8  blktime, timbeg , deltt ;

    *ierror = IEOK ;
    *nrlst = 0 ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    defdate = 19000101 ;
    deftime =   000000 ;
    idef = 0 ;

    timbeg = Julian_from_date_time( defdate, deftime) ;
/*** printf( "\nGetTme default %i %i %f\n",defdate, deftime, timbeg); ***/
    deltt   = 1.0; /* A step - completely arbitrary of one day */

/*** printf( "GetTme maxlst %i\n", maxlst) ;  ***/
    for (i=0L; i<maxlst; i++)
    {

      Get_tekal_block_time( &jjjjmmdd, &hhmmss, fname, iunit) ;
/***  printf( "GetTme i date time %i %i %i\n", i, jjjjmmdd, hhmmss);  ***/
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetTme time error \n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      if ( jjjjmmdd < 0 ) { *ierror = IEUNKN ; return ; }

      if ( jjjjmmdd == 0 )
      {
        blktime = timbeg + idef*deltt ;
/***    printf( "GetTme Julian %f\n", blktime);  ***/
        idef = idef + 1 ;
      }
      else
      {
        blktime = Julian_from_date_time( jjjjmmdd, hhmmss) ;
/***    printf( "GetTme Julian %f\n", blktime);  ***/
      }

      *(timlst + *nrlst) = blktime ;
      (*nrlst)++ ;

      skip_tekal_blocname_record( iunit) ;
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetTme bloc error\n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng);
/***  printf( "GetTme dim %i %i %i\n", nrows, ncols, rowlng);  ***/
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetTme dim error \n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      skip_tekal_values_records( iunit, nrows);

    }

    CLOSFL ( fname, ierror) ;

    return;

} /* End of ODSGetTmeAnyTekAscMap */

 TVoid ODSGetLocAnyTekAscMap (
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
    FILE    *iunit ;
    TInt4   i, ldum ;
    TInt4   c , col ;
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


    CLOSFL ( fname, ierror);
    return ;

} /* End of ODSGetLocAnyTekAscMap */


void ODSGetGrdAnyTekAscMap(  TString fname,
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
    TInt4   i, j, ldum;
    TInt4   nblocs, nrows, ncols, rowlng ;
    TInt4   selected_col,
            block ;
    TReal4  x, y, colval;
    TReal4  *array      ;
    TInt4   ix , iy , nxmax , nymax ;
    TInt4   count       ;
    TReal4  tekal_misval1, tekal_misval2 ;
    TChar * colstr;

    OdsOpenFile (fname, &itype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

/* Missing value: may be set in the file - via the comments
   For now: fixed at -999.0 or 999.999
*/
    tekal_misval1 = -999.0 ;
    tekal_misval2 =  999.999 ;

    /* get position of grid; block nr and column nr are in fname(3) */
    colstr = &fname[ODS_FILNAMLEN+ODS_FILNAMLEN];
    sscanf( colstr, "Grid:block-%d_column-%d", &block, &selected_col );

    /* skip block-1 blocks of tekal file */
/*
    TODO !

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
*/

/*
    skip_tekal_comment_records( iunit ) ;
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;

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

*/

        indx[0] = 0 ;

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;
        if ( rowlng == 0 )
        {
           rowlng = 1 ; /* Crude correction for missing information */
        }

        array = ( TReal4 *) malloc( nrows * sizeof( TReal4 ) ) ;

/***    printf( "Get grid\n") ; ***/
/***    printf( "rows cols lng %i %i %i \n", nrows, ncols, rowlng); ***/

        for ( i = 0; i < nrows; i++ )
        {

          *ierror = fscanf (iunit, "%f%*[ ,\t]%f%*[ ,\t]%f", &x, &y, &colval);

/***         if ( i <= 5 ) printf( "%i %f %f %f\n", i,x,y,colval); ***/

/* AM: rather awkward short-stop! Why would it be required?
          if ( colval != tekal_misval1 && colval != tekal_misval2 )
          {
           if ( x == 0. || y == 0. )
           {
             printf( "Error: illegal record %i %f %f %f\n", i,x,y,colval);
             exit(1) ;
           }
          }
*/

          skip_tekal_record( iunit) ;

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

          array[i] = colval ;

        } /* end for */

/* Interpretation of values: a cell is active if it has four
   value, one for each vertex
*/
        nymax = nrows / rowlng ;
        nxmax = rowlng         ;
        i     = 0              ;

        for ( iy = 0 ; iy < nymax ; iy ++ )
        {
           for ( ix = 0 ; ix < nxmax ; ix ++ )
           {
              indx[i] = 0 ; /* Assume cell is not active */
              count   = 0 ; /* Count number vertices with values */

              if ( array[i] != tekal_misval1 &&
                   array[i] != tekal_misval2 )
              {
                 count ++ ;
              }
              if ( ix > 0 )
              {
                 if ( array[i-1] != tekal_misval1 &&
                      array[i-1] != tekal_misval2 )
                 {
                    count ++ ;
                 }
              }
              if ( iy > 0 )
              {
                 if ( array[i-nxmax] != tekal_misval1 &&
                      array[i-nxmax] != tekal_misval2 )
                 {
                    count ++ ;
                 }
              }
              if ( ix > 0 && iy > 0 )
              {
                 if ( array[i-nxmax-1] != tekal_misval1 &&
                      array[i-nxmax-1] != tekal_misval2 )
                 {
                    count ++ ;
                 }
              }

              if ( count == 4 )
              {
                 indx[i] = i + 1 ;
              }

              i ++ ;
           }
        }

        *nocell = nrows ;

/* Free work array */
    free( array ) ;

    CLOSFL (fname, ierror);
    return;
} /* End of ODSGetGrdAnyTekAscMap */


TVoid ODSGetMatAnyTekAscMap (
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
    TInt4   nrows, ncols, rowlng ;
    TInt4   selected_col, block ;
    TReal4  colval;
    TChar * colstr;

    TInt4 defdate, deftime, jjjjmmdd, hhmmss ;
    TReal8  blktime, timbeg , deltt ;
    int idef ;

    /* fill up value array with missing value */
    for (i = 0L ; i < maxdim ; i ++ )
    {
       *(xdata + i) = misval ;
    }

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

/*** printf( "\nGetMat: find time %f \n", tim[0]) ;    ***/

    /* find block for given time */

    defdate = 19000101 ;
    deftime = 000000 ;
    timbeg = Julian_from_date_time( defdate, deftime) ;
/*** printf( "\nGetMat default %i %i %f\n",defdate, deftime, timbeg); ***/
    deltt  = 1.0 ; /* same step as in GetTme.... */
    idef = 0 ;

    Get_tekal_block_time( &jjjjmmdd, &hhmmss, fname, iunit );
/*** printf( "GetMat : read time %i %i\n", jjjjmmdd, hhmmss) ; ***/
    if ( ferror( iunit) || feof( iunit) )
    { /* printf( "GetMat time error\n") ; exit(1); */
        *ierror = IEUNKN ;
        return ;
      }
    if ( jjjjmmdd < 0 ) { *ierror = IEUNKN ; return ; }

    if ( jjjjmmdd == 0 )
    {
       blktime = timbeg + idef * deltt ;
/***   printf( "GetMat time = Julian %f\n", blktime) ; ***/
    }
    else
    {
       blktime = Julian_from_date_time( jjjjmmdd, hhmmss) ;
/***   printf( "GetMat time = Julian %f\n", blktime) ; ***/
    }

    while( blktime != tim[0] )
    {
      skip_tekal_blocname_record( iunit) ;
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetMat bloc error\n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng);
/***  printf( "GetMat dim %i %i %i\n", nrows, ncols, rowlng) ;  ***/
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetMat dim error\n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      skip_tekal_values_records( iunit, nrows);
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetMat skip error\n") ; exit(1); */
        *ierror = IEUNKN ;
        break ;
      }

      Get_tekal_block_time( &jjjjmmdd, &hhmmss, fname, iunit );
/***  printf( "GetMat : read time %i %i\n", jjjjmmdd, hhmmss) ; ***/
      if ( ferror( iunit) || feof( iunit) )
      { /* printf( "GetMat time error\n") ; exit(1) ; */
        *ierror = IEUNKN ;
        break ;
      }

      if ( jjjjmmdd < 0 ) { return ; }

      if ( jjjjmmdd == 0 )
      {
         idef = idef + 1 ;
         blktime = timbeg + idef * deltt ;
/***     printf( "GetMat time = Julian %f\n", blktime) ; ***/
      }
      else
      {
         blktime = Julian_from_date_time( jjjjmmdd, hhmmss) ;
/***     printf( "GetMat time = Julian %f\n", blktime) ; ***/
      }

    }

/*** printf( "Found time %f\n", blktime) ;  ***/

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

    skip_tekal_blocname_record( iunit) ;
    if ( ferror( iunit) || feof( iunit) )
    { /* printf( "GetMat bloc error\n") ; exit(1) ; */
        *ierror = IEUNKN ;
        return ;
    }

    read_tekal_dimension_record( iunit, &nrows, &ncols, &rowlng ) ;
/*** printf( "Dim %i %i %i\n", nrows, ncols, rowlng) ;  ***/
    if ( ferror( iunit) || feof( iunit) )
    { /* printf( "GetMat dim error\n") ; exit(1) ; */
        *ierror = IEUNKN ;
        return ;
    }

    /* Check selected parameters */

/*** printf( "Col %i\n", parcod) ; ***/

    if ( ( nrows > maxdim ) || ( parcod > ncols ) )
    {
        *ierror = IEOTHR ;
        CLOSFL ( fname, &ldum);
        return ;
    }

    /* loop over all rows in selected block parcod */
    for (i = 0L ; i < nrows ; i ++ )
    {
        /* skip parcod-1 columns */
        for (j=0L; j<parcod-1; j++)
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
        } /* end for skip parcod-1 columns */

        /* read value */
        colval = misval ;
        *ierror = fscanf ( iunit, "%e", &colval ) ;

/***  if ( i <= 10 ) printf( "%i %f\n", i, colval) ;    ***/

            skip_tekal_record( iunit) ;

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
    } /* end for all rows in block */

    CLOSFL (fname, ierror);
    return ;

} /* End of ODSGetMatAnyTekAscMap */



/* @@---------------------------------------------------
    Function:    read_tekal_dimension_record
    Author:      Pleun Koole
    Purpose:     read tekal dimension record
    Context:     Any_TEKAL_ASCII_2d interface
    Pseudo code:

  ------------------------------------------------------*/

 static TVoid read_tekal_dimension_record (
      FILE *iunit, TInt4 *nrows, TInt4 *ncols, TInt4 *rowlng)
{

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    *nrows = 0 ;
    *ncols = 0 ;
    *rowlng = 0 ;

    /* read number of rows and number of columns
       and horizontal array dimension */
    fscanf ( iunit, "%ld%*[ ,\t]%ld%*[ ,\t]%ld", nrows, ncols, rowlng ) ;
    skip_tekal_record( iunit) ;

    return ;

} /* End of read_tekal_dimension_record */


/* Read all comment records and read time if any available
   returns time < 0 : error
           time = 0 : no time availble , create default time
           time > 0 : time found, use it
*/

static TVoid Get_tekal_block_time(
    TInt4  *jjjjmmdd ,
    TInt4  *hhmmss   ,
    TString fname    ,
    FILE   *iunit    )
{

    TInt4  c , ldum   ;
    TChar *pstr ;
    TChar  recordpart[41] ;
    int nrows, ncols, rowlng ;

     /* from next block, read comment records and get time if any */

      *jjjjmmdd = 0 ;
      *hhmmss = 0 ;

      while ( ( c = getc ( iunit) ) == '*' )
      {
        ungetc ( c, iunit ) ; /* put back first character */
        if ( (fgets ( recordpart, (size_t) 40, iunit)) == NULL )
        {
            if ( ferror ( iunit) )
            {
                CLOSFL ( fname, &ldum);
                *jjjjmmdd = -1 ;
                return ;
            }
            if ( feof ( iunit) )
            {
                CLOSFL ( fname, &ldum);
                *jjjjmmdd = -1 ;
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
/***    printf( "%s\n", recordpart) ;  ***/

        /* Note:
           This way, a fixed format is implemented. It should be
           friendlier though!
        */
        if ( strncmp ( "* time : ", recordpart, (size_t) 9 ) == 0 )
        {
           sscanf( recordpart +  9  , "%ld%*[ ,\t]%ld",
                    jjjjmmdd, hhmmss ) ;
        }
      }
}

TReal8 Julian_from_date_time( TInt4 jjjjmmdd, TInt4 hhmmss)
{

    TInt4   iyear0, imonth0, iday0, ihour0, imin0, isec0 ;
    TReal8 jultime ;

    iyear0  =  jjjjmmdd                               / 10000 ;
    imonth0 = (jjjjmmdd - iyear0*10000              ) /   100 ;
    iday0   = (jjjjmmdd - iyear0*10000 - imonth0*100)         ;
    ihour0  =  hhmmss                                 / 10000 ;
    imin0   = (hhmmss   - ihour0*10000              ) /   100 ;
    isec0   = (hhmmss   - ihour0*10000 - imin0*100  )         ;
    julian ( &iyear0, &imonth0, &iday0, &ihour0, &imin0, &isec0 ,
               &jultime) ;

    return jultime ;
}
/*----------------------*/

