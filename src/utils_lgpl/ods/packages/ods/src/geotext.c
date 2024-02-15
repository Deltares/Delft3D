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
 *  geotext.c  -  ODS functions for standard TEKAL ANNO files
 *
 */

 /* General information:
  * This file contains the following functions:
  *     -        TVoid  ODSGetDimTekAnno
  *     -        TVoid  ODSGetParTekAnno
  *     -        TVoid  ODSGetTmeTekAnno
  *     -        TVoid  ODSGetLocTekAnno
  *     -        Tvoid  ODSGetGrdTekAnno
  *     -        TVoid  ODSGetMatTekAnno
  *     -        TVoid  read_tekal_dimension_record
  * uses from tekasc.c :
  *              TVoid skip_tekal_record
  *              TVoid skip_tekal_block
  *              TVoid skip_tekal_comment_records
  *              TVoid skip_tekal_blocname_record
  *              TVoid skip_tekal_values_records
  *
  * Note:
  * The file type is almost that of the TEKAL ANNO tasks, though
  * only the first block is read.
  * The difference is that the first character of the text may
  * be interpreted as a delimiter: /text/ will appear as text
  * This makes an unambiguous usage of leading and trailing blanks
  * possible. (If no such character is present - the first is not
  * repeated - assume that anything is to be included up to the
  * end of line).
  * I
  */
/* @end@ */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

/* #include "portable.h" */
#include "ods.h"
#include "odsmodel.h"
/* #include "utils.h"
#include "itrans.h"
#include "julian.h"
#include "opnclose.h"
#include "geotext.h" */

#ifndef FALSE
#define FALSE   0
#define TRUE    1
#endif

static TVoid read_tekal_dimension_record ( FILE *, TInt4 *, TInt4 *);
static TVoid ReadTekAnnoBlock( FILE *, TInt4 *, TReal4 *, TReal4 *, TString,
                               TInt4 , TInt4  , TReal4  , TInt4 *          ) ;


/* @@---------------------------------------------------
    Function:    ODSGetDimTekAnno
    Author:      Arjen Markus
    Purpose:     Return the dimensions
    Context:     Used in ODS routine getdim()
    Pseudo code:
                 Ignore any blocks, just read the first
                 one.
  ------------------------------------------------------*/
TVoid ODSGetDimTekAnno (
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
/* Function: Get Dimensions from TEKAL ANNO file                      */
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
    FILE    *iunit                 ;
    TInt4   i      , ncols , nrows ;
    TInt4   first  , last  , ldum  ;
    TChar * colstr                 ;
    TChar   string[60]             ;
    TReal4  misval                 ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    switch (dimtyp[0])
    {
    case 'p':  /* par */
    case 'P':
        /* There are always two parameters: XCOR and YCOR; the third column
           is text
        */

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols) ;

        if ( ncols != 3 )
        {
           *ierror = IEINFO ;
        }
        ndim[0] = (TInt4) 1 ;
        ndim[1] = ncols - 1 ; /* leave out the text column */
        break;

    case 'l':  /* loc */
    case 'L':

        skip_tekal_comment_records( iunit ) ;
        skip_tekal_blocname_record( iunit) ;
        read_tekal_dimension_record( iunit, &nrows, &ncols) ;

        misval = -999.0 ; /* Dummy */
        nrows  = -nrows ; /* Return the number */
        first  = -1     ;
        last   = -1     ;
        ReadTekAnnoBlock( iunit , &nrows , NULL   , NULL , NULL ,
                          first , last   , misval , ierror      ) ;
        ndim[0] = (TInt4) 1 ; /* Linear array */
        ndim[1] = nrows     ; /* Number of "location" names */
        break;

    case 't':  /* tim */
    case 'T':

        /* No time information */

        ndim[0] = (TInt4) 1 ;
        ndim[1] =         1 ; /* Number of times: dummy */

        break;

    default:
        *ierror = IEOTHR;
        CLOSFL (fname, &ldum);
        return;
   } /* end switch */

   CLOSFL (fname, &ldum);
   if ( *ierror == IEOK )
   {
      *ierror = ldum ;
   }

   return ;

} /* End of ODSGetDimTekAnno */


/* @@---------------------------------------------------
    Function:    ODSGetParTekAnno
    Author:      Arjen Markus
    Purpose:     Generate parameterlist
    Context:     Used in ODS routine getpar()
    Pseudo code:

  ------------------------------------------------------*/
TVoid ODSGetParTekAnno (
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
/* Function: Get Parameters from TEKAL ANNO file                      */
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
    TInt4   i, nrows, ncols, rowlng, ldum ;

    *ierror = IEOK ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
       return;
    }

    *nrlst = 2 ;

    /* Set fixed names */
    sprintf( parlst +  0 , "XCOR" ) ;
    sprintf( parlst + 21 , "YCOR" ) ;

    /* Overwrite column names if names are given in comment block */

    /* set parameter type, unit and indexcodes */
    for (i=0L; i<*nrlst; i++)
    {
        strcpy ( paruni + 21 * ( i ) , "                    " ) ;
        *(partyp+i) = ODS_PT_LOC_DEP ;
        *(parcod+i) = i + 1 ;
    } /* end for */

    CLOSFL ( fname, ierror) ;
    return ;

} /* End of ODSGetParTekAnno */

/* @@---------------------------------------------------
    Function:    ODSGetTmeTekAnno
    Author:      Arjen Markus
    Purpose:     Get times
    Context:     Used by ODS routine gettme()
    Pseudo code:
                 Dummy routine - data time-independent

  ------------------------------------------------------*/
TVoid ODSGetTmeTekAnno (
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
/* Function: Times from TEKAL ANNO file                               */
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

    for ( i =0 ; i < maxlst ; i ++ )
    {
      *(timlst + i) = timbeg ;
    }
    *nrlst = maxlst ;

    CLOSFL ( fname, ierror) ;

    return;

} /* End of ODSGetTmeTekAnno */

 TVoid ODSGetLocTekAnno (
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
/* Function: Get Location from TEKAL ANNO file                        */
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
    TInt4   i , ldum , nrows , ncols ;
    TInt4   c , col  , first , last  ;
    TChar   recordpart[41]           ;
    TChar   *pstr                    ;
    TChar   colnam[21]               ;
    TReal4  misval                   ;

    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    *ierror = IEOK ;

    skip_tekal_comment_records( iunit ) ;
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;

/* Read the names (en passant the coordinates)
*/
    *nrlst = -nrows      ;
    first  = 1           ;
    last   = maxlst      ;
    misval = -999.0      ; /* Dummy */

    ReadTekAnnoBlock( iunit , nrlst , NULL   , NULL , loclst ,
                      first , last  , misval , ierror        ) ;

/* Fill the two arrays locnr and loctyp
*/
    for ( i = 0 ; i < *nrlst ; i ++ )
    {
       locnr[i]  = i + 1 ;
       loctyp[i] = 0     ;
    }

    CLOSFL ( fname, ierror);
    return ;

} /* End of ODSGetLocTekAnno */

TVoid ODSGetMatTekAnno (
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
/* Function: Get Matrix from TEKAL ANNO file                          */
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
    TInt4   i      , j     , ldum  ;
    TInt4   nrows  , ncols         ;
    TInt4   first  , last          ;
    TChar * colstr                 ;

    TInt4   defdate, deftime, jjjjmmdd, hhmmss ;
    TReal8  blktime, timbeg , deltt ;
    int idef ;

/* Prepare the reading
*/
    OdsOpenFile (fname, ftype, &iunit, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }
    skip_tekal_comment_records( iunit ) ;
    skip_tekal_blocname_record( iunit) ;
    read_tekal_dimension_record( iunit, &nrows, &ncols ) ;

/* Read the names (en passant the coordinates):
   either the x-coordinate or the y-coordinate
*/
    first = loc[0] ;
    last  = loc[1] ;

    switch ( parcod )
    {
       case 1 : /* X-coordinates */
          ReadTekAnnoBlock( iunit , &nrows , xdata  , NULL  , NULL ,
                            first , last   , misval , ierror       ) ;
          break ;

       case 2 : /* Y-coordinates */
          ReadTekAnnoBlock( iunit , &nrows , NULL   , xdata , NULL ,
                            first , last   , misval , ierror       ) ;
          break ;

       default : /* Impossible */
          *ierror = IEINFO ;
          break ;
    }

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
      FILE *iunit, TInt4 *nrows, TInt4 *ncols )
{

    if ( ferror ( iunit) || feof ( iunit) )
    {
        return ;
    }

    *nrows = 0 ;
    *ncols = 0 ;

    /* read number of rows and number of columns
       and horizontal array dimension */
    fscanf ( iunit, "%ld%*[ ,\t]%ld", nrows, ncols ) ;
    skip_tekal_record( iunit) ;

    return ;

} /* End of read_tekal_dimension_record */

/* @@---------------------------------------------------
    Function:    ReadTekAnnoBlock
    Author:      Arjen Markus
    Purpose:     Read the block with coordinates and texts
    Context:     Used by all external functions in this file
    Pseudo code:
                 The file has been opened. First position
                 it at the first line of the block.
                 This, as any other line, contasins the x-
                 and y-coordinates.
                 Scan the line and determine the number of
                 parts of 19 characters in which to put the
                 text (limitation of ODS).
                 Continue until all lines in the blocks
                 have been read.
    Note:
                 The procedure is twofold:
                 - if the pointers to the arrays are not NULL,
                   the data are stored. If they are NULL, these
                   particulsr arrays are ignored.
                 - if the value of nrlst < 0, only return the number
                   of strings and substrings. This is the surest
                   way of getting a consistent count!
  ------------------------------------------------------*/

static TVoid ReadTekAnnoBlock(
      FILE    *iunit  ,   /* I File pointer */
      TInt4   *nrlst  ,   /* IO Number of strings (input: block size) */
      TReal4  *xcoord ,   /* O X-coordinates (may be NULL!) */
      TReal4  *ycoord ,   /* O Y-coordinates (may be NULL!) */
      TString loclst  ,   /* O String array (may be NULL!)  */
      TInt4   first   ,   /* I First location in block      */
      TInt4   last    ,   /* I Last  location in block      */
      TReal4  misval  ,   /* I Missing value                */
      TInt4   *ierror )   /* O Error code                   */
{
    TInt4   idx    , idxf         ;
    TInt4   i , j  , nblock       ;
    TInt4   length                ;
    TReal4  xstore , ystore       ;
    TReal4  xcrd   , ycrd         ;
    TChar   buffer[80] , text[80] ;
    TString pstr                  ;

/* Loop over all lines in the block
*/
    nblock = *nrlst ;
    idxf   = 1      ;
    idx    = 0      ;

    if ( *nrlst < 0 ) nblock = -(*nrlst) ;

    for ( i = 0 ; i < nblock ; i ++ )
    {
       fgets( buffer , sizeof( buffer ) , iunit ) ;
       if ( ferror( iunit ) || feof( iunit ) )
       {
          /* Take care of incorrect headers by storing the
             correct information
          */
          /* ---
          *ierror = IEUEOF ;
          return ;
          --- */
          break ;
       }

       sscanf( buffer , "%f%*[ ,\t]%f%*[ ,\t]%[^\n]" , &xcrd , &ycrd , text ) ;

/* Skip over end of line
*/
       pstr = strchr( buffer , '\n' ) ;
       if ( pstr == NULL )
       {
          skip_tekal_record( iunit ) ; /* Skip over the last bit */
       }

/* Look for a delimiting character: first non-blank
*/
       pstr   = text           ;
       length = strlen( text ) ;
       for ( j = 0 ; j < length ; j ++ )
       {
          if ( text[j] != ' ' && text[j] != '\t' )
          {
             if ( !isalnum( text[j] ) )
             {
                pstr = strchr( &text[j+1] , text[j] ) ;
                if ( pstr != NULL )
                {
                   *pstr = '\0' ;
                   pstr  = &text[j+1] ;
                }
             }
             else
             {
                pstr = text ; /* No delimiting character found */
             }
             break ;
          }
       }

/* Copy into the appropriate strings.
   If the string is too long to store as an ODS location name,
   reserve another location for the rest (and so on).
*/
       xstore = xcrd           ;
       ystore = ycrd           ;
       length = strlen( pstr ) ;

       while ( length > 0 )
       {
          if ( idxf >= first && idxf <= last )
          {
             if ( xcoord != NULL ) xcoord[idx] = xstore ;
             if ( ycoord != NULL ) ycoord[idx] = ystore ;
             if ( loclst != NULL )
             {
                strncpy( loclst+idx*(PARLEN+1) , pstr , PARLEN ) ;
                loclst[idx*(PARLEN+1)+PARLEN-1] = '\0' ;
             }
             idx ++ ;
          }

          if ( length > PARLEN-1 )
          {
             xstore = misval         ;
             ystore = misval         ;
             pstr   += PARLEN-1      ;
          }
          length -= (PARLEN-1)       ;

          idxf ++ ;
       }
    }

    if ( *nrlst < 0 )
    {
       *nrlst = idxf  - 1 ; /* Return the number of strings */
    }

    return ;

} /* End of read_tekal_dimension_record */
/*----------------------*/

