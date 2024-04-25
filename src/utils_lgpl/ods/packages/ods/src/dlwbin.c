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
/*
 *  dlwbin.c  -  ODS functions for standard Delwaq/Delpar MAP and HIS files
 *               also useful for Delpar PLO files and DELWAQ balances files.
 *
 *  Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 28-06-04 9:19 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dlwbin.c,v $
*/
/*
 *
 */

/*   Date:       28 Feb 1994                                          */
/*   Time:       11:53                                                */
/*   Program:    DLWBIN.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andr  Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   0.00 -- 21 Jun 1993 -- 11:19 -- Operating System: DOS            */
/*   Project:     Open Data Structuur                                 */
/*   Module:      Loketjes                                            */
/*   Subroutines: gldlwh Get locations from Delwaq HIS file           */
/*                gldlwm Get locations from Delwaq MAP file           */
/*                gpdlwm Get parameters from Delwaq MAP/HIS file      */
/*                gtdlwh Get times from Delwaq HIS file               */
/*                gtdlwm Get times from Delwaq MAP file               */
/*                gvdlwh Get values from Delwaq HIS file              */
/*                gvdlwm Get values from Delwaq MAP file              */
/*   Function:    "Loket" functions per filetype                      */
/*   Comment:     General version                                     */
/*   Reference:   opendata.doc                                        */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

/* #include "portable.h" */
#include "ods.h"
#include "odsmodel.h"
/* #include "dlwbin.h"
#include "itrans.h"
#include "julian.h"
#include "equal.h"
#include "nefis.h"
#include "opnclose.h" */

#ifndef FALSE
#    define FALSE   0
#    define TRUE    1
#endif

#define MAXIBU 512


/* Nice patch for MS-Windows DLL's ! */
#ifdef MSWINDOWS
#   define sprintf wsprintf
     _far __cdecl wsprintf(char _far* lpszOut, const char _far* lpszFmt, ...);
#endif

/* #include "DEZE NOG NIET!" */

static void GetDlwRunId(    FILE   *fp     , TInt4  ftype   , TReal8 *dlwtim ,
                            TReal8 *dlwtm0 , TInt4  *ierror                  );
static void SkipDlwRunId(   FILE   *fp     , TInt4  ftype   , TInt4  *nsub   ,
                            TInt4  *nseg   , TInt4  *headersize,
                            TInt4  *ierror                                   );
static void SkipDlwHSubs(   FILE   *fp     , TInt4  nsub    , TInt4  *ierror );

static TString ReadDlwqSubs(FILE   *fp     , TInt4  *nsub   , TInt4  *ierror );

static void GetDelparPloXY( FILE   *fp     , TInt4  *loc    , TInt4  *nseg   ,
                            TInt4  parcod  , TReal4 *values , TInt4  *ierror );

static TVoid IdentifyDlwqBalances(
                      FILE    *fp      , TInt4   nsub     , TString pardef   ,
                      TString list_par , TString list_bal , TString unit_bal ,
                      TInt4   *balcod  , TInt4   *baltyp  , TInt4   *nbal    ,
                      TInt4   *ierror                                        );

/* Define the number of bytes for the delimiters in Fortran unformatted
   files - should the platform not support Fortran binary files
*/
#define DELIM 0
#ifdef UNIX
#ifndef LINUX
#undefine DELIM
#define DELIM 4
#endif
#endif

/*------------------------------------------------------------------------
 * subroutine for opening a file, gets called by OdsOpenFile()
 *------------------------------------------------------------------------
 */
void DlwBinOpen( char    *fname  ,
                 FILE    **fp    ,
                 TInt4   *ierror )
{
    char    ftype [] = "rb" ;

    *ierror = IEOK;

    *fp = fopen ( fname, ftype) ;
    if ( *fp == NULL)
        *ierror = IENOFI ;

    return;
}

/*------------------------------------------------------------------------
 * subroutine for closing a file, gets called by OdsDoClose()
 *------------------------------------------------------------------------
 */
void DlwBinClose( FILE    *fp,
                  TInt4   *ierror)
{
    *ierror = IEOK;

    if ( fclose (fp) != 0)
        *ierror = IENOFI ;

    return;
}


/*************************************************************************/
/*    SUBROUTINE Get Location from DeLWaq His/Map file                   */
/*************************************************************************/

void ODSGetLocDlwq( char    *fname,
                    TInt4   *ftype,
                    char    *locdef,
                    TInt4   maxdef,
                    char    *loclst,
                    TInt4   maxlst,
                    TInt4   *nrlst,
                    TInt4   *locnr,
                    TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdef  maxdef     I   List of parameters wanted.               */
/*       loclst  maxlst     O   List of parameters found.                */
/*       locnr   maxlst     O   List of index numbers of locations found.*/
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*                                                                       */
/*************************************************************************/
{
    FILE   *iunit ;
    char    segnam [ 21] ;
    size_t items_read ;
    TInt4   i, idef, iseg, nsub, nseg[5], ldum ;
    TInt4   headersize ;

/* In case of a DELWAQ map file or DELPAR plo file, we do not
   return any names
*/
    *ierror = IEOK ;
    if ( *ftype == ITDLWM || *ftype == ITDLPP )
    {
       *nrlst = 0 ;
       return ;
    }

/* Else read the names from the file (third record)
*/
    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

   /* skip  DELWAQ run id:                                            */

    SkipDlwRunId (iunit, *ftype, &nsub, nseg, &headersize, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

   /* skip  DELWAQ substances                                         */

    SkipDlwHSubs (iunit, nsub, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return ;
    }

    *nrlst = 0 ;
    *ierror = IEOK ;

    for ( i = 1; i <= nseg[0]; i++)
    {

       /*   Read DELWAQ segment number and segment name from file      */

        items_read = fread ( &iseg, (size_t) 1, (size_t) 4, iunit) ;
        if ( ferror ( iunit) || feof ( iunit) || ( items_read != 4))
        {
            if ( feof ( iunit) || ( items_read != 4))
                *ierror = IEUEOF ;
            else
                itrans ( ferror ( iunit), (TInt4 *) ierror) ;
            CLOSFL ( fname, &ldum);
            return ;
        }
        items_read = fread ( segnam, (size_t) 1, (size_t) 20, iunit) ;
        if ( ferror ( iunit) || feof ( iunit) || ( items_read != 20))
        {
            if ( feof ( iunit) || ( items_read != 20))
                *ierror = IEUEOF ;
            else
                itrans ( ferror ( iunit), (TInt4 *) ierror) ;
            CLOSFL ( fname, &ldum);
            return ;
        }
        segnam [ 20] = '\0' ;

        idef = 0 ;
        while ( idef < maxdef)
        {
            if ( equal ( locdef + ( idef * (PARLEN+1)), segnam))
            {
                if ( *nrlst < maxlst)
                {
                    strncpy ( loclst + ( *nrlst * (PARLEN+1)), segnam,
                             (size_t) PARLEN) ;
                    loclst [ ( *nrlst * (PARLEN+1)) + PARLEN] = '\0' ;
                    locnr[*nrlst] = i - 1 ;
                    (*nrlst)++ ;
                }
                else
                {
                    *ierror = IELMNY ;
                    CLOSFL ( fname, &ldum);
                    return ;
                }
                break ;
            }
            else
            {
                idef++ ;
            }
        }
    }
    CLOSFL ( fname, ierror);

    return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Parameter from DeLWaq Map/His file or DelPar PLO    */
/*************************************************************************/

void ODSGetParDlwq  ( char    *fname,
                      TInt4   *ftype,
                      char    *pardef,
                      TInt4   maxdef,
                      char    *parlst,
                      char    *paruni,
                      TInt4   maxlst,
                      TInt4   *nrlst,
                      TInt4   *partyp,
                      TInt4   *parcod,
                      TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of types of parameters found.       */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*                                                                       */
/*************************************************************************/
{
    FILE    *iunit ;
    char    subnam [ 21] ;
    char    *ip1, *ip2 ;
    size_t  items_read ;
    TInt4   i, idef, nsub, nsub2, nseg[5], ldum , nbal ;
    TInt4   rec_delim ;
    TInt4   *baltyp  , *balcod             ;
    TString list_par , list_bal , unit_bal ;
    TInt4   headersize ;

    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

   /* Balances files require special treatment */
    if ( *ftype == ODS_DELWAQ_BAL_BIN )
    {
       list_par = ReadDlwqSubs( iunit , &nsub , ierror ) ;
       list_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ;
       unit_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ;
       baltyp   = malloc( sizeof( TInt4 ) *          nsub ) ;
       balcod   = malloc( sizeof( TInt4 ) *          nsub ) ;

       IdentifyDlwqBalances( iunit    , nsub     , pardef , list_par ,
                             list_bal , unit_bal , balcod , baltyp   ,
                             &nbal    , ierror                       ) ;

       for ( i = 0 ; i < nbal ; i ++ )
       {
          if ( i < maxlst )
          {
             strncpy ( parlst   + ( i * (PARLEN+1) ) ,
                       list_bal + ( i * PARLEN )     ,
                       PARLEN                        )   ;
             *(parlst+i*(PARLEN+1)+PARLEN) = '\0'        ;
             strcpy( paruni + i * (PARLEN+1) , "-" )     ;
             parcod[i] = balcod[i]                       ;
             partyp[i] = baltyp[i]                       ;
          }
       }
       *nrlst = i ;

       free( list_par ) ;
       free( list_bal ) ;
       free( unit_bal ) ;
       free( baltyp   ) ;
       free( balcod   ) ;

       CLOSFL ( fname, ierror) ;
       return ;
    }


   /* skip  DELWAQ run id:  */

    SkipDlwRunId (iunit, *ftype, &nsub, nseg, &headersize, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

    *nrlst = 0 ;
    *ierror = IEOK ;

    /* DELPAR plo files give two extra parameters
    */
    nsub2 = nsub ;
    if ( *ftype == ITDLPP )
    {
       nsub2 = nsub + 2 ;
    }

    for ( i = 1; i <= nsub2; i++)
    {

       /*       Read DELWAQ parameter from file                        */

        if ( i <= nsub )
        {
           items_read = fread ( subnam, (size_t) 1, (size_t) 20, iunit) ;
           if ( ferror ( iunit) || feof ( iunit) || ( items_read != 20))
           {
              if ( feof ( iunit) || ( items_read != 20))
                 *ierror = IEUEOF ;
              else
                 itrans ( ferror ( iunit), (TInt4 *) ierror) ;
              fclose ( iunit) ;
              return ;
           }
           subnam [ 20] = '\0' ;
        }
        else
        {
           if ( i == nsub+1 ) strcpy( subnam , "XCOORD" ) ;
           if ( i == nsub+2 ) strcpy( subnam , "YCOORD" ) ;
        }

        idef = 0 ;
        while ( idef < maxdef)
        {
            if ( equal ( pardef + ( idef * PARLEN), subnam))
            {
                if ( *nrlst < maxlst)
                {

                   /* Assume DELWAQ names can have a unit between []      */
                   /* or between () , EAV 27/6/94 */

                    ip1 = strchr ( subnam, (int) '[') ;
                    ip2 = strchr ( subnam, (int) ']') ;
                    if (ip1 == NULL)
                    {
                        ip1 = strchr ( subnam, (int) '(') ;
                        ip2 = strchr ( subnam, (int) ')') ;
                    }
                   /* add the substance name to parlst */
                   /* add unit (or blanks, if no unit was found) to paruni */

                    if (( ip2 > ip1) && ( ip1 != NULL) && ( ip2 != NULL))
                    {
                        strncpy ( parlst + ( *nrlst * (PARLEN+1)), subnam,
                               (size_t) (ip1-subnam)) ;
                        strncpy ( paruni + ( *nrlst * (PARLEN+1)), ip1 + 1,
                               (size_t) (ip2-ip1-1)) ;
                        *(parlst+ ( *nrlst * (PARLEN+1)) + (ip1-subnam)) = '\0';
                        *(paruni+ ( *nrlst * (PARLEN+1)) + (ip2-ip1-1))  = '\0';
                    }
                    else
                    {
                        strncpy ( parlst + ( *nrlst * (PARLEN+1)), subnam,
                                  (size_t) PARLEN) ;
                        strncpy ( paruni + ( *nrlst * (PARLEN+1)),
                                    "                   ", (size_t) PARLEN) ;
                        *(parlst + ( *nrlst * (PARLEN+1)) + PARLEN) = '\0' ;
                        *(paruni + ( *nrlst * (PARLEN+1)) + PARLEN) = '\0' ;
                    }
                    *(parcod + *nrlst) = i;

                    /* for now return hard-wired parameter type codes */

                    if (*ftype == ODS_DELWAQ_HIS_BIN)
                    {
                       partyp[*nrlst] = ODS_PT_LOC_DEP | ODS_PT_LOC_MNK ;
                    }
                    else
                    {
                       if ( i <= nsub )
                       {
                          partyp[*nrlst] = ODS_PT_TIME_DEP | ODS_PT_LOC_MNK;
                       }
                       else
                       {
                          /* The extra parameters: special codes -2,-3 */
                          parcod[*nrlst] = nsub - i - 1 ;
                          partyp[*nrlst] = ODS_PT_LOC_MNK;
                       }
                    }

                    (*nrlst)++ ;
                }
                else
                {
                   /* too much parameters for array space, give up */

                    *ierror = IEPMNY ;
                    CLOSFL ( fname, &ldum) ;
                    return ;
                }
                break ;
            }
            else
                idef++ ;
        }
    }

    CLOSFL ( fname, ierror) ;

    return ;
}


/*************************************************************************/
/*    SUBROUTINE Get Times from DELWAQ HIS/MAP file or DELPAR PLO        */
/*************************************************************************/

void ODSGetTmeDlwq ( char    * fname,
                     TInt4   *ftype,
                     TReal8  *timin,
                     TInt4   maxiti,
                     TInt4   pardep,
                     TReal8  *tim,
                     TInt4   maxoti,
                     TInt4   *nrtim,
                     TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       pardep  -          I   Which parameter                          */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*                                                                       */
/*************************************************************************/
{
    FILE    *iunit ;
    size_t  items_read ;
    TInt4   nsub, nseg[5], itim, iftime ;
    TInt4   file_pos, istep, ldum;
    TReal8  dlwtim, dt0, ftime ;
    TInt4   some_lower ;
    int     seek_err ;
    TInt4   headersize ;

    *ierror = IEOK ;

    /* Special parameters for DELPAR PLO files */
    if ( pardep < 0 )
    {
       *nrtim = 0 ;
       return ;
    }

    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

   /* get DELWAQ run id:                               */

    GetDlwRunId (iunit, *ftype, &dlwtim, &dt0, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

   /* make sure we are at the right position for reading nsub etc.    */

    SkipDlwRunId (iunit, *ftype, &nsub, nseg, &headersize, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

   /* now loop over the timesteps in the file */

    *nrtim = 0 ;
    istep = 0 ;

    while ( ! feof ( iunit))
    {
/* Skip the first three records
*/
        file_pos = headersize ;

/* Now jump to the current record
*/
        file_pos += istep * 4L * (1+nseg[0]*nsub) + (1+2*istep)*DELIM;

        if ( ( seek_err = fseek ( iunit, file_pos, SEEK_SET) ) )
        {
            itrans ( -1 , (TInt4 *) ierror) ;
            CLOSFL ( fname, &ldum) ;
            return ;
        }

        istep++ ;
        items_read = fread ( &iftime, (size_t) 1, (size_t) 4, iunit) ;
        if ( ferror ( iunit) )
        {
            itrans ( ferror ( iunit), (TInt4 *) ierror) ;
            CLOSFL ( fname, &ldum) ;
            return ;
        }
        if ( feof ( iunit) || ( items_read != 4))
        {
            break ;
        }
        ftime = dlwtim + (iftime * dt0 / 86400.0) ;

       /* check if it is still possible that we find something */

        some_lower = FALSE ;
        for ( itim = 0; (itim < maxiti) && !some_lower ; itim++)
        {
            if ( ftime <= *(timin + 2*itim + 1))
                some_lower = TRUE ;
        }
        if ( ! some_lower)
            break ;

        for ( itim = 0; itim < maxiti; itim++)
        {
            if (( ftime >= *(timin + 2*itim)) &&
                ( ftime <= *(timin + 2*itim + 1)))
            {
                if ( *nrtim < maxoti)
                {
                    *(tim + *nrtim) = ftime ;
                    (*nrtim)++ ;
                    break ;
                }
                else
                {
                    *nrtim  = maxoti ;
                    *ierror = IETMNY ;
                    CLOSFL ( fname, &ldum) ;
                    return;
                }
            } /* timin1 < ftime < timin2  */
        }  /* for itim */
    }

    CLOSFL ( fname, ierror) ;

    return;
}

/*--------------------------------------------------------------------
** SUBROUTINE Get Dimensions from DeLWaq His/Map file or DELPAR PLO
**--------------------------------------------------------------------
*/

void ODSGetDimDlwq ( char    *fname,
                     TInt4   *ftype,
                     char    *dim,
                     TInt4   *pardep,
                     TInt4   *ndim,
                     TInt4   *ierror)

/*-----------------------------------------------------------------------*/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       dim     -          I   Dimension wanted (loc, par, tim)         */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definitions.    */
/*       ndim    4          O   dimension. [0]=n-dim [1]=x [2]=y [3]=z   */
/*       pardep  1          I   which parameter?                         */
/*                                                                       */
/*-----------------------------------------------------------------------*/
{
    FILE    *iunit                                    ;
    TInt4   i, nsub, nseg[5], nsteps , nobytes , nbal ;
    TInt4   *baltyp  , *balcod                        ;
    TString list_par , list_bal , unit_bal            ;
    TChar   pardef[PARLEN+1]                          ;
    TInt4   ldum                                      ;
    TInt4   headersize ;

    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

    SkipDlwRunId(iunit, *ftype, &nsub, nseg, &headersize, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

    switch (dim[0])
    {
        case 'p':  /* par */
        case 'P':
        ndim[0] = (TInt4)1;
        ndim[1] = nsub; /* nr of substances */
        if ( *ftype == ITDLPP )
        {
           ndim[1] = nsub + 2 ;
        }
        if ( *ftype == ITDLWB )
        {
           list_par = ReadDlwqSubs( iunit , &nsub , ierror )    ;
           list_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ; /* !! */
           unit_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ;
           baltyp   = malloc( sizeof( TInt4 ) *          nsub ) ;
           balcod   = malloc( sizeof( TInt4 ) *          nsub ) ;

           /* Get the list of balances */
           strcpy( pardef , "*" ) ;
           IdentifyDlwqBalances( iunit    , nsub     , pardef , list_par ,
                                 list_bal , unit_bal , balcod , baltyp   ,
                                 &nbal    , ierror                       ) ;

           /* Get the specific list, if necessary */
           if ( *pardep != 0 ) /* TODO: Check this value! */
           {
              for ( i = 0 ; i < nbal ; i ++ )
              {
                 if ( balcod[i] == *pardep )
                 {
                    strncpy( pardef , list_bal + i * PARLEN , PARLEN ) ;
                    pardef[PARLEN] = '\0' ;
                 }
              }
              IdentifyDlwqBalances( iunit    , nsub     , pardef , list_par ,
                                    list_bal , unit_bal , balcod , baltyp   ,
                                    &nbal    , ierror                       ) ;
           }

           free( list_par ) ;
           free( list_bal ) ;
           free( unit_bal ) ;
           free( baltyp   ) ;
           free( balcod   ) ;

           ndim[1] = nbal ;
        }
        break;

        case 'l':  /* loc */
        case 'L':
        if ( *ftype == ITDLWH || *ftype == ITDLWM || *ftype == ITDLWB )
        {
           ndim[0] = (TInt4)1;
           ndim[1] = nseg[0]; /* nr of locations/segments */
        }
        if ( *ftype == ITDLPP )
        {
           if ( *pardep >= 0 )
           {
              ndim[0] = (TInt4)3;
              ndim[1] = nseg[1]*nseg[2] ;
              ndim[2] = 1       ;
              ndim[3] = nseg[3] ;
           }
           else /* Special parameters: x- and y-coordinates */
           {
              if ( *pardep != -1 )
              {
                 ndim[0] = (TInt4)2;
                 ndim[1] = nseg[1] + 2 ; /* x-direction */
                 ndim[2] = nseg[2] + 2 ; /* y-direction */
              }
              else
              {
                 ndim[0] = (TInt4)4;
                 ndim[1] = nseg[1] + 2 ; /* x-direction */
                 ndim[2] = nseg[2] + 2 ; /* y-direction */
                 ndim[3] = 1           ; /* z-direction */
                 ndim[4] = ndim[1]*ndim[2] ; /* size of admin array */
              }
           }
        }

        break;

        case 't':  /* tim */
        case 'T':
        /* now calculate the number of timesteps */

        fseek( iunit , 0L , SEEK_END ) ;
        nsteps = ftell( iunit ) ;
        nsteps -= headersize ;

        nobytes = sizeof( TReal4 ) * nseg[0] * nsub + sizeof( TInt4 )
                  + 2*DELIM ;
        nsteps = nsteps / nobytes ; /* Number of completed records */

       /* now pass the number of steps back */
       /* if not a special parameter ! */
        ndim[0] = (TInt4)1;
        if ( *pardep > 0 )
        {
           ndim[1] = nsteps;
        }
        else
        {
           ndim[1] = 0;
        }
        break;

        default:
        *ierror = IEOTHR;
        CLOSFL (fname, &ldum);
        return;
   }

   CLOSFL (fname, ierror);
   return ;
}

/*--------------------------------------------------------------------
** SUBROUTINE Get Matrix from DeLWaq His/Map file or DELPAR PLO
**--------------------------------------------------------------------
*/

void ODSGetMatDlwq ( char    *fname,
                     TInt4   *ftype,
                     TInt4   *parcod,
                     TReal8  *tim,
                     TInt4   *loc,
                     float   *misval,
                     TInt4   *maxdim,
                     float   *values,
                     TInt4   *ierror)

/*-----------------------------------------------------------------------*/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       parcod  -          I   Parameter (code) of data to be retrieved.*/
/*       tim     3          I   date start, stop, step (Julian)          */
/*       loc     3*3        I   location of data subset to be retrieved: */
/*                                        [,0] X start, stop, step       */
/*                                        [,1] Y start, stop, step       */
/*                                        [,2] Z start, stop, step       */
/*       misval  -          I   Missing value.                           */
/*       maxdim  -          I   Size of values array.                    */
/*       values  -          O   Retreived data.                          */
/*       ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                       */
/*-----------------------------------------------------------------------*/
{
    FILE    *iunit                                               ;
    TInt4   i          , iloc     , ldum                         ;
    size_t  items_read                                           ;
    TInt4   nsub       , npar     , nseg[5]  , iftime     , isub ;
    TInt4   file_pos   , nw_pos   , istep    , offs              ;
    TReal8  dlwtim     , dt0      , ftime    , fcurtime          ;
    TInt4   start_loc  , end_loc  , step_loc , no_loc_lay        ;
    TReal4  *val                                                 ;
    TString list_par   , list_bal , unit_bal                     ;
    TChar   pardef[PARLEN+1]                                     ;
    TInt4   *baltyp    , *new_cod                                ;
    int     seek_err                                             ;
    TInt4   headersize ;

    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

   /* Set all out-matrix elements to missing value */

    for ( iloc = 0; iloc < *maxdim; iloc++)
       *(values + (iloc)) = *misval ;

   /* get DELWAQ run id:                               */

    GetDlwRunId (iunit, *ftype, &dlwtim, &dt0, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

   /* make sure we are at the right position for reading nsub etc.    */

    SkipDlwRunId (iunit, *ftype, &nsub, nseg, &headersize, ierror);
    if (*ierror != IEOK)
    {
        CLOSFL ( fname, &ldum);
        return;
    }

   /* take care of the special parameters for DELPAR PLO files */
    if ( *ftype == ITDLPP && *parcod < 0 )
    {
       GetDelparPloXY( iunit , loc , nseg , *parcod , values , ierror ) ;
       CLOSFL ( fname, &ldum);
       return ;
    }

/* Allocate an array to hold all parameter values for one segment
*/
    val = (TReal4 *) malloc( sizeof( TReal4 ) * nsub ) ;

/* Now loop over the timesteps in the file
   The dataset can have layers, but second dimension is always 1
*/

/* Before the advent of the new-style PLO-files ... */
#if 0
    if ( *ftype == ITDLWH || *ftype == ITDLWM || *ftype == ITDLWB )
    {
       no_loc_lay = loc[1] - loc[0] + 1 ;
       start_loc  = loc[0] + no_loc_lay * loc[6] ;
       end_loc    = loc[1] + no_loc_lay * loc[7] ;
       step_loc   = loc[2];
    }
    if ( *ftype == ITDLPP ) /* Not entirely thorough! */
    {
       start_loc = loc[0] ;
       end_loc   = ( loc[1] + 1 ) * ( loc[4] + 1 ) - 1 ;
       step_loc  = loc[2] ;
    }
#else
    if ( *ftype != ITDLPP || loc[4] == 0 ) /* Second dimension: 1 */
    {
       no_loc_lay = loc[1] - loc[0] + 1 ;
       start_loc  = loc[0] + no_loc_lay * loc[6] ;
       end_loc    = loc[1] + no_loc_lay * loc[7] ;
       step_loc   = loc[2];
    }
    else
    {
       /* Old-style PLO-files, ancient state files */
       start_loc = loc[0] ;
       end_loc   = ( loc[1] + 1 ) * ( loc[4] + 1 ) - 1 ;
       step_loc  = loc[2] ;
    }
#endif

/* The parameter may represent a set of parameters in the case of balances
*/
    if ( *ftype != ITDLWB )
    {
       npar       = 1                                         ;
       new_cod    = malloc( sizeof( TInt4 ) *          npar ) ;
       new_cod[0] = *parcod - 1                               ;
    }
    else
    {
       /* If the parameter code is larger/equal 10000, it represents
          a set of parameters
       */
       if ( *parcod < 10000 )
       {
          npar       = 1                                         ;
          new_cod    = malloc( sizeof( TInt4 ) *          npar ) ;
          new_cod[0] = *parcod - 1                               ;
       }
       else
       {
          list_par = ReadDlwqSubs( iunit , &nsub , ierror )    ;
          list_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ; /* !! */
          unit_bal = malloc( sizeof( TChar ) * PARLEN * nsub ) ;
          baltyp   = malloc( sizeof( TInt4 ) *          nsub ) ;
          new_cod  = malloc( sizeof( TInt4 ) *          nsub ) ;

          /* Get the list of balances */
          strcpy( pardef , "*" ) ;
          IdentifyDlwqBalances( iunit    , nsub     , pardef  , list_par ,
                                list_bal , unit_bal , new_cod , baltyp   ,
                                &npar    , ierror                        ) ;

          /* Get the specific list */
          for ( i = 0 ; i < npar ; i ++ )
          {
             if ( new_cod[i] == *parcod )
             {
                strncpy( pardef , list_bal + i * PARLEN , PARLEN ) ;
                pardef[PARLEN] = '\0' ;
             }
          }
          IdentifyDlwqBalances( iunit    , nsub     , pardef  , list_par ,
                                list_bal , unit_bal , new_cod , baltyp   ,
                                &npar    , ierror                        ) ;

          for ( i = 0 ; i < npar ; i ++ )
          {
             new_cod[i] -- ; /* Correct the index! */
          }
          free( list_par ) ;
          free( list_bal ) ;
          free( unit_bal ) ;
          free( baltyp   ) ;
       }
    }
    offs      = 0;

    ftime     = -1.0;
    fcurtime  = tim[0];

    for (istep = 0; ftime <= tim[1]; istep++ )
    {
/* Skip the first few records
*/
        file_pos = headersize ;

/* Jump to the current record
*/
        file_pos += istep * 4L * ( 1L + nseg[0] * nsub ) +
                    (1+2*istep)*DELIM ;

        if ( ( seek_err = fseek ( iunit, file_pos, SEEK_SET) ) )
        {
            itrans ( -1 , (TInt4 *) ierror) ;
            CLOSFL (fname, &ldum);
            break ;
        }

        items_read = fread ( &iftime, (size_t) 1, (size_t) 4, iunit) ;
        if ( ferror ( iunit) || feof( iunit ) || items_read != 4 )
        {
            itrans ( ferror ( iunit), (TInt4 *) ierror) ;
            CLOSFL (fname, &ldum);
            break ;
        }
        if ( feof ( iunit) || ( items_read != 4))
            break ;

        ftime = dlwtim + (iftime * dt0 / 86400.0) ;

      /* check if this timestep is in wanted range
         Here we have a problem - solve it ad hoc
      */
      /* This does not quite work: tim[2] may be set to 1.0!

        if (ftime < fcurtime - 0.1*tim[2] || ftime > fcurtime + 0.1*tim[2] )
            continue;

        Instead use the fact that DELWAQ and DELPAR use seconds as
        the unit of time
      */
        if (ftime < fcurtime - 1.0e-5 || ftime > fcurtime + 1.0e-5 )
        {
           continue ;
        }
            /* not a correct value for given stepsize */

      /* current timestep matches, get data */
        for ( iloc = start_loc ; (iloc <= end_loc) && (offs < *maxdim) ;
            iloc += step_loc )
        {
            nw_pos = file_pos + 4 + 4 * ( iloc * nsub ) ;
            if ( ( seek_err = fseek ( iunit, nw_pos, SEEK_SET) ) )
            {
                itrans ( -1 , (TInt4 *) ierror) ;
                CLOSFL (fname, &ldum);
                return ;
            }

            items_read = fread ( val, sizeof( TReal4 ) , (size_t) nsub, iunit) ;
            if ( ferror ( iunit) || feof ( iunit) )
            {
                itrans ( ferror ( iunit), (TInt4 *) ierror) ;
                CLOSFL (fname, &ldum);
                break ;
            }

            /* Copy the correct values
               Note: skip the special parameters!
            */
            for ( i = 0 ; i < npar ; i ++ )
            {
               if ( new_cod[i] < nsub )
               {
                  *(values +(offs)) = val[new_cod[i]] ;
               }
               offs ++ ;
            }

        }  /* for iloc */

        fcurtime += tim[2];
    }

/* For DELPAR PLO files: a zero means missing value
   Not anymore - ARS 6554
*/
#if 0
    if ( *ftype == ITDLPP )
    {
       for ( iloc = 0 ; iloc < *maxdim ; iloc ++ )
       {
          if ( values[iloc] == 0.0 )
          {
             values[iloc] = *misval ;
          }
       }
    }
#endif

/* Free the arrays and close the file
*/
    free( new_cod ) ;
    free( val     ) ;

    CLOSFL (fname, ierror);
    return ;
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/*------------------------------------------------------------------------
 * subroutine for retrieving runID from header of DELWAQ .his or .map file
 *   file should already be open
 *------------------------------------------------------------------------
 */
static void GetDlwRunId( FILE    *fp,
                         TInt4   ftype,
                         TReal8  *dlwtim,
                         TReal8  *dlwtm0,
                         TInt4   *ierror )
{
    size_t   items_read ;
    TInt4    iyear0, imonth0, iday0, ihour0, imin0, isec0, idt0 ;
    TInt4    nsub     ;
    TInt4    ref_date[3] ;
    char     runid[41];
    int      seek_err ;
    long     newpos   ;


    if ( ftype == ITDLWM || ftype == ITDLWH || ftype == ITDLWB )
    {
       if ( ( seek_err = fseek ( fp, 120L+DELIM, SEEK_SET) ) )
       {
          itrans ( -1 , (TInt4 *) ierror) ;
          return ;
       }
       *dlwtim = 0.0 ;
       *dlwtm0 = 86400.0 ;

   /* read DELWAQ header hoping to find a start time */

   /*   t0= 1993.12.31 12:59:59 (scu =12345678 )   */
   /*   01234567890123456789012345678901234567890  */
   /*             1         2         3         4  */

       items_read = fread ( &runid, (size_t) 1, (size_t) 40, fp) ;
       if ( ferror ( fp) || feof ( fp) || ( items_read != 40))
       {
          if ( feof ( fp ) || ( items_read != 40))
              *ierror = IEUEOF ;
          else
              itrans ( ferror ( fp ), (TInt4 *) ierror) ;
          return ;
       }
       runid [  8] = '\0' ;
       runid [ 11] = '\0' ;
       runid [ 14] = '\0' ;
       runid [ 17] = '\0' ;
       runid [ 20] = '\0' ;
       runid [ 23] = '\0' ;
       runid [ 38] = '\0' ;
       iyear0  = atol ( runid + 4) ;
       imonth0 = atol ( runid + 9) ;
       iday0   = atol ( runid +12) ;
       ihour0  = atol ( runid +15) ;
       imin0   = atol ( runid +18) ;
       isec0   = atol ( runid +21) ;
       idt0    = atol ( runid +30) ;

/* If the time scale factor is not given ...
*/
       if ( idt0 == 0 )
       {
          idt0 = 1 ;
       }

       if (( iyear0 < 1800) || ( imonth0 < 1) || ( iday0 < 1) ||
           ( ihour0 <    0) || ( imin0   < 0) || ( isec0 < 0) ||
           ( idt0   <    0))
       {
 /*       *dlwtim = 0.0 ; */
          idt0    = 1 ;
          iyear0  = 1900L;
          imonth0 = 1L;
          iday0   = 1L;
          ihour0  = 0L;
          imin0   = 0L;
          isec0   = 0L;
       }
    }

    if ( ftype == ITDLPP )
    {
       fseek ( fp, 160L+3*DELIM, SEEK_SET) ;
       items_read = fread ( &nsub, (size_t) 1, (size_t) 4, fp) ;

       if ( nsub > 0 )
       {
          newpos = 160 + 3*4 ;
       }
       else
       {
          newpos = 160 + 5*4 ;
       }
       newpos += 3*DELIM ;

       if ( ( seek_err = fseek ( fp, newpos, SEEK_SET) ) )
       {
          itrans ( -1 , (TInt4 *) ierror) ;
          return ;
       }
       *dlwtim = 0.0 ;
       *dlwtm0 = 86400.0 ;

       items_read = fread ( ref_date, sizeof( TInt4 ) , 3 , fp) ;
       if ( ferror ( fp ) || feof ( fp ) )
       {
          if ( feof ( fp ) )
              *ierror = IEUEOF ;
          else
              itrans ( ferror ( fp ), (TInt4 *) ierror) ;
          return ;
       }
       iyear0  = ref_date[0] ;
       imonth0 = ref_date[1] ;
       iday0   = ref_date[2] / 86400 ;
       ihour0  = ( ref_date[2] - iday0 * 86400 ) / 3600 ;
       imin0   = ( ref_date[2] - iday0 * 86400 - ihour0 * 3600 ) / 60 ;
       isec0   = ( ref_date[2] - iday0 * 86400 - ihour0 * 3600 - imin0 * 60 ) ;
       idt0    = 1L;
    }

    *dlwtm0 = (TReal8) idt0 ;
    julian ( (TInt4 *) &iyear0, (TInt4 *) &imonth0, (TInt4 *) &iday0,
        (TInt4 *) &ihour0, (TInt4 *) &imin0,   (TInt4 *) &isec0,
        (TReal8 *) dlwtim ) ;

    if ( *dlwtim == -1. )
    {
      idt0    = 1 ;
      iyear0  = 1900L;
      imonth0 = 1L;
      iday0   = 1L;
      ihour0  = 0L;
      imin0   = 0L;
      isec0   = 0L;
      *dlwtm0 = (TReal8) idt0 ;
      julian ( (TInt4 *) &iyear0, (TInt4 *) &imonth0, (TInt4 *) &iday0,
        (TInt4 *) &ihour0, (TInt4 *) &imin0,   (TInt4 *) &isec0,
        (TReal8 *) dlwtim ) ;
    }

    return;
}


/*------------------------------------------------------------------------
 * subroutine for skipping runID in header of DELWAQ .his or .map file
 *   file should already be open
 *
 * Notes:
 * This routine leaves the file pointer at the end of the first records
 * (just before the start of the substance names - after the record
 * delimiter, so that the next byte belongs to the substance names).
 * The parameter headersize measures the number of bytes in the file
 * before the first record with actual data (not including the record
 * delimiter).
 * The header is organised as follows:
 * Record 1: 4*40 characters of text (containing the T0-string)
 * Record 2: - for DELWAQ/DELPAR map/history files: 2 numbers
 *           - for DELPAR PLO and PSF files:
 *             - Old version: 6 numbers
 *             - New version: 8 numbers (the first is -1 to indicate "new")
 * Record 2a: PLO/PSF only, 4 numbers, window coordinates
 * Record 3: Names of the substances - 20*nosys
 * Record 4: (Present in DELWAQ/DELPAR history files only)
 *           Names of the locations - 20*noseg
 *------------------------------------------------------------------------
 */
static void SkipDlwRunId( FILE    *fp,
                          TInt4   ftype,
                          TInt4   *nsub,
                          TInt4   *nseg,
                          TInt4   *headersize,
                          TInt4   *ierror)
{
    TInt4   nodims ;
    size_t  items_read ;
    int     seek_err ;

    *ierror = IEOK;

    if ( ( seek_err = fseek ( fp, 160L+3*DELIM, SEEK_SET) ) )
    {
        itrans ( -1 , (TInt4 *) ierror) ;
        return ;
    }

    items_read = fread ( nsub, (size_t) 1, (size_t) 4, fp) ;
    if ( ferror ( fp) || feof ( fp) || ( items_read != 4))
    {
        if ( feof ( fp ) || ( items_read != 4))
            *ierror = IEUEOF ;
        else
            itrans ( ferror ( fp ), (TInt4 *) ierror) ;
        return ;
    }
    if ( ftype == ITDLWH || ftype == ITDLWM || ftype == ITDLWB )
    {
       *headersize = 160 + 8 + 2*2*DELIM ;
       nodims  = 1 ;
       nseg[2] = 1 ;
       nseg[3] = 1 ;
    }
    if ( ftype == ITDLPP )
    {
       /* ARS 6525: PLO/PSF file new style uses a -1 to mark the difference
       */
       if ( *nsub == -1 )
       {
          *headersize = 160 + 13*4 + 3*2*DELIM ;
          items_read = fread ( nsub, (size_t) 1, (size_t) 4, fp) ;
          nodims = 3 ;
       }
       else
       {
          *headersize = 160 + 11*4 + 3*2*DELIM ;
          nodims  = 2 ;
          nseg[3] = 1 ;
       }
    }
    items_read = fread( &nseg[1], nodims , sizeof( TInt4 ) , fp ) ;

    nseg[0] = nseg[1] * nseg[2] * nseg[3] ;

    if ( ftype == ITDLPP )
    {
       /* Exchange the two dimensions, because of the storage in file */
       nodims  = nseg[1] ;
       nseg[1] = nseg[2] ;
       nseg[2] = nodims  ;

       /* Skip the 8 extra items - and the extra record */

       fseek( fp , 8*4L+2*DELIM , SEEK_CUR ) ;
    }

    if ( DELIM > 0 )
    {
       fseek( fp , 2*DELIM , SEEK_CUR ) ;
    }

    if ( ferror ( fp) || feof ( fp) )
    {
        if ( feof ( fp ) )
            *ierror = IEUEOF ;
        else
            itrans ( ferror ( fp ), (TInt4 *) ierror) ;
        return ;
    }

   /*    There is no foolproof way to check if this realy is a DELWAQ */
   /*    3.x HIS file, but if the following holds, it certainly is    */
   /*    no DELWAQ MAP/HIS file.                                      */

    if (( *nsub < 1) || ( nseg[0] < 1))
    {
        *ierror = IETYPE ;
        return ;
    }
    (*headersize) = (*headersize) + (*nsub) * 20L + 2*DELIM;
    if ( ftype == ITDLWH || ftype == ITDLWB )
    {
       (*headersize) = (*headersize) + nseg[1] * 24L + 2*DELIM;
    }

    return ;
}


/*------------------------------------------------------------------------
 * subroutine for skipping substance names in header of DELWAQ HIS/MAP file
 * the file should already be open and positioned after runID in header
 *------------------------------------------------------------------------
 */
static void SkipDlwHSubs ( FILE    *fp,
                           TInt4   nsub,
                           TInt4   *ierror )
{
    int     seek_err ;
    *ierror = IEOK;

    if ( ( seek_err = fseek ( fp, (TInt4) (2*DELIM + nsub * 20), SEEK_CUR) ) )
    {
        itrans ( -1 , (TInt4 *) ierror) ;
    }
    return ;
}


/*------------------------------------------------------------------------
 * Subroutine for reading substance names in header of DELWAQ HIS/MAP file
 * The file should already be open.
 * The routine returns the number of substances and an allocated list of
 * their names. The calling routine should free this list.
 *
 * Note: this is for DELWAQ files only!!
 *------------------------------------------------------------------------
 */
static TString ReadDlwqSubs ( FILE    *fp,
                              TInt4   *nsub,
                              TInt4   *ierror )
{
    TString list_par ;
    long    pos      ;

    *ierror = IEOK;

/* Get the number of substances
*/
    pos = 4 * 40 + 3*DELIM ;
    fseek( fp   , pos , SEEK_SET  ) ;
    fread( nsub , sizeof( TInt4 ) , 1 , fp ) ;

    if ( feof( fp ) || ferror( fp ) )
    {
        *ierror = IEUEOF ; /* Might be a read error though! */
        return NULL ;
    }

/* Allocate the list; read it
*/
    list_par = (TString) malloc( sizeof( TChar ) * 20 * (*nsub + 1) ) ;
    list_par[20*(*nsub)] = '\0' ; /* Make sure we have null-character */

    pos = 4 * 40 + 2 * 4 + 5*DELIM ;
    fseek( fp   , pos , SEEK_SET  ) ;
    fread( list_par , sizeof( TChar ) , 20 * (*nsub) , fp ) ;

    if ( feof( fp ) || ferror( fp ) )
    {
        *ierror = IEUEOF ; /* Might be a read error though! */
        free( list_par ) ;
        return NULL ;
    }
    else
    {
       return list_par ;
    }
}

/*------------------------------------------------------------------------
 * Subroutine for identifying the names of mass balances:
 * They will be represented as parameters with a hierarchy
 * The routine returns the number of mass balances and the list of mass
 * balance names, codes and types.
 * The calling routine should allocate the lists.
 * Note:
 * - The list of balances can not be longer than the list of parameters
 *   So a safe bet for the size is the number of individual parameters
 * - Four lists should be allocated:
 *   - the list of balance names
 *   - the list of balance units (dummy)
 *   - the list of ODS codes for balances
 *   - the list of ODS types for balances
 *------------------------------------------------------------------------
 */
static TVoid IdentifyDlwqBalances( FILE    *fp      ,
                                   TInt4   nsub     ,
                                   TString pardef   ,
                                   TString list_par ,
                                   TString list_bal ,
                                   TString unit_bal ,
                                   TInt4   *balcod  ,
                                   TInt4   *baltyp  ,
                                   TInt4   *nbal    ,
                                   TInt4   *ierror  )
{
   TInt4   i      , j       , nobal ;
   TInt4   length , new_bal         ;
   TInt4   lengthc                  ;
   TString pstr   , plist   , pbal  ;
   TString pundsc                   ;

/* The routine has two modes:
   - Identify the balances (pardef="*"):
     The list of parameters will contain names like:
        12345678901234567890
        OXY       Mass
        OXY       Processes
        OXY       dREARoxy
        ...
     These should be grouped to "OXY balance"
     (An alternative, used with SOBEK balance output:
        "OXY _Storage"
     there the _ is the important character)
   - Identify the components of a balance (pardef="OXY balance" etc):
     Return the list of parameters that start with OXY- etc.
*/
   nobal = 0 ;
   for ( i = 0 ; i < nsub ; i ++ )
   {
      plist   = list_par + i * PARLEN ;
      pstr    = plist + 10            ;
      length  = 10                    ;
      lengthc = 10                    ;
      pundsc  = strchr( plist, '_' )  ;
      if ( pundsc != NULL )
      {
         lengthc = (int)(pundsc-plist) - 1 ;
      }
      new_bal = 1                     ;

      if ( strcmp( pardef , "*" ) == 0 )
      {
         for ( j = 0 ; j < nobal ; j ++ )
         {
            pbal = list_bal + j * PARLEN ;
            if ( strncmp( plist , pbal , lengthc ) == 0 )
            {
               new_bal = 0 ;
               break ;
            }
         }

         if ( new_bal == 1 )
         {
            pbal = list_bal + nobal * PARLEN ;
            strncpy( pbal, plist,        lengthc        ) ;
            pbal[lengthc] = '\0'             ;
            strncat( pbal, "          ", length-lengthc ) ;
            pbal[length] = '\0'              ;
            if ( pstr != NULL )
            {
               pbal[length] = '\0' ;
               strcat( pbal , "balance" ) ; /* Total should not exceed PARLEN
                                               characters - TODO */
            }
            pstr = unit_bal + nobal * PARLEN ;
            strcpy( pstr , "-" ) ;
            balcod[nobal] = ( nobal + 1 ) * 10000 ;
            baltyp[nobal] = ODS_PT_HIERACHY | ODS_PT_LOC_DEP | ODS_PT_LOC_MNK ;

            nobal ++ ;
         }
      }
      else
      {
         length = 10 ;

         if ( strncmp( plist , pardef , lengthc ) == 0 )
         {
            pbal = list_bal + nobal * PARLEN ;
            strncpy( pbal , plist , PARLEN ) ;
            pbal[PARLEN] = '\0'              ;

            pstr = unit_bal + nobal * PARLEN ;
            strcpy( pstr , "-" )             ;
            balcod[nobal] = i + 1            ; /* Possibly problematic - TODO */
            baltyp[nobal] = ODS_PT_LOC_DEP | ODS_PT_LOC_MNK ;

            nobal ++ ;
         }
      }
   }

/* We are done (some administration left to do)
*/
   *nbal = nobal ;
   return        ;
}

/*------------------------------------------------------------------------
 * subroutine for generating the coordinates for DELPAR PLO files
 *------------------------------------------------------------------------
 */
static void GetDelparPloXY( FILE    *fp,
                            TInt4   *loc,
                            TInt4   *nseg ,
                            TInt4   parcod ,
                            TReal4  *values ,
                            TInt4   *ierror )
{
    TInt4   marker[10] ;
    TReal4  xywin[4]   ;
    int     seek_err   ;
    long    newpos     ;
    TInt4   i , j  , ij  ;

    *ierror = IEOK;

/* Skip the preceeding items
   ARS 6525: a -1 for new files
*/
    newpos = 160L + 3*DELIM ;
    if ( ( seek_err = fseek ( fp, newpos, SEEK_SET) ) )
    {
        itrans ( -1 , (TInt4 *) ierror) ;
    }
    fread( marker, 1 , sizeof( TInt4 ) , fp ) ;
    if ( marker[0] < 0 )
    {
       fread( marker, 7 , sizeof( TInt4 ) , fp ) ;
    }
    else
    {
       fread( marker, 5 , sizeof( TInt4 ) , fp ) ;
    }

/* Get the extremes of the plot window
*/
    fread( xywin , 4 , sizeof( TReal4 ) , fp ) ;
    if ( ferror ( fp ) || feof( fp ) )
    {
       itrans ( ferror ( fp ), (TInt4 *) ierror) ;
       return ;
    }

/* Now calculate the coordinates

   Note (AM, dd 31 march 1999):
   Robert Vos noted that the coordinates were not properly
   calculated. There was an offset of -1.
*/
    xywin[1] = ( xywin[1] - xywin[0] ) / nseg[1] ;
    xywin[3] = ( xywin[3] - xywin[2] ) / nseg[2] ;
    ij = 0 ;
    for ( j = 0 ; j < nseg[2]+2 ; j ++ )
    {
       for ( i = 0 ; i < nseg[1]+2 ; i ++ )
       {
          if ( parcod == -2 ) /* x-coordinate */
          {
             values[ij] = xywin[0] + i * xywin[1] ;
          }
          else                /* y-coordinate */
          {
             values[ij] = xywin[2] + j * xywin[3] ;
          }
          ij ++ ;
       }
    }

    return ;
}


/*------------------------------------------------------------------------ */
/*    SUBROUTINE Get Grid from DeLPar Plo file                             */
/*------------------------------------------------------------------------ */

void ODSGetGrdDlpr( char    *fname,
                    TInt4   *ftype,
                    TInt4   *indloc,
                    TInt4   *indx,
                    TInt4   *nocell,
                    TInt4   *igisty,
                    TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       igisty  -          O   Type of geographic information           */
/*       indloc  -          I   Location index array (3*3) [IGNORED!]    */
/*       indx    -          O   Index array                              */
/*       nocell  1          O   Number of cells (model data) in grid etc.*/
/*                                                                       */
/*************************************************************************/
{
    FILE   *iunit ;
    TInt4   i, j , ij , iseg , nsub, nseg[5], ldum ;
    TInt4   headersize ;

/* Only for DELPAR PLO files
*/
    *ierror = IEOK ;
    if ( *ftype != ITDLPP )
    {
       *ierror = IETYPE ;
       return ;
    }

/* Open the file
*/
    (void) OdsOpenFile (fname, ftype, &iunit, ierror);
    if (*ierror != IEOK)
        return;

   /* skip  DELWAQ run id - this provides us the information we need  */

    SkipDlwRunId (iunit, *ftype, &nsub, nseg, &headersize, ierror);
    CLOSFL ( fname, &ldum);
    if (*ierror != IEOK)
    {
        return;
    }

   /* Construct the matrix of indices:
      Unfortunately, the matrix of values is stored in an awkward
      way, necessitating some index manipulation
    */

    ij   = 0 ;
    for ( j = 0 ; j < nseg[2]+2 ; j ++ )
    {
       for ( i = 0 ; i < nseg[1]+2 ; i ++ )
       {
          if ( i == 0 || i == nseg[1]+1 || j == 0 || j == nseg[2]+1 )
          {
             indx[ij] = 0 ;
          }
          else
          {
             indx[ij] = (i-1)*nseg[2] + j ; /* Note: +1 */
          }
          ij ++ ;
       }
    }

    /* Set the other parameters */
    *nocell = nseg[1] * nseg[2] ;
    *igisty = IGCURV ;

    return ;
}
/*------------------------- end of dlwbin.c --------------------------*/
