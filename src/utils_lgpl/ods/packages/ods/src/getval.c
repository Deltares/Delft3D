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
 *  getval.c  -  Read ODS 3-D matrix data from file
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/getval.c,v $
*/
/*
 *
 */


/*   Date:       28 Feb 1993                                          */
/*   Time:       11:52                                                */
/*   Program:    GETVAL.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 16:31 -- Operating System: DOS             */
/*   1.00 -- 5 Aug 1993 -- 16:21 -- Operating System: DOS             */
/*   0.00 -- 5 Aug 1993 -- 12:31 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetValue                                             */
/*   Function:                                                        */
/*   Comment: 1.03: Support for WASPRO files                          */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <stdio.h>

#ifndef NOPROT

/* function prototypes */

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif


/* the function to get values from 'any' file:   */

DllExport void getval ( char   *fname,  TInt4 *itype,  char  *locin,  char   *parin,
                        double *timin,  TInt4 *maxilo, TInt4 *maxipa, TInt4  *maxiti,
                        float  *misval, char  *loc,    char  *par,    double *tim,
                        float  *values, TInt4 *maxolo, TInt4 *maxopa, TInt4  *maxoti,
                        TInt4  *nrloc , TInt4 *nrpar,  TInt4 *nrtim , TInt4  *ierror)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   -          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        len.    -          I   UNIX only: length of CHARACTER variable */
/*        loc     maxolo     I   List of locations found.                */
/*        locin   maxilo     I   List of locations wanted.               */
/*        maxilo  -          I   Max. nr of wanted locations.            */
/*        maxipa  -          I   Max. nr of wanted parameters.           */
/*        maxiti  -          I   Max. nr of wanted time intervals.       */
/*        maxolo  -          I   Max. nr of locations to return.         */
/*        maxopa  -          I   Max. nr of parameters to return.        */
/*        maxoti  -          I   Max. nr of time intervals to return.    */
/*        misval  -          I   Real to use if no data found.           */
/*        nrloc   -          O   Nr of locations returned.               */
/*        nrpar   -          O   Nr of parameters returned.              */
/*        nrtim   -          O   Nr of times returned.                   */
/*        par     maxopa     I   List of parameters found.               */
/*        parin   maxipa     I   List of parameters wanted.              */
/*        tim     maxoti     I   List of times found.                    */
/*        timin   maxiti     I   List of time-intervals wanted.          */
/*        values  *              fortran: maxolo, maxopa, maxoti         */
/*                           O   Returned values.                        */
/*                                                                       */
/*************************************************************************/

   {
   int   i, inum ;
   char *fn      ;

   /* strip trailing blanks and add terminating 0 to all strings,
      just to make sure:
   */

   for (inum = 0 ; inum < ODS_FNAME_DIM ; inum++)
   {
       fn = &fname[inum*ODS_FILNAMLEN] ;

       fn[ODS_FILNAMLEN-1] = '\0' ;
       for ( i = ODS_FILNAMLEN-2 ; i >= 0 ; i -- )
       {
          if ( fn[i] == ' ')
          {
             fn[i] = '\0' ;
          }
          else
          {
             break ;
          }
       }
   }

   *nrloc = *nrpar = *nrtim = 0 ;

#  ifdef PRINTIT
#  ifndef MSWINDOWS
   printf ( "\n\n*** ARGUMENTS getval:\n");
   printf ( "filename       : %s\n", fname);
   printf ( "filetype       : %ld\n", *itype);
   printf ( "locin          : %s\n", locin);
   printf ( "parin          : %s\n", parin);
   printf ( "timin 1        : %12.2f\n", (double)*timin);
   printf ( "timin 2        : %12.2f\n", (double)*(timin+1));
#  endif
#  endif

   /*  Get filetype, if we don't know, first check extension:          */

   if ( *itype == ITUNDE)
      *itype = iftype ( fname) ;

   /*  Now proceed:                                                    */

   switch ( *itype)
      {
      case ITUNDE :

         /* Unable to determine filetype                               */

         *ierror = IEUNDE ;
         break ;

#ifdef PC
      case ITDBF3 :
      case ITDBF4 :

         /* DBASE 3 filetype                                           */

         ODSGetValDbf3 ( fname , locin , parin , timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   , par   ,
                         tim   , values, *maxolo, *maxopa, *maxoti,
                         nrloc , nrpar , nrtim , ierror) ;
         break ;
#endif

      case ODS_DELWAQ_MAP_BIN :

         /* DELWAQ 3 MAP file                                          */

         ODSGetValDlwm ( fname,   itype,   locin,   parin,   timin, *maxilo,
                         *maxipa, *maxiti, *misval, loc   ,  par   ,
                         tim   ,  values,  *maxolo, *maxopa, *maxoti,
                         nrloc ,  nrpar ,  nrtim ,  ierror) ;
         break ;

      case ODS_DELWAQ_HIS_BIN :

         /* DELWAQ 3 HIS file                                          */

         ODSGetValDlwh ( fname,   itype,   locin ,  parin ,  timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   ,  par   ,
                         tim   ,  values,  *maxolo, *maxopa, *maxoti,
                         nrloc ,  nrpar ,   nrtim , ierror) ;
         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

         ODSGetValWs1  ( fname , locin , parin , timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   , par   ,
                         tim   , values, *maxolo, *maxopa, *maxoti,
                         nrloc , nrpar , nrtim , ierror) ;
         break ;

      case ITWAS  :

         /* WASPRO WAS file                                            */

         ODSGetValWas  ( fname , locin , parin , timin , *maxilo,
                         *maxipa, *maxiti, *misval, loc   , par   ,
                         tim   , values, *maxolo, *maxopa, *maxoti,
                         nrloc , nrpar , nrtim , ierror) ;
         break ;

      case ITMPX  :

         /* MAPPIX file                                                */

         ODSGetValMPX ( fname , locin , parin , timin , *maxilo,
                        *maxipa, *maxiti, *misval, loc   , par   ,
                        tim   , values, *maxolo, *maxopa, *maxoti,
                        nrloc , nrpar , nrtim , ierror) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN ;
      }
   return ;
   }
