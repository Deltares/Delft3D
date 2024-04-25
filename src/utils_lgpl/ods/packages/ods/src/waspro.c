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
 *  waspro.c  -  ODS functions for WASPRO .WAS and .WS1 format files
 *
 *   Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:52p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/waspro.c,v $
*/
/*
 *
 */

/*   Date:       28 Feb 1994                                          */
/*   Time:       11:53                                                */
/*   Program:    LOKETJES.C                                           */
/*   Version:    1.03                                                 */
/*   Programmer: Andr  Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS            */
/*   0.00 -- 21 Jun 1993 -- 11:19 -- Operating System: DOS            */
/*   Project:     Open Data Structuur                                 */
/*   Module:      Loketjes                                            */
/*   Subroutines: glwas  Get locations from WASPRO WAS file           */
/*                glws1  Get locations from WASPRO WS1 file           */
/*                gpwas  Get parameters from WASPRO WAS file          */
/*                gpws1  Get parameters from WASPRO WS1 file          */
/*                gtwas  Get times from WASPRO WAS file               */
/*                gtws1  Get times from WASPRO WS1 file               */
/*                gvwas  Get values from WASPRO WAS file              */
/*                gvws1  Get values from WASPRO WS1 file              */
/*   Function:    "Loket" functions per filetype                      */
/*   Comment:     General version                                     */
/*                1.03: g?was and g?ws1 added.                        */
/*   Reference:   opendata.doc                                        */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "portable.h"
#include "ods.h"
#include "itrans.h"
#include "julian.h"
#include "equal.h"

#ifndef FALSE
#define FALSE   0
#define TRUE    1
#endif

#define MAXIBU  512

/*************************************************************************/
/*    SUBROUTINE Get Locations from waspro WAS file                      */
/*************************************************************************/

void ODSGetLocWas  ( char *fname,
                     char *locdef,
                    TInt4 maxdef,
                     char *loclst,
                    TInt4 maxlst,
                    TInt4 *nrlst,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       locdef  maxdef     I   List of parameters wanted.               */
/*       loclst  maxlst     O   List of parameters found.                */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   segnam [ 21] ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  iloc, ip, idef, nseg ;

   *ierror = IEOK ;
   *nrlst = 0 ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* check file type  */

      tmpstr [ 22] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 22, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF MUNICIPALITIES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* read nr. of locations  */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;

      /* skip header */

      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get locations from file */

      fgets ( tmpstr, 7, iunit) ;
      for ( iloc = 0; iloc < nseg; iloc++)
         {
         fgets ( tmpstr, 10, iunit) ;
         ip = 0 ;
         while ( tmpstr [ ip ] == ' ')
            {
            ip++ ;
            }
         strcpy ( segnam, tmpstr + ip ) ;

         idef = 0 ;
         while ( idef < maxdef)
            {
            if ( equal ( locdef + ( idef * PARLEN), segnam))
               {
               if ( *nrlst < maxlst)
                  {
                  strncpy ( loclst + ( *nrlst * PARLEN), segnam, (size_t) PARLEN) ;
                  loclst [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
                  (*nrlst)++ ;
                  }
               else
                  {
                  *ierror = IELMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }
         }

      fclose ( iunit) ;
      }
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Locations from waspro WS1 file                      */
/*************************************************************************/

void ODSGetLocWs1  ( char *fname,
                     char *locdef,
                    TInt4 maxdef,
                     char *loclst,
                    TInt4 maxlst,
                    TInt4 *nrlst,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       locdef  maxdef     I   List of parameters wanted.               */
/*       loclst  maxlst     O   List of parameters found.                */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  ipar, idef, nsub, nseg ;

   *ierror = IEOK ;
   *nrlst = 0 ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* check filetype */

      tmpstr [ 19] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF DISTRICTS  :") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* read nr. of locations */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;

      /* check filetype */

      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF FIELD TYPES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      fgets ( tmpstr, 20, iunit) ;
      nsub = atol ( tmpstr) + 1 ;   /* add 1 for type urban */

      fgets ( tmpstr, 150, iunit) ;

      /* get 'substances' from file */

      fgets ( tmpstr, 35, iunit) ;
      for ( ipar = 1; ipar < nsub; ipar++)
         {
         fgets ( tmpstr, 7, iunit) ;
         }

      /* skip line */

      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get locations and values from file */

      fgets ( tmpstr, 8, iunit) ;
      fgets ( tmpstr, 9, iunit) ;
      while ( strcmp ( tmpstr, "--------") != 0)
         {
         idef = 0 ;
         while ( idef < maxdef)
            {
            if ( equal ( locdef + ( idef * PARLEN), tmpstr))
               {
               if ( *nrlst < maxlst)
                  {
                  strncpy ( loclst + ( *nrlst * PARLEN), tmpstr, (size_t) PARLEN) ;
                  loclst [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
                  (*nrlst)++ ;
                  }
               else
                  {
                  *ierror = IELMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }

         /* skip total area */

         fgets ( tmpstr, 9, iunit) ;

         /* get urban area */

         fgets ( tmpstr, 10, iunit) ;

         /* get field areas */

         for ( ipar = 1; ipar < nsub; ipar++)
            {
            fgets ( tmpstr, 7, iunit) ;
            }
         fgets ( tmpstr, 150, iunit) ;
         fgets ( tmpstr, 8, iunit) ;
         fgets ( tmpstr, 9, iunit) ;
         }

      fclose ( iunit) ;
      }
   return ;

   }

/*************************************************************************/
/*    SUBROUTINE Get Parameter from waspro WAS file                      */
/*************************************************************************/

void ODSGetParWas  ( char *fname,
                     char *pardef,
                    TInt4 maxdef,
                     char *parlst,
                     char *paruni,
                    TInt4 maxlst,
                    TInt4 *nrlst,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  idef, nseg, iloc ;

   *ierror = IEOK ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* Be pessemistic, assume we will find nothing ...                 */

      *nrlst = 0 ;

      tmpstr [ 22] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 22, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF MUNICIPALITIES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* skip header */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;
      fgets ( tmpstr, 150, iunit) ;
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ("GELEZEN TMPSTR : >%s<\n", tmpstr) ;
#     endif
#     endif
      fgets ( tmpstr, 150, iunit) ;
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ("GELEZEN TMPSTR : >%s<\n", tmpstr) ;
#     endif
#     endif

      /* get locations from file */

      fgets ( tmpstr, 7, iunit) ;
      for ( iloc = 0; iloc < nseg; iloc++)
         fgets ( tmpstr, 10, iunit) ;

      /* skip line */

      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get parameters from file */

      fgets ( tmpstr, 8, iunit) ;
      while ( strcmp ( tmpstr, "-------") != 0)
         {
#        ifdef PRINTIT
#           ifndef MSWINDOWS
               printf ("GELEZEN TMPSTR (param): >%s<\n", tmpstr) ;
#           endif
#        endif
         idef = 0 ;
         while ( idef < maxdef)
            {
            if ( equal ( pardef + ( idef * PARLEN), tmpstr))
               {
               if ( *nrlst < maxlst)
                  {
                  strncpy ( parlst + ( *nrlst * PARLEN), tmpstr, (size_t) PARLEN) ;
                  parlst [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
                  strncpy ( paruni + ( *nrlst * PARLEN), " ", (size_t) PARLEN) ;
                  (*nrlst)++ ;
                  }
               else
                  {
                  *ierror = IEPMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }

         for ( iloc = 0; iloc < nseg; iloc++)
            {
            fgets ( tmpstr, 10, iunit) ;
            }
         fgets ( tmpstr, 150, iunit) ;
         fgets ( tmpstr, 8, iunit) ;
         }

      fclose ( iunit) ;
      }
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Parameter from waspro WS1 file                      */
/*************************************************************************/

void ODSGetParWs1  ( char *fname,
                     char *pardef,
                    TInt4 maxdef,
                     char *parlst,
                     char *paruni,
                    TInt4 maxlst,
                    TInt4 *nrlst,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   subnam [ 21] ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  idef, nsub, nseg, ipar ;
   int    ip ;

   *ierror = IEOK ;
   *nrlst = 0 ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* check header 1 */

      tmpstr [ 19] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ("GELEZEN TMPSTR (check1): >%s<\n", tmpstr) ;
#     endif
#     endif
      if ( strcmp ( tmpstr, "NO. OF DISTRICTS  :") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* read nr. of locations */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;

      /* check header 2 */

      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ("GELEZEN TMPSTR (check2): >%s<\n", tmpstr) ;
#     endif
#     endif
      if ( strcmp ( tmpstr, "NO. OF FIELD TYPES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* read nr. of parameters */

      fgets ( tmpstr, 20, iunit) ;
      nsub = atol ( tmpstr) + 1 ;  /*  add 1 for type 'urban'  */
      fgets ( tmpstr, 150, iunit) ;

      /* get 'substances' from file */

      idef = 0 ;
      while ( idef < maxdef)
         {
         if ( equal ( pardef + ( idef * PARLEN), "URBAN"))
            {
            strncpy ( parlst, "URBAN", (size_t) PARLEN) ;
            *nrlst = 1 ;
            break ;
            }
         else
            idef++ ;
         }
      fgets ( tmpstr, 35, iunit) ;
      for ( ipar = 1; ipar < nsub; ipar++)
         {
         fgets ( tmpstr, 7, iunit) ;
         ip = 0 ;
         while ( tmpstr [ ip ] == ' ')
            {
            ip++ ;
            }
         strcpy ( subnam, tmpstr + ip ) ;
         idef = 0 ;
         while ( idef < maxdef)
            {
            if ( equal ( pardef + ( idef * PARLEN), subnam))
               {
               if ( *nrlst < maxlst)
                  {
                  strncpy ( parlst+ ( *nrlst * PARLEN), subnam, (size_t) PARLEN) ;
                  parlst[ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
                  strncpy ( paruni+ ( *nrlst * PARLEN), " ", (size_t) PARLEN) ;
                  (*nrlst)++ ;
                  }
               else
                  {
                  *ierror = IEPMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }
         }

      fclose ( iunit) ;
      }
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Times from waspro WAS file                          */
/*************************************************************************/

void ODSGetTmeWas  ( char *fname,
                     double *timin,
                    TInt4 maxiti,
                     double *tim,
                    TInt4 maxoti,
                    TInt4 *nrtim,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  itim ;

   *ierror = IEOK ;
   *nrtim = 0 ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      tmpstr [ 22] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 22, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF MUNICIPALITIES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      for ( itim = 0; itim < maxiti; itim++)
         {
         if (( 0.0 >= *(timin + 2*itim)) &&
             ( 0.0 <= *(timin + 2*itim + 1)))
            {
            if ( *nrtim < maxoti)
               {
               *nrtim = 1 ;
               *tim = 0.0 ;
               }
            else
               *ierror = IETMNY ;
            break ;
            }
         }
      fclose ( iunit) ;
      }
   }

/*************************************************************************/
/*    SUBROUTINE Get Times from waspro WS1 file                          */
/*************************************************************************/

void ODSGetTmeWs1  ( char *fname,
                     double *timin,
                    TInt4 maxiti,
                     double *tim,
                    TInt4 maxoti,
                    TInt4 *nrtim,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  itim ;

   *ierror = IEOK ;
   *nrtim = 0 ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      tmpstr [ 19] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF DISTRICTS  :") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* skip header */

      fgets ( tmpstr, 150, iunit) ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF FIELD TYPES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }
      for ( itim = 0; itim < maxiti; itim++)
         {
         if (( 0.0 >= *(timin + 2*itim)) &&
             ( 0.0 <= *(timin + 2*itim + 1)))
            {
            if ( *nrtim < maxoti)
               {
               *nrtim = 1 ;
               *tim = 0.0 ;
               }
            else
               *ierror = IETMNY ;
            break ;
            }
         }
      fclose ( iunit) ;
      }
   }

/*************************************************************************/
/*    SUBROUTINE Get Values from waspro WAS file                         */
/*************************************************************************/

void ODSGetValWas ( char *fname,
                    char *locin,
                    char *parin,
                    double *timin,
                   TInt4 maxilo,
                   TInt4 maxipa,
                   TInt4 maxiti,
                    float misval,
                    char *loc,
                    char *par,
                    double *tim,
                    float *values,
                   TInt4 maxolo,
                   TInt4 maxopa,
                   TInt4 maxoti,
                   TInt4 *nrloc,
                   TInt4 *nrpar,
                   TInt4 *nrtim,
                   TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       loc     maxolo     O   Locations as found.                      */
/*       locin   maxilo     I   Asked locations.                         */
/*       maxilo  -          I   Nr. of location masks in input.          */
/*       maxipa  -          I   Nr. of parameter masks in input.         */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxolo  -          I   Max. nr. of locations in output.         */
/*       maxopa  -          I   Max. nr. of parameters in output.        */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       misval  -          I   Missing value indicator.                 */
/*       nrloc   -          O   Nr. of locations in output.              */
/*       nrpar   -          O   Nr. of parameters in output.             */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       par     maxopa     O   Parameters as found.                     */
/*       parin   maxipa     I   Asked parameters.                        */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*       values  maxolo*    O   Values found.                            */
/*               maxopa*                                                 */
/*               maxoti                                                  */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   segnam [ 21] ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  idef, nseg, iloc, ipar, itim ;
   int    ip, is_equal ;
   int    ibuffr [ MAXIBU] ;
   float  val ;

   *ierror = IEOK ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* Be pessemistic, assume we will find nothing ...                 */

      for ( iloc = 0; iloc < MAXIBU; iloc++) ibuffr [ iloc] = -1 ;
      for ( iloc = 0; iloc < maxolo; iloc++)
         for ( ipar = 0; ipar < maxopa; ipar++)
            for ( itim = 0; itim < maxoti; itim++)
               *(values + (itim * maxolo * maxopa +
                          ipar * maxolo +
                          iloc)) = misval ;
/*       fortran: values ( maxolo, maxopa, maxoti)                       */

      *nrloc = 0 ;
      *nrpar = 0 ;
      *nrtim = 0 ;

      tmpstr [ 22] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 22, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF MUNICIPALITIES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* skip header */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;
      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get locations from file */

      fgets ( tmpstr, 7, iunit) ;
      for ( iloc = 0; iloc < nseg; iloc++)
         {
         fgets ( tmpstr, 10, iunit) ;
         ip = 0 ;
         while ( tmpstr [ ip ] == ' ')
            {
            ip++ ;
            }
         strcpy ( segnam, tmpstr + ip ) ;
#        ifdef PRINTIT
#           ifndef MSWINDOWS
               printf ( "SEGNAM >%s<\n", segnam) ;
#           endif
#        endif
         idef = 0 ;
         while ( idef < maxilo)
            {
            if ( equal ( locin + ( idef * PARLEN), segnam))
               {
               if ( *nrloc < maxolo)
                  {
                  strncpy ( loc + ( *nrloc * PARLEN), segnam, (size_t) PARLEN) ;
                  loc [ ( *nrloc * PARLEN) + PARLEN] = '\0' ;
                  if ( *nrloc < MAXIBU)
                     {
                     ibuffr [ *nrloc] = (int) iloc ;
                     (*nrloc)++ ;
                     }
                  else
                     {
                     *nrloc  = MAXIBU ;
                     *ierror = IEBUFF ;
                     break ;
                     }
                  }
               else
                  {
                  *ierror = IELMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }
         }

      /* skip line */

      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get parameters from file */

      fgets ( tmpstr, 8, iunit) ;
      while ( strcmp ( tmpstr, "-------") != 0)
         {
         idef = 0 ;
         is_equal = FALSE ;
         while ( idef < maxipa)
            {
            if ( equal ( parin + ( idef * PARLEN), tmpstr))
               {
               if ( *nrpar < maxopa)
                  {
                  is_equal = TRUE ;
                  strncpy ( par + ( *nrpar * PARLEN), tmpstr, (size_t) PARLEN) ;
                  par [ ( *nrpar * PARLEN) + PARLEN] = '\0' ;
                  (*nrpar)++ ;
                  }
               else
                  {
                  *ierror = IEPMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }

         for ( iloc = 0; iloc < nseg; iloc++)
            {
            fgets ( tmpstr, 10, iunit) ;
            val = (float) atof ( tmpstr) ;
            if ( is_equal)
               {
               for ( idef = 0; idef < *nrloc; idef++)
                  {
                  if ( (int) iloc == ibuffr [ idef])
                     {
                     *(values + ( ( *nrpar - 1) * maxolo + idef)) = val ;
                     }
                  }
               }
            }
         fgets ( tmpstr, 150, iunit) ;
         fgets ( tmpstr, 8, iunit) ;
         }

      *nrtim = 1 ;
      *tim = 0.0 ;
      fclose ( iunit) ;
      }
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Values from waspro WS1 file                         */
/*************************************************************************/

void ODSGetValWs1 ( char *fname,
                    char *locin,
                    char *parin,
                    double *timin,
                   TInt4 maxilo,
                   TInt4 maxipa,
                   TInt4 maxiti,
                    float misval,
                    char *loc,
                    char *par,
                    double *tim,
                    float *values,
                   TInt4 maxolo,
                   TInt4 maxopa,
                   TInt4 maxoti,
                   TInt4 *nrloc,
                   TInt4 *nrpar,
                   TInt4 *nrtim,
                   TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       loc     maxolo     O   Locations as found.                      */
/*       locin   maxilo     I   Asked locations.                         */
/*       maxilo  -          I   Nr. of location masks in input.          */
/*       maxipa  -          I   Nr. of parameter masks in input.         */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxolo  -          I   Max. nr. of locations in output.         */
/*       maxopa  -          I   Max. nr. of parameters in output.        */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       misval  -          I   Missing value indicator.                 */
/*       nrloc   -          O   Nr. of locations in output.              */
/*       nrpar   -          O   Nr. of parameters in output.             */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       par     maxopa     O   Parameters as found.                     */
/*       parin   maxipa     I   Asked parameters.                        */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*       values  maxolo*    O   Values found.                            */
/*               maxopa*                                                 */
/*               maxoti                                                  */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *iunit ;
   char   ftype [] = "r" ;
   char   subnam [ 21] ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  idef, nsub, nseg, iloc, ipar, itim ;
   int    ip, is_equal ;
   int    ibuffr [ MAXIBU] ;
   float  val ;

   *ierror = IEOK ;

   iunit = fopen ( fname, ftype) ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      }
   else
      {

      /* Be pessemistic, assume we will find nothing ...                 */

      for ( iloc = 0; iloc < MAXIBU; iloc++) ibuffr [ iloc] = -1 ;
      for ( iloc = 0; iloc < maxolo; iloc++)
         for ( ipar = 0; ipar < maxopa; ipar++)
            for ( itim = 0; itim < maxoti; itim++)
               *(values + (itim * maxolo * maxopa +
                          ipar * maxolo +
                          iloc)) = misval ;
/*       fortran: values ( maxolo, maxopa, maxoti)                       */

      *nrloc = 0 ;
      *nrpar = 0 ;
      *nrtim = 0 ;

      tmpstr [ 19] = '\0' ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF DISTRICTS  :") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      /* skip header */

      fgets ( tmpstr, 20, iunit) ;
      nseg = atol ( tmpstr) ;
      items_read = fread ( &tmpstr, (size_t) 1, (size_t) 19, iunit) ;
      if ( strcmp ( tmpstr, "NO. OF FIELD TYPES:") != 0)
         {
         *ierror = IETYPE ;
         fclose ( iunit) ;
         return ;
         }

      fgets ( tmpstr, 20, iunit) ;
      nsub = atol ( tmpstr) + 1 ;   /* add 1 for type urban */

      fgets ( tmpstr, 150, iunit) ;

      /* get 'substances' from file */

      idef = 0 ;
      while ( idef < maxipa)
         {
         if ( equal ( parin + ( idef * PARLEN), "URBAN"))
            {
            strncpy ( par, "URBAN", (size_t) PARLEN) ;
            ibuffr [ 0] = 0 ;
            *nrpar = 1 ;
            break ;
            }
         else
            idef++ ;
         }
      fgets ( tmpstr, 35, iunit) ;
      for ( ipar = 1; ipar < nsub; ipar++)
         {
         fgets ( tmpstr, 7, iunit) ;
         ip = 0 ;
         while ( tmpstr [ ip ] == ' ')
            {
            ip++ ;
            }
         strcpy ( subnam, tmpstr + ip ) ;
         idef = 0 ;
         while ( idef < maxipa)
            {
            if ( equal ( parin + ( idef * PARLEN), subnam))
               {
               if ( *nrpar < maxopa)
                  {
                  strncpy ( par + ( *nrpar * PARLEN), subnam, (size_t) PARLEN) ;
                  par [ ( *nrpar * PARLEN) + PARLEN] = '\0' ;
                  if ( *nrpar < MAXIBU)
                     {
                     ibuffr [ *nrpar] = (int) ipar ;
                     (*nrpar)++ ;
                     }
                  else
                     {
                     *nrpar  = MAXIBU ;
                     *ierror = IEBUFF ;
                     break ;
                     }
                  }
               else
                  {
                  *ierror = IEPMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }
         }

      /* skip line */

      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;
      fgets ( tmpstr, 150, iunit) ;

      /* get locations and values from file */

      fgets ( tmpstr, 8, iunit) ;
      fgets ( tmpstr, 9, iunit) ;
      while ( strcmp ( tmpstr, "--------") != 0)
         {
         idef = 0 ;
         is_equal = FALSE ;
         while ( idef < maxilo)
            {
            if ( equal ( locin + ( idef * PARLEN), tmpstr))
               {
               if ( *nrloc < maxolo)
                  {
                  is_equal = TRUE ;
                  strncpy ( loc + ( *nrloc * PARLEN), tmpstr, (size_t) PARLEN) ;
                  loc [ ( *nrloc * PARLEN) + PARLEN] = '\0' ;
                  (*nrloc)++ ;
                  }
               else
                  {
                  *ierror = IELMNY ;
                  break ;
                  }
               break ;
               }
            else
               idef++ ;
            }

         /* skip total area */

         fgets ( tmpstr, 9, iunit) ;

         /* get urban area */

         fgets ( tmpstr, 10, iunit) ;
         val = (float) atof ( tmpstr) ;
         if ( is_equal)
            {
            if ( 0 == ibuffr [ 0])
               {
               *(values + ( *nrloc - 1)) = val ;
               }
            }

         /* get field areas */

         for ( ipar = 1; ipar < nsub; ipar++)
            {
            fgets ( tmpstr, 7, iunit) ;
            val = (float) atof ( tmpstr) ;
            if ( is_equal)
               {
               for ( idef = 0; idef < *nrpar; idef++)
                  {
                  if ( (int) ipar == ibuffr [ idef])
                     {
                     *(values + (idef * maxolo + *nrloc - 1)) = val ;
                     }
                  }

               }
            }
         fgets ( tmpstr, 150, iunit) ;
         fgets ( tmpstr, 8, iunit) ;
         fgets ( tmpstr, 9, iunit) ;
         }

      *nrtim = 1 ;
      *tim = 0.0 ;
      fclose ( iunit) ;
      }
   return ;
   }


