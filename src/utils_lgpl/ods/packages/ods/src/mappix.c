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
 *  mappix.c  -  ODS functions for MAPPIX files
 *
 *   Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/mappix.c,v $
*/
/*
 *
 */

/*   Project:     Open Data Structuur                                 */
/*   Module:      Mappix.c                                            */
/*   Subroutines: glocmappix   Get locations                          */
/*                gparmappix   Get parameters                         */
/*   Function:    "Loket" functions per filetype                      */
/*   Comment:     General version (using dynamic buffers)             */
/*   Reference:   opendata.doc                                        */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "portable.h"
#include "ods.h"
#include "itrans.h"
#include "julian.h"
#include "equal.h"

#ifndef FALSE
#define FALSE   0
#define TRUE    1
#endif

/* Nice patch for MS-Windows DLL's ! */
#ifdef MSWINDOWS
#   define sprintf wsprintf
    int _far __cdecl wsprintf(char _far* lpszOut, const char _far* lpszFmt, ...);
#endif


/*************************************************************************/
/*    SUBROUTINE Get Location from MAPPIX file                           */
/*************************************************************************/

void ODSGetLocMPX ( char *fname,
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
   char   locnam [PARLEN] ;
   size_t items_read ;
   short  i, idef, npar, nloc ,ntim ;
   short  recordlen, firstrecord ;

   iunit = fopen ( fname, "rb\0") ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      return ;
      }

   /* check if this really is a MAPPIX file                           */
   items_read = fread ( &locnam, (size_t) 4, (size_t) 1, iunit ) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   locnam[4]='\0' ;
   if ( strcmp (locnam,"REAL") != 0 )
     {
     *ierror = IETYPE ;
     fclose ( iunit ) ;
     return ;
     }

   /* determine file dimensions                                          */
   if ( fseek ( iunit, 24L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &nloc, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &ntim, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &npar, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   /* calculate record length and position                            */
   recordlen = 2 + 4 * nloc ;
   firstrecord = 2 + (short) (40.0 * (7 + npar) / recordlen) ;

   /* loop over all locations in the file                             */
   *nrlst = 0 ;
   *ierror = IEOK ;
   if ( fseek ( iunit, (firstrecord - 1) * recordlen, SEEK_SET)) goto HandleFileErr ;

   for ( i = 1; i <= nloc; i++)
      {

      /* Read MAPPIX segment number from file */
      items_read = fread ( &idef, (size_t) 2, (size_t) 1, iunit) ;
      if ( ferror ( iunit) | feof ( iunit) | ( items_read != 1)) goto HandleFileErr ;
      sprintf ( locnam, "%i", idef) ;

      /* Try to match the location */
      idef = 0 ;
      while ( idef < maxdef)
         {
         if ( equal ( locdef + ( idef * PARLEN), locnam))
            {
            if ( *nrlst < maxlst)
               {
               strncpy ( loclst + ( *nrlst * PARLEN), locnam, (size_t) PARLEN) ;
               loclst [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
               (*nrlst)++ ;
               }
            else
               {
               *ierror = IELMNY ;
               fclose ( iunit) ;
               return ;
               }
            break ;
            }
         else
            idef++ ;
         }
      }

   fclose ( iunit) ;
   return ;

HandleFileErr:

   if ( feof ( iunit) )
      *ierror = IEUEOF ;
   else
      itrans ( ferror ( iunit), (TInt4 *) ierror) ;

   fclose ( iunit) ;
   return ;

   }

/*************************************************************************/
/*    SUBROUTINE Get Parameter from MAPPIX file                          */
/*************************************************************************/

void ODSGetParMPX ( char *fname,
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
   char   parnam [41] ;
   char   pardim [41] ;
   size_t items_read ;
   short  i, idef, npar, nloc, ntim ;

   iunit = fopen ( fname, "rb\0") ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      return ;
      }

   /* check if this really is a MAPPIX file                           */
   items_read = fread ( &parnam, (size_t) 4, (size_t) 1, iunit ) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   parnam[4]='\0' ;
   if ( strcmp (parnam,"REAL") != 0 )
     {
     *ierror = IETYPE ;
     fclose( iunit ) ;
     return ;
     }

   /* determine file dimensions                                          */
   if ( fseek ( iunit, 24L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &nloc, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &ntim, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &npar, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   /* get MAPPIX parameter name and dimension                                         */
   if ( fseek ( iunit, 40L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( parnam, (size_t) 40, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   parnam[39]='\0' ;
   items_read = fread ( pardim, (size_t) 40, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   pardim[39]='\0' ;

   /* loop over series in the file */
   *nrlst = 0 ;
   *ierror = IEOK ;
   if ( fseek ( iunit, 120L, SEEK_SET) != 0) goto HandleFileErr ;

   for ( i = 1; i <= npar; i++)
      {

      /* Read MAPPIX parameter from file */
      if ( npar > 1 )
         {
         items_read = fread ( parnam, (size_t) 1, (size_t) 40, iunit) ;
         if ( ferror ( iunit) || feof ( iunit) || ( items_read != 40)) goto HandleFileErr ;
         parnam[PARLEN]='\0' ;
         }

      /* try to match the parameter */
      idef = 0 ;
      while ( idef < maxdef )
         {
         if ( equal ( pardef + ( idef * PARLEN), parnam))
            {
            if ( *nrlst < maxlst)
               {
               strncpy ( parlst + ( *nrlst * PARLEN), parnam, (size_t) PARLEN) ;
               strncpy ( paruni + ( *nrlst * PARLEN), pardim, (size_t) PARLEN) ;
               parlst [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
               paruni [ ( *nrlst * PARLEN) + PARLEN] = '\0' ;
               (*nrlst)++ ;
               }
            else
               {
               *ierror = IEPMNY ;
               fclose ( iunit) ;
               return ;
               }
            break ;
            }
         else
            idef++ ;
         }
      }
   fclose ( iunit) ;
   return ;

HandleFileErr:

   if ( feof ( iunit) )
      *ierror = IEUEOF ;
   else
      itrans ( ferror ( iunit), (TInt4 *) ierror) ;

   fclose ( iunit) ;
   return ;

   }

/*************************************************************************/
/*    SUBROUTINE Get Times from MAPPIX file                              */
/*************************************************************************/

void ODSGetTmeMPX ( char *fname,
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
   char   tsttype [9] ;
   short  npar, nloc, ntim, iftime, i ;
   TInt4  lng1, lng0, lngyear ;
   TInt4  recordlen, firstrecord ;
   TInt4  file_pos, itim ;
   double mpxtim, dt0, ftime ;
   int    some_lower ;
   int    ROUNDDAY, YEARSER ;
   size_t items_read ;

   *ierror = IEOK ;

   iunit = fopen ( fname, "rb\0") ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      return ;
      }

   /* check if this really is a MAPPIX file                           */
   items_read = fread ( &tsttype, (size_t) 4, (size_t) 1, iunit ) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   tsttype[4]='\0' ;
   if ( strcmp (tsttype,"REAL") != 0 )
     {
     *ierror = IETYPE ;
     fclose( iunit ) ;
     return ;
     }

   /* get name of time scale */
   if ( fseek ( iunit, 16L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &tsttype, (size_t) 8, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   tsttype[8]='\0' ;
   for (i=0 ; i<4 ; i++) tsttype[i] = (char) toupper ( (int) tsttype[i]) ;

   /* determine file dimensions                                          */
   if ( fseek ( iunit, 24L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &nloc, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &ntim, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &npar, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   /* calculate record length and position                            */
   recordlen = 2 + 4 * nloc ;
   firstrecord = 2 + (short) (40.0 * (7 + npar) / recordlen) ;

   /* attemp to set time scale                                        */
   mpxtim = 0.0 ;
   dt0=1 ;
   ROUNDDAY=TRUE ;
   YEARSER=FALSE ;
   if ( strcmp(tsttype,"DECADE  ") == 0 )
      dt0 = 365. / 36 ;
   else if ( strcmp(tsttype,"WEEKS   ") == 0 )
      dt0 = 365. / 52 ;
   else if ( strcmp(tsttype,"TWOWEEKS") ==0 )
      dt0 = 365. / 26 ;
   else if ( strstr(tsttype,"YEAR*") != NULL )
      {
      YEARSER = TRUE ;
      ROUNDDAY = FALSE ;
      i= (short) atoi( &tsttype[5] ) ;
      dt0 = 365. / i ;
      }

   /* loop over the time steps in the file                            */

   *nrtim = 0 ;
   itim = 0 ;

   for ( itim=1 ; itim <= ntim ; itim++ )
      {
      file_pos = (firstrecord + itim - 1) * recordlen ;
      if ( fseek ( iunit, file_pos, SEEK_SET)) goto HandleFileErr ;
      items_read = fread ( &iftime, (size_t) 2, (size_t) 1, iunit) ;
      if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1) ) goto HandleFileErr ;

      /* set time step */
      if ( YEARSER )
         {
         lngyear = 1900 + iftime / 100 ;
         lng1 = 1 ;
         lng0 = 0 ;
         julian( &lngyear, &lng1, &lng1, &lng0, &lng0, &lng0, &ftime ) ;
         ftime = ftime + ( iftime - 100 * ( iftime / 100 ) ) * dt0 ;
         ftime = (TInt4) ftime ;
         }
      else ftime = mpxtim + iftime * dt0 ;

      if ( ROUNDDAY ) ftime = (TInt4) (ftime + 0.5);

      /* check if it is still possible that we find something */
      some_lower = FALSE ;
      for ( i = 0; i < maxiti; i++)
         {
         if ( ftime <= *(timin + 2*i + 1)) some_lower = TRUE ;
         }
      if ( ! some_lower) break ;

      for ( i = 0; i < maxiti; i++)
         {
         if (( ftime >= *(timin + 2*i)) &&
             ( ftime <= *(timin + 2*i + 1)))
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
               break ;
               }
            } /* timin1 < ftime < timin2  */
         }  /* for i */
      }

   fclose ( iunit) ;
   return ;

HandleFileErr:

   if ( feof ( iunit) )
      *ierror = IEUEOF ;
   else
      itrans ( ferror ( iunit), (TInt4 *) ierror) ;

   fclose ( iunit) ;
   return ;

   }

/*************************************************************************/
/*    SUBROUTINE Get Values from MAPPIX file                             */
/*************************************************************************/

void ODSGetValMPX ( char *fname,
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
   char   tsttype [9] ;
   char   parnam [41] ;
   char   pardim [41] ;
   char   locnam [PARLEN] ;
   short  npar, nloc, ntim, ipar, iloc, itim ;
   short  idef, iftime, i ;
   TInt4  lng1, lng0, lngyear ;
   TInt4  recordlen, firstrecord ;
   TInt4  file_pos ;
   double mpxtim, dt0, ftime ;
   int    some_lower ;
   int    ROUNDDAY, YEARSER ;
   size_t items_read ;

   short  *ixloc , *ixpar, *ixtim, *chktim;
   float  *databuf ;

   ixloc = NULL ;
   ixpar = NULL ;
   ixtim = NULL ;
   chktim = NULL ;
   databuf = NULL ;

   iunit = fopen ( fname, "rb\0") ;

   if ( iunit == NULL)
      {
      *ierror = IENOFI ;
      return ;
      }

   /* check if this really is a MAPPIX file                           */
   items_read = fread ( &parnam, (size_t) 4, (size_t) 1, iunit ) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   parnam[4]='\0' ;
   if ( strcmp (parnam,"REAL") != 0 )
     {
     *ierror = IETYPE ;
     goto HandleExit ;
     }

   /* get name of time scale */
   if ( fseek ( iunit, 16L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &tsttype, (size_t) 8, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   tsttype[8]='\0' ;
   for (i=0 ; i<4 ; i++) tsttype[i] = (char) toupper ( (int) tsttype[i] ) ;

   /* determine file dimensions                                          */
   if ( fseek ( iunit, 24L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( &nloc, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &ntim, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   items_read = fread ( &npar, (size_t) 2, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;

   /* allocate buffers: */
   ixloc = calloc ( nloc, 2 ) ;
   ixpar = calloc ( npar,2 ) ;
   ixtim = calloc ( ntim, 2 ) ;
   chktim = calloc ( ntim, 2 ) ;
   databuf = calloc ( nloc, 4 ) ;
   if ( databuf == NULL)
      {
      *ierror = IEBUFF ;
      goto HandleExit ;
      }

   /* get MAPPIX parameter name and dimension                                         */
   if ( fseek ( iunit, 40L, SEEK_SET) != 0) goto HandleFileErr ;
   items_read = fread ( parnam, (size_t) 40, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   parnam[39]='\0' ;

   items_read = fread ( pardim, (size_t) 40, (size_t) 1, iunit) ;
   if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1)) goto HandleFileErr ;
   pardim[39]='\0' ;

   /* loop over series in the file */
   *nrpar = 0 ;
   *ierror = IEOK ;
   if ( fseek ( iunit, 120L, SEEK_SET) != 0) goto HandleFileErr ;

   for ( ipar = 1; ipar <= npar; ipar++)
      {

      /* Read MAPPIX parameter from file */
      if ( npar > 1 )
         {
         items_read = fread ( parnam, (size_t) 1, (size_t) 40, iunit) ;
         if ( ferror ( iunit) || feof ( iunit) || ( items_read != 40)) goto HandleFileErr ;
         }
      parnam[PARLEN]='\0' ;

      /* try to match the parameter */
      idef = 0 ;
      while ( idef < maxipa )
         {
         if ( equal ( parin + ( idef * PARLEN), parnam ) )
            {
            if ( *nrpar < maxopa )
               {
               strncpy ( par + ( *nrpar * PARLEN ), parnam, (size_t) PARLEN) ;
               par [ ( *nrpar * PARLEN ) + PARLEN] = '\0' ;
               ixpar[*nrpar] = ipar ;
               (*nrpar)++ ;
               }
            else
               {
               *ierror = IEPMNY ;
               goto HandleExit ;
               }
            break ;
            }
         else
            idef++ ;
         }
      }

   /* calculate record length and position                            */
   recordlen = 2 + 4 * nloc ;
   firstrecord = 2 + (short) (40.0 * (7 + npar) / recordlen) ;

   /* loop over all locations in the file                             */
   *nrloc = 0 ;
   *ierror = IEOK ;
   if ( fseek ( iunit, (firstrecord - 1) * recordlen, SEEK_SET)) goto HandleFileErr ;

   for ( iloc = 1; iloc <= nloc; iloc++)
      {

      /* Read MAPPIX segment number from file */
      items_read = fread ( &idef, (size_t) 2, (size_t) 1, iunit) ;
      if ( ferror ( iunit) | feof ( iunit) | ( items_read != 1)) goto HandleFileErr ;

      sprintf ( locnam, "%i", idef) ;

      /* Try to match the location */
      idef = 0 ;
      while ( idef < maxilo)
         {
         if ( equal ( locin + ( idef * PARLEN), locnam))
            {
            if ( *nrloc < maxolo)
               {
               strncpy ( loc + ( *nrloc * PARLEN), locnam, (size_t) PARLEN) ;
               loc [ ( *nrloc * PARLEN) + PARLEN] = '\0' ;
               ixloc[*nrloc] = iloc ;
               (*nrloc)++ ;
               }
            else
               {
               *ierror = IELMNY ;
               goto HandleExit ;
               }
            break ;
            }
         else
            idef++ ;
         }
      }

   /* attemp to set time scale                                        */
   mpxtim = 0.0 ;
   dt0=1 ;
   ROUNDDAY=TRUE ;
   YEARSER=FALSE ;
   if ( strcmp(tsttype,"DECADE  ") == 0 )
      dt0 = 365. / 36 ;
   else if ( strcmp(tsttype,"WEEKS   ") == 0 )
      dt0 = 365. / 52 ;
   else if ( strcmp(tsttype,"TWOWEEKS") ==0 )
      dt0 = 365. / 26 ;
   else if ( strstr(tsttype,"YEAR*") != NULL )
      {
      YEARSER = TRUE ;
      ROUNDDAY = FALSE ;
      i= (short) atoi( &tsttype[5] ) ;
      dt0 = 365. / i ;
      }

   /* loop over the time steps in the file */
   *nrtim = 0 ;
   itim = 0 ;

   for ( itim=1 ; itim <= ntim ; itim++ )
      {
      file_pos = (firstrecord + itim - 1) * recordlen ;
      if ( fseek ( iunit, file_pos, SEEK_SET) != 0 ) goto HandleFileErr ;

      items_read = fread ( &iftime, (size_t) 2, (size_t) 1, iunit) ;
      if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1) ) goto HandleFileErr ;

      /* set time step */
      if ( YEARSER )
         {
         lngyear = 1900 + iftime / 100 ;
         lng1 = 1 ;
         lng0 = 0 ;
         julian( &lngyear, &lng1, &lng1, &lng0, &lng0, &lng0, &ftime ) ;
         ftime = ftime + ( iftime - 100 * ( iftime / 100 ) ) * dt0 ;
         ftime = (TInt4) ftime ;
         }
      else ftime = mpxtim + iftime * dt0 ;

      if ( ROUNDDAY ) ftime = (TInt4) (ftime + 0.5);

      /* check if it is still possible that we find something */
      some_lower = FALSE ;
      for ( i = 0; i < maxiti; i++)
         {
         if ( ftime <= *(timin + 2*i + 1)) some_lower = TRUE ;
         }
      if ( ! some_lower) break ;

      for ( i = 0; i < maxiti; i++)
         {
         if (( ftime >= *(timin + 2*i)) &&
             ( ftime <= *(timin + 2*i + 1)))
            {
            if ( *nrtim < maxoti)
               {
               *(tim + *nrtim) = ftime ;
               ixtim[*nrtim] = itim ;
               chktim[*nrtim] = iftime ;
               (*nrtim)++ ;
               break ;
               }
            else
               {
               *nrtim  = maxoti ;
               *ierror = IETMNY ;
               break ;
               }
            } /* timin1 < ftime < timin2  */
         }  /* for i */
      }

   /* Be pessemistic, assume we will find nothing ...                 */

   for ( itim = 0; itim < maxoti; itim++)
      for ( ipar = 0; ipar < maxopa; ipar++)
         for ( iloc = 0; iloc < maxolo; iloc++)
            *(values + (itim * maxolo * maxopa +
                       ipar * maxolo +
                       iloc)) = misval ;
            /* fortran: values ( maxolo, maxopa, maxoti) */


   /* Now get the actual data !                                        */

   for ( ipar = 0; ipar < *nrpar; ipar++)
     for ( itim = 0; itim < *nrtim ; itim++)

       {
       file_pos=(firstrecord + (ixpar[ipar] - 1) * ntim + ixtim[itim] - 1) * recordlen ;
       if ( fseek ( iunit, file_pos, SEEK_SET) != 0 ) goto HandleFileErr ;

       /* just to make sure, check the time step */
       items_read = fread ( &iftime, (size_t) 2, (size_t) 1, iunit) ;
       if ( ferror ( iunit) || feof ( iunit) || ( items_read != 1) ) goto HandleFileErr ;
       if ( iftime != chktim[itim] )
         {
         *ierror = IETIME ;
         goto HandleExit ;
         }

       /* read the whole record and copy selected locations */
       items_read = fread ( databuf, (size_t) 4, (size_t) nloc, iunit) ;
       if ( ferror ( iunit) || feof ( iunit) || ( items_read != (size_t) nloc) ) goto HandleFileErr ;
       for ( iloc = 0; iloc < *nrloc ; iloc++ )
          values[itim * maxolo * maxopa +
                 ipar * maxolo +
                 iloc] = databuf[ixloc[iloc]-1] ;
       }

   /* we're done, bye ! */

HandleExit:

   fclose ( iunit) ;
   free ( ixloc ) ;
   free ( ixpar ) ;
   free ( ixtim ) ;
   free ( chktim ) ;
   free ( databuf ) ;
   return ;

/* is there a cleaner way then goto's for cleaning up on error ? */

HandleFileErr:

   if ( feof ( iunit) )
      *ierror = IEUEOF ;
   else
      itrans ( ferror ( iunit), (TInt4 *) ierror) ;

   goto HandleExit ;

   }

