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
 *  dbasewse.c  -  ODS functions for Dbase WISE format files
 *
 *   Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dbasewse.c,v $
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
/*   Subroutines: gkdbf3 Get key values from dbf3 file                */
/*                gldbf3 Get locations from dbf3 file                 */
/*                gldbf4 Get locations from dbf4 file                 */
/*                gpdbf3 Get parameters from dbf3 file                */
/*                gpdbf4 Get parameters from dbf4 file                */
/*                gtdbf3 Get times from dbf3 file                     */
/*                gvdbf3 Get values from dbf3 file                    */
/*   Function:    "Loket" functions per filetype                      */
/*   Comment:     General version                                     */
/*   Reference:   opendata.doc                                        */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/

#include "portable.h"
#include "ods.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "equal.h"

#ifdef MSWINDOWS
#  include "d4all_dl.h"
#else
#  ifdef PC
#     include "d4all.h"
#  endif
#endif

#define FALSE   0
#define TRUE    1

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#define PARDES  40
#define LENFNA 128
#define MAXIBU 200

/* defines for external functions and functions that can be called from
   external fortran programs: */

#ifdef SUN
#   define GKDBF3 gkdbf3_
#   define GX_DB3 gx_db3_
#else
#   define GKDBF3 gkdbf3
#   define GX_DB3 gx_db3
#endif

#ifndef NOPROT

/* function prototypes                                                   */

void GX_DB3 ( char *fname,
             TInt4 lenfna,
              char *fieldname,
              char *unitfield,
              char *pardef,
             TInt4 lendef,
             TInt4 maxdef,
              char *parlst,
             TInt4 lenlst,
              char *paruni,
             TInt4 lenuni,
             TInt4 maxlst,
             TInt4 *nrlst,
             TInt4 *ierror) ;

#endif

#ifdef PC           /* the following routines are for DBF support and
                       need the CodeBase library. At the moment only
                       available on PC                                   */

/*************************************************************************/
/*    SUBROUTINE Get Parameter from DBase 3 File                         */
/*************************************************************************/

void ODSGetParDbf3 ( char *fname,
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
  TInt4 lenfna, lendef, lenlst, lenuni ;
   char fieldn [] = "SUBST     " ;
   char unitn []  = "PREFUNIT  " ;

   lenfna = LENFNA ;
   lendef = PARLEN ;
   lenlst = PARLEN ;
   lenuni = PARLEN ;
   GX_DB3 ( fname,  lenfna, fieldn, unitn,  pardef,
            lendef, maxdef, parlst, lenlst, paruni,
            lenuni, maxlst, nrlst,  ierror ) ;
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Parameter from DBase 4 File (WITHOUT INDEX THIS IS  */
/*    THE SAME AS FOR DBASE III !!)                                      */
/*************************************************************************/

void ODSGetParDbf4 ( char *fname,
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
  TInt4 lenfna, lendef, lenlst, lenuni ;
   char fieldn [] = "SUBST     " ;
   char unitn []  = "PREFUNIT  " ;

   lenfna = LENFNA ;
   lendef = PARLEN ;
   lenlst = PARLEN ;
   lenuni = PARLEN ;
   GX_DB3 ( fname,  lenfna, fieldn, unitn,  pardef,
            lendef, maxdef, parlst, lenlst, paruni,
            lenuni, maxlst, nrlst,  ierror ) ;
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Location from DBase 3 File                          */
/*************************************************************************/

void ODSGetLocDbf3 ( char *fname,
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
  TInt4 lenfna, lendef, lenlst, lenuni ;
   char paruni [] = "" ;
   char fieldn [] = "STAT      " ;
   char unitn []  = "          " ;

   lenfna = LENFNA ;
   lendef = PARLEN ;
   lenlst = PARLEN ;
   lenuni = 0 ;
   GX_DB3 ( fname,  lenfna, fieldn, unitn,  locdef,
            lendef, maxdef, loclst, lenlst, paruni,
            lenuni, maxlst, nrlst,  ierror ) ;
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Location from DBase 4 File (is same as DBF3 as      */
/*    long as no strchr used)                                             */
/*************************************************************************/

void ODSGetLocDbf4 ( char *fname,
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
  TInt4 lenfna, lendef, lenlst, lenuni ;
   char paruni [] = "" ;
   char fieldn [] = "STAT      " ;
   char unitn []  = "          " ;

   lenfna = LENFNA ;
   lendef = PARLEN ;
   lenlst = PARLEN ;
   lenuni = 0 ;
   GX_DB3 ( fname,  lenfna, fieldn, unitn,  locdef,
            lendef, maxdef, loclst, lenlst, paruni,
            lenuni, maxlst, nrlst,  ierror ) ;
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get description of key from DBase 3 File                */
/*************************************************************************/

void FUNTYPE GKDBF3 ( char *fname,
                      char *keydef,
                     TInt4 *maxdef,
                      char *keylst,
                      char *keydes,
                     TInt4 *maxlst,
                      char *keyfld,
                      char *desfld,
                     TInt4 *nrlst,
                     TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       desfld  -          I   Name of field containing description.    */
/*       fname   -          I   Full filename of dBase file              */
/*       ierror  -          O   Errorcode. See errors.inc for definitio  */
/*       keydef  maxdef     I   List of keys wanted (may contain *).     */
/*       keydes  maxlst     O   List of keys-descriptions found.         */
/*       keyfld  -          I   Name of field containing key.            */
/*       keylst  maxlst     O   List of keys found.                      */
/*       maxdef  -          I   (Max.) Nr of wanted keys.                */
/*       maxlst  -          I   Max. nr of keys to return.               */
/*       nrlst   -          O   Nr of parameters returned.               */
/*                                                                       */
/*************************************************************************/
   {
  TInt4 lenfna, lendef, lenlst, lendes ;

   lenfna = LENFNA ;
   lendef = PARLEN ;
   lenlst = PARLEN ;
   lendes = PARDES ;
   GX_DB3 ( fname,  lenfna,  keyfld, desfld, keydef,
            lendef, *maxdef, keylst, lenlst, keydes,
            lendes, *maxlst, nrlst,  ierror ) ;
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Values from DBF 3 file                              */
/*************************************************************************/

void ODSGetValDbf3 ( char *fname,
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
   CODE4       code_base;
   DATA4       *data_file  = NULL ;
   FIELD4      *field_dat  = NULL,
               *field_tim  = NULL,
               *field_loc  = NULL,
               *field_par  = NULL,
               *field_val  = NULL ;

   int    rc, lFound ;
   TInt4  ipar, itim, iloc, il, ip, it ;
   char   *cont_fld ;
   char   str_par [ 10], str_loc [ 10], str_dat [ 10], str_tim [ 10] ;
   TInt4  itime, ihour0, imin0 ;
   double ftime, val ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   WORD wType ;
   char tmpString [ 200] ;

   wType = MB_OK | MB_ICONEXCLAMATION ;
   wsprintf ( (LPSTR) tmpString, "GVDBF3 filenaam : %s", fname) ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif

   *ierror = IEOK ;
   *nrloc = 0 ;
   *nrpar = 0 ;
   *nrtim = 0 ;

   /* Be pessemistic, assume we will find nothing ...                 */

   for ( iloc = 0; iloc < maxolo; iloc++)
      for ( ipar = 0; ipar < maxopa; ipar++)
         for ( itim = 0; itim < maxoti; itim++)
            *(values + (itim * maxolo * maxopa +
                       ipar * maxolo +
                       iloc)) = misval ;
/*    fortran: values ( maxolo, maxopa, maxoti)                       */

   d4init ( &code_base);
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4init") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
   wsprintf ( (LPSTR) tmpString, "GVDBF3 code_base.error_code %d", code_base.error_code) ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
      rc = e4code ( &code_base) ;
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in init: %d ***\n", rc );
#     else
      wsprintf ( (LPSTR) tmpString, "GVDBF3 ERROR in init %d ", rc) ;
      MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#     endif
#     endif
      *ierror = IEOTHR;
      return;
      }
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after e4code") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif

   code_base.auto_open  = FALSE;
   code_base.read_only  = TRUE;

   data_file = d4open ( &code_base, fname);
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4open") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in open: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case  -60 : *ierror = IENOFI;      /* open error              */
                     break;
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case  -70 : *ierror = IETYPE;      /* not dbt file            */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }

   field_loc = d4field ( data_file, "STAT") ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4field STAT") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in STAT field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }
   field_par = d4field ( data_file, "SUBST") ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4field SUBST") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in SUBST field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }
   field_dat = d4field ( data_file, "DATE") ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4field DATE") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in DATE field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }
   field_tim = d4field ( data_file, "TIME") ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4field TIME") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in TIME field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }
   field_val = d4field ( data_file, "VALUE") ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4field VALUE") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in VALUE field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }

   /* Now the loop over the records on file, for each record .. */

   /* tzt hier zetten juiste index expressie ?? */

   for ( rc = d4top ( data_file); rc == r4success ; rc = d4skip ( data_file, 1L))
      {
#     ifdef MSWINDOWS
#     ifdef PRINTIT
      wsprintf ( (LPSTR) tmpString, "GVDBF3 rc = %d", rc) ;
      MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#     endif
#     endif
      if ( ! d4deleted ( data_file))
         {
         cont_fld = f4memo_str ( field_par) ;
         strncpy ( str_par, cont_fld, 10) ;

         cont_fld = f4memo_str ( field_loc) ;
         strncpy ( str_loc, cont_fld, 10) ;

         cont_fld = f4memo_str ( field_dat) ;
         ftime = (double) f4long ( field_dat) - 0.5 ;
         strncpy ( str_dat, cont_fld, 10) ;

         cont_fld = f4memo_str ( field_tim) ;
         itime = f4long ( field_tim) ;
         strncpy ( str_tim, cont_fld, 10) ;

         cont_fld = f4memo_str ( field_val) ;
         val = f4double ( field_val) ;

         ihour0  = itime / 100 ;
         imin0   = itime - (ihour0 * 100) ;
         ftime = ftime + ( ihour0 / 24.0) + ( imin0 / 1440.0) ;

#        ifdef PRINTIT
#        ifndef MSWINDOWS
         printf ( "\nRecord content = >%s<\n", str_par);
         printf ( "                 >%s<\n", str_loc);
         printf ( "                 >%s<\n", str_dat);
         printf ( "                 >%s<\n", str_tim);
         printf ( "          JULIAN: %lf\n", ftime) ;
#        endif
#        endif

         for ( ipar = 0; ipar < maxipa; ipar++)
            {
            for ( iloc = 0; iloc < maxilo; iloc++)
               {
               for ( itim = 0; itim < maxiti; itim++)
                  {
                  if (( equal ( parin + ( ipar * PARLEN), str_par)) &&
                      ( equal ( locin + ( iloc * PARLEN), str_loc)) &&
                      ( ftime >= *(timin + 2*itim)) &&
                      ( ftime <= *(timin + 2*itim + 1)))
                     {
#                    ifdef PRINTIT
#                    ifndef MSWINDOWS
                     printf ( " found\n") ;
#                    endif
#                    endif
                     /* found one, now determine if there already is an
                        entry for this location, parameter and time  */

                     ip = 0 ;
                     lFound = FALSE ;
                     while ( ip < *nrpar)
                        {
                        if ( strcmp ( str_par, par + ip * PARLEN) == 0)
                           {
                           lFound = TRUE ;
                           break ;
                           }
                        else
                           ip++ ;
                        }
                     if ( ! lFound)
                        {
                        if ( ip < maxopa)
                           {
                           strcpy ( par + *nrpar * PARLEN, str_par) ;
                           (*nrpar)++ ;
                           }
                        else
                           *ierror = IEPMNY ;
                        }

                     il = 0 ;
                     lFound = FALSE ;
                     while ( il < *nrloc)
                        {
                        if ( strcmp ( str_loc, loc + il * PARLEN) == 0)
                           {
                           lFound = TRUE ;
                           break ;
                           }
                        else
                           il++ ;
                        }
                     if ( ! lFound)
                        {
                        if ( il < maxolo)
                           {
                           strcpy ( loc + *nrloc * PARLEN, str_loc) ;
                           (*nrloc)++ ;
                           }
                        else
                           *ierror = IELMNY ;
                        }

                     it = 0 ;
                     lFound = FALSE ;
                     while ( it < *nrtim)
                        {
                        if ( fabs ( ftime - *(tim + it)) < .0003)
                           {
                           lFound = TRUE ;
                           break ;
                           }
                        else
                           it++ ;
                        }
                     if ( ! lFound)
                        {
                        if ( it < maxoti)
                           {
                           *(tim + *nrtim) = ftime ;
                           (*nrtim)++ ;
                           }
                        else
                           *ierror = IETMNY ;
                        }

                     if (( ip < maxopa) && ( il < maxolo) && ( it < maxoti))
                        *(values + (it * maxolo * maxopa + ip * maxolo + il)) = (float) val ;
                     }
                  }
               }
            }
         }
      }
   d4close ( data_file) ;
#  ifdef MSWINDOWS
#  ifdef PRINTIT
   wsprintf ( (LPSTR) tmpString, "GVDBF3 after d4close") ;
   MessageBox( 0, tmpString, "DEBUG MESSAGE", wType) ;
#  endif
#  endif
   d4close_all ( &code_base);
#  ifndef S4DLL
      mem4reset () ;
#  endif
   return ;
   }

/*************************************************************************/
/*    SUBROUTINE Get Times from DBase 3 (4) file                         */
/*************************************************************************/

void ODSGetTmeDbf3 ( char *fname,
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
   CODE4       code_base;
   DATA4       *data_file  = NULL ;
   FIELD4      *field_dat  = NULL,
               *field_tim  = NULL ;


   int    rc, lFound ;
   TInt4  itim, it ;
   char   *cont_fld ;
   char   str_dat [ 10], str_tim [ 10] ;
   TInt4  itime, ihour0, imin0 ;
   double ftime ;

   *ierror = IEOK ;
   *nrtim = 0 ;

   d4init ( &code_base);
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in init: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      *ierror = IEOTHR;
      return;
      }

   code_base.auto_open  = FALSE;
   code_base.read_only  = TRUE;

   data_file = d4open ( &code_base, fname);
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in open: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case  -60 : *ierror = IENOFI;      /* open error              */
                     break;
         case  -70 : *ierror = IETYPE;      /* not dbt file            */
                     break;
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }

   field_dat = d4field ( data_file, "DATE") ;
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in DATE field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }
   field_tim = d4field ( data_file, "TIME") ;
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
#     ifndef MSWINDOWS
      printf ( "\n*** ERROR in TIME field access: %d ***\n", e4code ( &code_base));
#     endif
#     endif
      switch ( e4code ( &code_base))
         {
         case -200 : *ierror = IETYPE;      /* not data file           */
                     break;
         case -210 : *ierror = IEINFO;      /* unrecognized field name */
                     break;
         case -310 : *ierror = IETYPE;      /* not index file          */
                     break;
         case -920 : *ierror = IEBUFF;      /* out of memory           */
                     break;
         default   : *ierror = IEOTHR;
         }
      return;
      }

   /* Now the loop over the records on file, for each record .. */

   /* tzt hier zetten juiste index expressie ?? */

   for ( rc = d4top ( data_file); rc == r4success ; rc = d4skip ( data_file, 1L))
      {
      if ( ! d4deleted ( data_file))
         {
         cont_fld = f4memo_str ( field_dat) ;
         ftime = (double) f4long ( field_dat) - 0.5 ;
         strncpy ( str_dat, cont_fld, 10) ;

         cont_fld = f4memo_str ( field_tim) ;
         itime = f4long ( field_tim) ;
         strncpy ( str_tim, cont_fld, 10) ;

         ihour0  = itime / 100 ;
         imin0   = itime - (ihour0 * 100) ;
         ftime = ftime + ( ihour0 / 24.0) + ( imin0 / 1440.0) ;

#        ifdef PRINTIT
#        ifndef MSWINDOWS
         printf ( "\nRecord content = >%s<\n", str_dat);
         printf ( "                 >%s<\n", str_tim);
         printf ( "          JULIAN: %lf\n", ftime) ;
#        endif
#        endif

         for ( itim = 0; itim < maxiti; itim++)
            {
            if (( ftime >= *(timin + 2*itim)) &&
                ( ftime <= *(timin + 2*itim + 1)))
               {
#              ifdef PRINTIT
#              ifndef MSWINDOWS
               printf ( " found\n") ;
#              endif
#              endif

               /* found one, now determine if there already is an
                  entry for this time  */

               it = 0 ;
               lFound = FALSE ;
               while ( it < *nrtim)
                  {
                  if ( fabs ( ftime - *(tim + it)) < .0003)
                     {
                     lFound = TRUE ;
                     break ;
                     }
                  else
                     it++ ;
                  }
               if ( ! lFound)
                  {
                  if ( it < maxoti)
                     {
                     *(tim + *nrtim) = ftime ;
                     (*nrtim)++ ;
                     }
                  else
                     *ierror = IETMNY ;
                  }
               }
            }
         }
      }

   d4close ( data_file);
   d4close_all ( &code_base);
   #ifndef S4DLL
      mem4reset() ;
   #endif
   return ;
   }

#endif         /* ifdef PC */





