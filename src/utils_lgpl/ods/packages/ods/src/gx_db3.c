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
/*   Date:       6 Dec 1993                                           */
/*   Time:       13:00                                                */
/*   Program:    GX_DB3.C                                             */
/*   Version:    1.02                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.01 -- 7 Jul 1993 -- 13:52 -- Operating System: DOS             */
/*   1.00 -- 7 Jul 1993 -- 13:48 -- Operating System: DOS             */
/*   0.00 -- 1 Jul 1993 -- 14:57 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GX_DB3                                               */
/*   Function:                                                        */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#if defined(MSWINDOWS) || defined(MSDOS)

#include <string.h>

#ifdef MSWINDOWS
#   include "d4all_dl.h"
#   undef PRINTIT
#else
#   include "d4all.h"
#endif

#define FALSE 0
#define TRUE  1

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#ifndef NOPROT

/* function prototypes */

int equal ( char *mask, char *name) ;

#endif

/* function for dBase 3 */

#ifdef SUN
#   define GX_DB3 gx_db3_
#else
#   define GX_DB3 gx_db3
#endif

#ifndef NOPROT
#  ifndef HPOLD
#     ifdef PC
      void GX_DB3 ( char *fname ,
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
                   TInt4 *nrlst ,
                   TInt4 *ierror)
#     else
#        error function gx_db3 not supported (yet) on UNIX platform
#     endif
#  else
#     error function gx_db3 not supported (yet) on UNIX platform
#  endif
#else
#  ifndef HPOLD
#     ifdef PC
      void GX_DB3 ( fname ,
                    lenfna,
                    fieldname,
                    unitfield,
                    pardef,
                    lendef,
                    maxdef,
                    parlst,
                    lenlst,
                    paruni,
                    lenuni,
                    maxlst,
                    nrlst ,
                    ierror)
#     else
#        error function gx_db3 not supported (yet) on UNIX platform
#     endif
#  else
#     error function gx_db3 not supported (yet) on UNIX platform
#  endif
      char *fname, *fieldname, *unitfield, *pardef, *parlst, *paruni ;
     TInt4 lenfna, lendef, maxdef, lenlst, lenuni, maxlst, *nrlst , *ierror ;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   -          I   Full filename, including extension      */
/*        .....   -          .   ...                                     */
/*                                                                       */
/*************************************************************************/

   {
   CODE4       code_base;
   DATA4       *data_file  = NULL;
   FIELD4      *field_ref  = NULL,
               *unit_ref   = NULL;
   FIELD4INFO  *field_info = NULL;
   TAG4INFO    *tag_info   = NULL;
   INDEX4      *index      = NULL;

   int  rc, lFound, hasunit ;
  TInt4 idef, ilst ;
   char *field_contents ;
   char *unit_contents ;
   int  record_nr ;

#  ifdef PRINTIT
   printf ( "\n\n*** ARGUMENTS gx_dbf3:\n");
   printf ( "filename       : %s\n", fname);
   printf ( "asked field    : %s\n", fieldname);
   printf ( "unit field     : %s\n", unitfield);
   printf ( "len filename   : %d\n", lenfna);
   printf ( "len pardef     : %d\n", lendef);
   printf ( "len parlst     : %d\n", lenlst);
   printf ( "maxdef         : %d\n", maxdef);
   printf ( "maxlst         : %d\n", maxlst);
#  endif

   *nrlst = 0 ;
   *ierror = IEOK ;
   d4init ( &code_base);
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
      printf ( "\n*** ERROR in init: %d ***\n", e4code ( &code_base));
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
      printf ( "\n*** ERROR in open: %d ***\n", e4code ( &code_base));
#     endif
      switch ( e4code ( &code_base))
         {
         case  -60 : *ierror = IENOFI;      /* open error              */
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

   field_ref = d4field ( data_file, fieldname);
   if ( e4code ( &code_base))
      {
#     ifdef PRINTIT
      printf ( "\n*** ERROR in field access: %d ***\n", e4code ( &code_base));
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
   if ( strncmp ( unitfield, "     ", 5) <= 0)
      {
      hasunit = FALSE;
#     ifdef PRINTIT
      printf ( "has NO unit\n");
#     endif
      }
   else
      {
#     ifdef PRINTIT
      printf ( "has unit\n");
#     endif
      hasunit = TRUE;
      unit_ref  = d4field ( data_file, unitfield);
      if ( e4code ( &code_base))
         {
#        ifdef PRINTIT
         printf ( "\n*** ERROR in unit access: %d ***\n", e4code (&code_base));
#        endif
         switch ( e4code ( &code_base))
            {
            case -200 : *ierror = IETYPE;      /* not data file  */
                        return;
            case -210 : hasunit = FALSE;       /* unrecognized field name */
                        code_base.error_code = 0;
#           ifdef PRINTIT
            printf ( "UNIT field not found\n");
#           endif
                        break;
            case -310 : *ierror = IETYPE;      /* not index file */
                        return;
            case -920 : *ierror = IEBUFF;      /* out of memory  */
                        return;
            default   : *ierror = IEOTHR;
                        return;
            }
         }
      }

   /* Now the loop over the records on file, for each record .. */

   /* tzt hier zetten juiste index expressie ?? */

#  ifdef PRINTIT
   printf ( "start loop over records\n");
#  endif
   record_nr = 0 ;
   for ( rc = d4top ( data_file); rc == r4success
                      ; rc = d4skip ( data_file, 1L))
      {
#     ifdef PRINTIT
      printf ( "   rc %d\n", rc) ;
#     endif
      record_nr++ ;
#     ifdef PRINTIT
      printf ( "   rec no %d\n", record_nr) ;
#     endif
      if ( ! d4deleted ( data_file))
         {
         field_contents = f4memo_str ( field_ref);

#        ifdef PRINTIT
         printf ( " Field content = >%s<\n", field_contents);
#        endif

         for ( idef = 0; idef < maxdef; idef++)
            {
            if ( equal ( pardef + ( idef * lendef), field_contents))
               {
               lFound = FALSE;
               for ( ilst = 0; ilst < *nrlst; ilst++)
                  {
                  if ( strcmp ( parlst + ilst * lenlst, field_contents) == 0)
                     {
                     lFound = TRUE;
                     break;
                     }
                  }
               if ( ! lFound)
                  {
                  (*nrlst)++;
                  if ( *nrlst > maxlst)
                     {
                     d4close ( data_file);
                     *ierror = IEPMNY;
                     (*nrlst)--;
                     return;
                     }
                  else
                     {
                     strcpy ( parlst + ( *nrlst - 1) * lenlst, field_contents);
                     if (( hasunit) && ( lenuni > 0))
                        {
                        unit_contents  = f4memo_str ( unit_ref);
#                       ifdef PRINTIT
                        printf ( " Unit  content = >%s<\n", unit_contents);
#                       endif
                        strcpy ( paruni + ( *nrlst - 1) * lenuni, unit_contents);
                        }
                     else
                        {
                        if ( lenuni > 0)
                           strcpy ( paruni + ( *nrlst - 1) * lenuni, "  ");
                        }
                     }
                  }
               /* leave the loop, if the field_contents fits one pardef, we
                  don't have to look further, we don't want multiple
                  occurences */
               break;
               }
            }
         }
      }

   d4close ( data_file);
#  ifdef PRINTIT
   printf ( "\ngx_db3, end, nr = %d\n", *nrlst);
#  endif
   return ;
   }

#else

void gx_db3 ( char *fname ,
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
	   TInt4 *nrlst ,
	   TInt4 *ierror)
{
    *ierror = IEUNKN;
}

#endif  /* #if defined(MSWINDOWS) || defined(MSDOS) */

