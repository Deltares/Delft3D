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
 *  iftype.c  -  ODS determine file type from extension
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/iftype.c,v $
*/
/*
 *
 */


/*   Date:       28 Feb 1994                                           */
/*   Time:       11:50                                                */
/*   Program:    IFTYPE.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 08:34 -- Operating System: DOS             */
/*   1.00 -- 4 Aug 1993 -- 08:49 -- Operating System: DOS             */
/*   0.00 -- 4 Aug 1993 -- 08:21 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     iftype                                               */
/*   Function:   get file type by examining extension                 */
/*   Comment:  1.03: support for WASPRO files (*.WS1 and *.WAS)       */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#ifndef NOPROT
   TInt4 iftype ( char *fname)
#else
   TInt4 iftype ( fname)
   char *fname ;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    I/O  Description                                       */
/*        ------  ---  -----------------------------                     */
/*        fname    I   Full filename, including extension                */
/*                                                                       */
/*     Returns:                                                          */
/*        itype    File type. See odstypes.h for definition              */
/*                                                                       */
/*************************************************************************/

   {
   char ext [4] ;
   char *ipos ;
   TInt4 itype ;

   ipos = strrchr ( fname, '.') ;
   if ( ipos == NULL)
      {

      /* Unable to get extension, type undefined */

      itype = ITUNDE ;
      }
   else
      {
      strncpy ( ext, ipos+1, 3) ;
      ext [3] = '\0' ;
      if ((( ext[0] == 'd') || ( ext[0] == 'D')) &&
          (( ext[1] == 'b') || ( ext[1] == 'B')) &&
          (( ext[2] == 'f') || ( ext[2] == 'F')))
         itype = ITDBF3 ;
      else if ((( ext[0] == 'm') || ( ext[0] == 'M')) &&
               (( ext[1] == 'a') || ( ext[1] == 'A')) &&
               (( ext[2] == 'p') || ( ext[2] == 'P')))
         itype = ITDLWM ;
      else if ((( ext[0] == 'h') || ( ext[0] == 'H')) &&
               (( ext[1] == 'i') || ( ext[1] == 'I')) &&
               (( ext[2] == 's') || ( ext[2] == 'S')))
         itype = ITDLWH ;
      else if ((( ext[0] == 'm') || ( ext[0] == 'M')) &&
               (( ext[1] == 'p') || ( ext[1] == 'P')) &&
               (( ext[2] == 'x') || ( ext[2] == 'X')))
         itype = ITMPX  ;
      else if ((( ext[0] == 'p') || ( ext[0] == 'P')) &&
               (( ext[1] == 's') || ( ext[1] == 'S')) &&
               (( ext[2] == 't') || ( ext[2] == 'T')))
         itype = ITJSP  ;
      else if ((( ext[0] == 'w') || ( ext[0] == 'W')) &&
               (( ext[1] == 's') || ( ext[1] == 'S')) &&
               (( ext[2] == '1') || ( ext[2] == '1')))
         itype = ITWS1  ;
      else if ((( ext[0] == 'w') || ( ext[0] == 'W')) &&
               (( ext[1] == 'a') || ( ext[1] == 'A')) &&
               (( ext[2] == 's') || ( ext[2] == 'S')))
         itype = ITWAS  ;
      else
         itype = ITUNDE ;
      }
   return itype ;
   }
