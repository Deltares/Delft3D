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
 *  gregor.c  -  ODS convert Julian to Gregorian date
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11-02-05 10:04 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/gregor.c,v $
 *
 */


/*   Date:       6 Dec 1993                                           */
/*   Time:       13:00                                                */
/*   Program:    GREGOR.C                                             */
/*   Version:    1.01                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.00 -- 27 Aug 1993 -- 19:23 -- Operating System: DOS            */
/*   0.00 -- 4 Aug 1993 -- 08:21 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     Gregor                                               */
/*   Function:   This functions returns the Gregorian date and the    */
/*               time of a so called Julian day, or iyear -9999       */
/*               if an error occurred.                                */
/*               The Julian day of a date is the number of days that  */
/*               has passed since January 1, 4712 BC at 12h00         */
/*               ( Gregorian). It is usefull to compute differces     */
/*               between dates. ( See function julian for the         */
/*               reverse proces ).                                    */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <math.h>

#undef SUN
#ifdef SUN
#   define GREGOR gregor_
#else
#   define GREGOR gregor
#endif

#ifndef NOPROT
   void FUNTYPE GREGOR ( double *julian,
                                   TInt4 *iyear  ,
                                   TInt4 *imonth ,
                                   TInt4 *iday   ,
                                   TInt4 *ihour  ,
                                   TInt4 *imin   ,
                                   TInt4 *isec   )
#else
   void FUNTYPE GREGOR ( julian,
                                   iyear  ,
                                   imonth ,
                                   iday   ,
                                   ihour  ,
                                   imin   ,
                                   isec   )
   double *julian ;
   TInt4 *iyear, *imonth, *iday, *ihour, *imin, *isec ;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name   In/Out Size            Description                      */
/*        ------ ------ -------         ---------------------------      */
/*        iyear  out    -               Year   ( -4713-.. )              */
/*        imonth out    -               Month  ( 1-12 )                  */
/*        iday   out    -               Day    ( 1-28,29,30 or 31 )      */
/*        ihour  out    -               Hour   ( 0-23 )                  */
/*        imin   out    -               Minute ( 0-59 )                  */
/*        isec   out    -               Second ( 0-59 )                  */
/*        julian in     -               Julian day                       */
/*                                                                       */
/*************************************************************************/

   {
   double temp1, temp2, temp3, temp4, temp5 ;

   if ( *julian < (double) 0.0 )
   {
      *iyear  = -9999 ;
      *imonth =     1 ;
      *iday   =     1 ;
      *ihour  =     0 ;
      *imin   =     0 ;
      *isec   =     0 ;
   }
   else
      {
      temp4 = *julian ;
      temp5 = *julian - (TInt4) *julian + 0.000005787 ;
      if ( temp5 < 0.5 )
         {
         temp3  = 0.5 + temp5 ;
         modf ( temp4, &temp4 ) ;
         }
      else
         {
         temp3  = temp5 - 0.5 ;
         modf ( temp4, &temp4 ) ;
         temp4  = temp4 + 1.0 ;
         }
      /* to fix rounding errors add 0.5 seconds: */
/*    temp3   = temp3 + .000005787 ; */
      temp1   = temp4 + 68569.0 ;
      temp2   = (double) (TInt4)  ( 4.0 * temp1 / 146097.0) ;
      temp1   = temp1 - (double) (TInt4) (( 146097.0 * temp2 + 3.0 ) / 4.0) ;
      *iyear  = (TInt4)   ( 4000.0 * ( temp1 + 1.0 ) / 1461001.0) ;
      temp1   = temp1 - (double) (TInt4) ( 1461.0 * *iyear / 4.0) + 31.0 ;
      *imonth = (TInt4)   ( 80.0 * temp1 / 2447.0) ;
      *iday   = (TInt4)   ( temp1 - (TInt4) ( 2447.0 * *imonth / 80.0)) ;
      /*
      printf ( " temp1 : %f\n", temp1) ;
      printf ( " temp2 : %f\n", temp2) ;
      printf ( " temp3 : %f\n", temp3) ;
      printf ( " temp4 : %f\n", temp4) ;
      printf ( " temp5 : %f\n", temp5) ;
      */
      temp1   = (double) (TInt4)  ( *imonth / 11.0) ;
      *imonth = (TInt4)   ( *imonth + 2.0 - 12.0 * temp1) ;
      *iyear  = (TInt4)   ( 100.0 * ( temp2 - 49.0 ) + *iyear + temp1) ;
      *ihour  = (TInt4)   ( temp3 * 24.0 ) ;
      *imin   = (TInt4)   ( temp3 * 1440.0 - 60.0 * *ihour ) ;
      *isec   = (TInt4)   ( temp3 * 86400.0 - 3600.0 * *ihour - 60.0 * *imin) ;
      }
   return ;
   }
