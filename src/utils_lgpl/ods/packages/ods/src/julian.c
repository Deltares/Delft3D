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
 *  julian.c  -  ODS convert Gregorian to Julian date
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 12/27/00 2:53p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/julian.c,v $
 *
 */


/*   Date:       12 Apr 1994                                          */
/*   Time:       15:55                                                */
/*   Program:    JULIAN.C                                             */
/*   Version:    1.02                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.01 -- 9 Dec 1993 -- 13:00 -- Operating System: DOS            */
/*   1.00 -- 27 Aug 1993 -- 19:23 -- Operating System: DOS            */
/*   0.00 -- 4 Aug 1993 -- 08:21 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     Julian                                               */
/*   Function:   This functions returns the so called Julian day of   */
/*               a date, or the value -1.0 if an error occurred.      */
/*               The Julian day of a date is the number of days that  */
/*               has passed since January 1, 4712 BC at 12h00         */
/*               ( Gregorian). It is usefull to compute differces     */
/*               between dates. ( See function gregot for the         */
/*               reverse proces ).                                    */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <math.h>

#undef SUN
#ifdef SUN
#   define JULIAN julian_
#else
#   define JULIAN julian
#endif

#ifndef NOPROT
   void FUNTYPE JULIAN ( TInt4 *iyear ,
                                   TInt4 *imonth,
                                   TInt4 *iday  ,
                                   TInt4 *ihour ,
                                   TInt4 *imin  ,
                                   TInt4 *isec  ,
                                   TReal8 *jdate)
#else
   void FUNTYPE JULIAN ( iyear ,
                                   imonth,
                                   iday  ,
                                   ihour ,
                                   imin  ,
                                   isec  ,
                                   jdate)
   TReal8 *jdate ;
   TInt4 *iyear, *imonth, *iday, *ihour, *imin, *isec ;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name   In/Out Size            Description                      */
/*        ------ ------ -------         ---------------------------      */
/*        jdate  out    -               Julian day                       */
/*        iyear  in     -               Year   ( -4713-.. )              */
/*        imonth in     -               Month  ( 1-12 )                  */
/*        iday   in     -               Day    ( 1-28,29,30 or 31 )      */
/*        ihour  in     -               Hour   ( 0-23 )                  */
/*        imin   in     -               Minute ( 0-59 )                  */
/*        isec   in     -               Second ( 0-59 )                  */
/*                                                                       */
/*************************************************************************/

   {
   TReal8 temp1, temp2 ;
   TInt4    monlen [] = { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} ;

   if (( *iyear  < -4713 ) || ( *imonth <  1 ) ||
       ( *imonth >    12 ) || ( *iday   <  1 ) ||
       ( *iday   > monlen[ *imonth -1 ]) ||
       ( *ihour  <     0 ) || ( *ihour  > 24 ) ||
       ( *imin   <     0 ) || ( *imin   > 60 ) ||
       ( *isec   <     0 ) || ( *isec   > 60 ))
      *jdate  = -1.0 ;
   else
      {
      if (( *imonth == 1) || ( *imonth == 2))
         temp1 = -1.0 ;
      else
         temp1 =  0.0 ;
      temp2  = *iday - 32075.0 +
               (TInt4) ( 1461.0 * ( *iyear + 4800.0 + temp1) / 4.0) +
               (TInt4) ( 367 * ( *imonth - 2 - (TInt4) temp1 * 12) / 12) -
               (TInt4) ( 3.0 * (TInt4) (( *iyear + 4900.0 + temp1) / 100.0) / 4.0) ;
      temp1  = *ihour * 3600.0 + *imin * 60.0 + *isec - 43200.0 ;
      *jdate = temp2 + ( temp1 / 86400.0 ) ;
      }
   return ;
   }
