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
 *  utils.c   -  ODS utility routines
 *
 *  Peter van den Bosch
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:52p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/utils.c,v $
*/
/*
 *
 *
 */


/*   Date:       24 april 1995                                        */
/*   Time:       11:25                                                */
/*   Program:    utils.c                                              */
/*   Version:    1.0                                                  */
/*   Programmer: Peter van den Bosch                                  */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   Project:    Open Data Structuur                                  */
/*   Module:                                                          */
/*   Function:                                                        */
/*   Comment:    MICROSOFT C 6.00 VERSION                             */
/*   Reference:                                                       */
/*   Review:                                                          */


#include <stdio.h>
#include <stdlib.h>

#include "portable.h"
#include "ods.h"
#include "julian.h"
#include "gregor.h"

#define FALSE 0
#define TRUE  1

/*************************************************************************/
/*    SUBROUTINE Add a number of seconds to a Julian date                */
/*************************************************************************/
double AddToJulian(
    double juldate,
    TInt4 addyear,
    TInt4 addmonth,
    TInt4 addday,
    TInt4 addsec )
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       juldate -          I   Julian date number                       */
/*       addyear -          I   Number of years to be added              */
/*       addmonth-          I   Number of months to be added             */
/*       addday  -          I   Number of days to be added               */
/*       addsec  -          I   Number of seconds to be added to juldate */
/*                                                                       */
/*       Return: new julian date number of ( juldate + addsec )          */
/*                                                                       */
/*************************************************************************/
{
    TInt4  icurtm, l , n  , idp , itp , ihou, imin,
           isec  , iy, imo, iday, imo1;
    TInt4  year, month, day, hour, min, sec, zero;
    double julday, ftime;


    /* Convert julian start time to full days */
    zero = 0;
    gregor( &juldate, &year, &month, &day, &hour, &min, &sec );
    julian ( (TInt4 *)&year, (TInt4 *)&month, (TInt4 *)&day, (TInt4 *)&zero,
	     (TInt4 *)&zero, (TInt4 *)&zero,  (TReal8 *)&julday );
    julday = julday + (double) 0.5 ;

    /* Add hours minutes and seconds */
    icurtm = hour*3600 + min*60 + sec + addsec ;
    iday   = icurtm / 86400 ;
    icurtm = icurtm - iday*86400 ;
    ihou   = icurtm / 3600 ;
    icurtm = icurtm - ihou*3600 ;
    imin   = icurtm / 60 ;
    isec   = icurtm - imin*60 ;

    /* add days */
    julday = julday + addday ;

    /* Convert true time from julian day-number */

    l      = (TInt4) julday + iday   +  68569 ;
    n      = 4      * l      / 146097 ;
    l      = l - ( 146097 * n + 3 ) / 4 ;
    iy     = 4000 * ( l + 1 ) / 1461001 ;
    l      = l - 1461 * iy / 4 + 31 ;
    imo    = 80 * l / 2447 ;
    iday   = l - 2447 * imo / 80 ;
    l      = imo / 11 ;
    imo    = imo + 2 - 12 * l ;
    iy     = 100 * ( n - 49 ) + iy + l ;

    /* add year and month */
    imo    = imo + addmonth;
    iy     = iy  + addyear;

    /* Convert to julian notation and store */

    imo1      = (imo -14)/12;
    idp       = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4
                + 367  * (imo  - 2    - imo1  *  12    )/12
                - 3    * ((iy  + 4900 + imo1  )/100    )/ 4;
    itp       = ihou * 3600 + imin * 60 + isec - 43200;

    ftime     = (double) idp + (double) itp / (double) 86400.0;

    return ftime ;
}

/*************************************************************************/
/*    SUBROUTINE Initializes a string with spaces                        */
/*************************************************************************/
void Blanks (
    char * string,
    TInt4 len )
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       string  -         I/O  String to be blanked                     */
/*       len     -          I   Lenght of string                         */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
{
    TInt4 i;

    for ( i = 0 ; i < len-1 ; i++ )
        {
        string[i] = ' ' ;
        }
    string[ len-1 ] = '\0' ;
}

/*************************************************************************/
/*    SUBROUTINE Close files and frees memory                            */
/*************************************************************************/
void Errfin (
    FILE * unit1,
    FILE * unit2,
    void * mem1,
    void * mem2,
    void * mem3 )

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       unit1   -          I   File pointer to be closed                */
/*       unit2   -          I   File pointer to be closed                */
/*       mem1    -          I   Memory pointer to be freed               */
/*       mem2    -          I   Memory pointer to be freed               */
/*       mem3    -          I   Memory pointer to be freed               */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
{
    if ( unit1 != NULL ) fclose( unit1 ) ;
    if ( unit2 != NULL ) fclose( unit2 ) ;

    if ( mem1 != NULL )  free( mem1 ) ;
    if ( mem2 != NULL )  free( mem2 ) ;
    if ( mem3 != NULL )  free( mem3 ) ;
}

