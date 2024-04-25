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
 *  utils.h   -  ODS prototype for utils.c
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/jsputil.h,v $
*/
/*
 *
 *
 */

#ifndef NOPROT

double AddToJulian( double juldate,TInt4 addyear,TInt4 addmonth,TInt4 addday,TInt4 addsec ) ;

void Blanks ( char * string,TInt4 len ) ;

void Errfin ( FILE * unit1, FILE * unit2, void * mem1, void * mem2, void * mem3 ) ;

#endif

