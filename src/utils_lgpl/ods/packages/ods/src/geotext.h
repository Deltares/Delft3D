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
 *  geotext.h  -  ODS function prototypes for TEKAL ANNOTATION files
 *
 *  Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/geotext.h,v $
*/
/*
 *
 */

void ODSGetParTekAnno(
                     char   * fname,
                     TInt4  * ftype,
                     char   * pardef,
                     TInt4    maxdef,
                     TInt4    timdep,
                     TInt4    locdep,
                     TInt4    maxlst,
                     TInt4    lang,
                     char   * parlst,
                     char   * paruni,
                     TInt4  * partyp,
                     TInt4  * parcod,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetDimTekAnno (
                     char   * fname,
                     TInt4  * ftype,
                     char   * dimtyp,
                     TInt4    pardep,
                     TInt4    timdep,
                     TInt4    locdep,
                     TInt4  * ndim,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetTmeTekAnno (
                     char   * fname,
                     TInt4  * ftype,
                     double * timdef,
                     TInt4    maxdef,
                     TInt4    pardep,
                     TInt4    locdep,
                     double * timlst,
                     TInt4    maxlst,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetLocTekAnno (
                     char   * fname,
                     TInt4  * ftype,
                     char   * locdef,
                     TInt4    maxdef,
                     TInt4    pardep,
                     TInt4    timdep,
                     char   * loclst,
                     TInt4  * loctyp,
                     TInt4  * locnr,
                     TInt4    maxlst,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetMatTekAnno (
                     char   * fname,
                     TInt4  * ftype,
                     TInt4    parcod,
                     double * tim,
                     TInt4  * loc,
                     float    misval,
                     TInt4    maxdim,
                     float  * xdata,
                     TInt4  * ierror)  ;

