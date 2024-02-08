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
 *  tridro.h  -  ODS function prototypes for TRISULA drogues files
 *
 *  Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:52p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/tridro.h,v $
*/
/*
 *
 */

void TriDroGetTme ( TString fname,   TInt4  *ftype,  TReal8 *timdef,
                    TInt4   *maxdef, TInt4  *pardep, TInt4  *locdep,
                    TInt4   *maxlst, TReal8 *timlst, TInt4  *timtyp,
                    TInt4   *nrtim,  TInt4 *ierror,  TString option) ;

void TriDroGetMat ( TString fname,   TInt4  *ftype,   TInt4  *parcod,
                    TInt4   *loc,    TReal8 *tim,     TReal4 *misval,
                    TInt4   *i3gl,   TInt4  *maxdim,  TReal4 *values,
                    TInt4   *ierror, TString option) ;

void TriDroGetLoc ( TString fname,   TInt4 *ftype,   TString locdef,
                    TInt4 *maxdef,   TInt4 *pardep,  TInt4   *timdep,
                    TInt4 *maxlst,   TString loclst, TInt4   *loctyp,
                    TInt4 *locnr,    TInt4 *nrlst,   TInt4   *ierror,
                    TString option) ;

void TriDroGetPar ( TString fname,   TInt4 *ftype,   TString pardef,
                    TInt4   *maxdef, TInt4 *timdep,  TInt4   *locdep,
                    TInt4   *maxlst, TInt4 *lang,    TString parlst,
                    TString paruni,  TInt4 *partyp,  TInt4   *parcod,
                    TInt4   *nrlst,  TInt4 *ierror,  TString option) ;

void TriDroGetDim ( TString fname,    TInt4 *ftype,   TString dim,
                    TInt4   *pardep,  TInt4 *timdep,  TInt4   *locdep,
                    TInt4   *ndim,    TInt4 *ierror,  TString option ) ;
