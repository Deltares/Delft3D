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
/* Prototypes for SHYFEM ODS routines - dummy for the moment */

TVoid ODSGetDimShyf( TString fname, TInt4   *ftype, TString dim,
                     TInt4   pardep, TInt4   timdep, TInt4   locdep,
                     TInt4   *ndim, TInt4   *ierror, TString option) ;

TVoid ODSGetParShyf ( TString fname, TInt4   *ftype, TString pardef,
                      TInt4   maxdef, TString parlst, TString paruni,
                      TInt4   maxlst, TInt4   *nrlst, TInt4   *partyp,
                      TInt4   *parcod, TInt4   *ierror) ;

TVoid ODSGetTmeShyf ( TString fname, TInt4   *ftype, TInt4   timdef,
                      TInt4   maxdef, TInt4   pardef, TInt4   locdep,
                      TReal8  *timlst, TInt4   maxlst, TInt4   *nrlst,
                      TInt4   *ierror, TString option) ;

TVoid ODSGetMatShyf(  TString fname, TInt4   itype, TInt4   parcod ,
                      TReal8  *tim , TInt4   *loc , TReal4  misval ,
                      TInt4   maxdim , TReal4  *data , TInt4   *ierror ) ;

TVoid ODSGetGrdShyf(  TString fname, TInt4   itype, TInt4   *indloc ,
                      TInt4   *indx , TInt4   *nocell , TInt4   *igisty ,
                      TInt4   *ierror ) ;
