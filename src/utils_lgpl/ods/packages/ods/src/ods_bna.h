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
 * Include files and definitions
 */

void ODS_BNA_getdim( char *fname  ,TInt4 *itype  , char *dim    ,
                    TInt4 *pardep ,TInt4 *timdep ,TInt4 *locdep ,
                    TInt4 *nodim  ,TInt4 *ierror , char *option ) ;

void ODS_BNA_getpar( char *fname  ,TInt4 *itype  , char *pardef ,
                    TInt4 *maxdef ,TInt4 *timdep ,TInt4 *locdep ,
                    TInt4 *maxlst ,TInt4 *lang   , char *parlst ,
                     char *paruni ,TInt4 *partyp ,TInt4 *parcod ,
                    TInt4 *nrlst  ,TInt4 *ierror , char *option ) ;

void ODS_BNA_getloc( char *fname  ,TInt4 *itype  , char *locdef ,
                    TInt4 *maxdef ,TInt4 *pardep ,TInt4 *timdep ,
                    TInt4 *maxlst , char *loclst ,TInt4 *locnr  ,
                    TInt4 *nrlst  ,TInt4 *ierror , char *option ) ;

void ODS_BNA_gettme( char *fname  ,TInt4   *itype  , double *timdef ,
                    TInt4 *maxdef ,TInt4   *pardep ,TInt4   *locdep ,
                    TInt4 *maxlst , double *timlst ,TInt4   *timtyp ,
                    TInt4 *nrlst  ,TInt4   *ierror , char   *option ) ;

void ODS_BNA_getmat( char *fname  ,TInt4   *itype  ,TInt4  *parcod ,
                    TInt4 *loc    , double *tim    , float *misval ,
                    TInt4 *i3gl   ,TInt4   *maxdim , float *data   ,
                    TInt4 *ierror , char   *option                 ) ;

void ODS_BNA_getgrd( char *fname  ,TInt4 *itype  ,TInt4  *loc    ,
                    TInt4 *iadmin ,TInt4 *nocell ,TInt4  *igisty ,
                    TInt4 *ierror                                ) ;

