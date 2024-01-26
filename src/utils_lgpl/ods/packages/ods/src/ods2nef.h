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
 *  ods2nef.h  -  ODS to NEFIS interface function prototypes
 *
 *  Eric Verschuur
 */
/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/ods2nef.h,v $
*/
/*
 *
 */


void OpenNefisDefDat ( char *fname,  BInt4 *datfds,  BInt4 *deffds,
                            TInt4 *ierror);

void CloseNefisDefDat ( char *fname,   BInt4 *datfds,  BInt4 *deffds,
                           TInt4 *ierror);

void OpenNefisFiles ( char *fname,  TInt4 ftype,    BInt4 *datfds,
                            BInt4 *deffds, char *grp_par, char *grp_res,
                           TInt4 *ierror);

void CloseNefisFiles ( char *fname,   BInt4 *datfds,  BInt4 *deffds,
                          TInt4 *ierror);

void GetNefisTme ( char *fname,    TInt4 *ftype,      double *timdef,
                      TInt4 *maxdef,    double *timlst, TInt4 *maxlst,
                      TInt4 *nrtim,    TInt4 *ierror,    char *option);

void GetNefisMat ( char *fname,  TInt4 *ftype,  TInt4 *parcod,
                         double *tim,  TInt4 *loc,     float *misval,
                        TInt4 *maxdim,  float *values,TInt4 *ierror,
                         char *option);

void GetNefisLoc ( char *fname,  TInt4 *ftype,   char *pardef,
                        TInt4 *maxdef,  char *parlst, TInt4 *maxlst,
                        TInt4 *nrlst,  TInt4 *locnr,  TInt4 *ierror,
                         char *option);

void GetNefisPar ( char *fname,  TInt4 *ftype,   char *pardef,
                        TInt4 *maxdef,  char *parlst,  char *paruni,
                        TInt4 *maxlst, TInt4 *nrlst,  TInt4 *partyp,
                        TInt4 *parcod, TInt4 *ierror,  char *option);

void GetNefisDim ( char *fname,  TInt4 *ftype,   char *dim,
                        TInt4 *pardep, TInt4 *timdep, TInt4 *locdep,
                        TInt4 *ndim,   TInt4 *ierror,  char *option);


