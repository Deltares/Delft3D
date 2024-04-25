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
 *  jspost.h  -  ODS function prototypes for JSPOST files
 *
 *   Peter van den Bosch
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/jspost.h,v $
*/
/*
 *
 */

void FUNTYPE ODSGetLocJSP ( char *fname, char *locdef,TInt4 maxdef, char *loclst,
			   TInt4 *loctyp,TInt4 maxlst, TInt4 *nrlst,TInt4 *locnr,
			   TInt4 *ierror, char *option) ;

void FUNTYPE ODSGetParJSP ( char *fname, char *pardef,TInt4 maxdef, char *parlst,
                            char *paruni,TInt4 maxlst,TInt4 *nrlst,TInt4 *partyp,
                           TInt4 *parcod,TInt4 *ierror, char *option ) ;

void FUNTYPE ODSGetTmeJSP ( char *fname,  double *timdef,TInt4 maxdef,  double *timlst,
                   TInt4 maxlst, TInt4 *nrlst,TInt4 *timtyp, TInt4 *ierror,
		    char *option ) ;

void FUNTYPE ODSGetValJSP ( char *fname,  char *locin,    char *parin,  double *timin,
                   TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                    char *loc,    char *par,      double *tim,  float *values,
                   TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                   TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;

void FUNTYPE ODSGetDimJSP ( char *fname,TInt4 *ftype, char *dim,TInt4 pardep,
			   TInt4 timdep,TInt4 locdep,TInt4 *ndim,TInt4 *ierror,
			    char *option ) ;
void FUNTYPE ODSGetMatJSP ( char    *fname,TInt4    *ftype, TInt4   *parcod,
		            double  *tim  , TInt4   *loc  , float *misval  ,
		           TInt4 *maxdim  , float *values ,TInt4 *ierror   ) ;

void FUNTYPE JspOpen( char *fname, FILE **fd, FILE **fs,TInt4 *ierror);
