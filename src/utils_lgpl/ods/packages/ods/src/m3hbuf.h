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
 *  m3hbuf.h  -  ODS function prototypes for TRISULA NEFIS files
 *
 *  Copyright (C)  Stichting Deltares, 2011-2024.
 *
 *   Pleun Koole
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/m3hbuf.h,v $
*/
/*
 */

#include "portable.h"

#ifndef M3HBUF_H_INCLUDED
#define M3HBUF_H_INCLUDED

void ODS_TRISULA_NEFIS_hismat_buffer(
    char *   fname,
    TInt4 *  parcod,
    float ** zbuffs,
    TInt4 *  ierror );

#endif  /* M3HBUF_H_INCLUDED */

#ifndef M3MBUF_H_INCLUDED
#define M3MBUF_H_INCLUDED

void ODS_TRISULA_NEFIS_mapmat_buffer(
    char *   fname,
    TInt4 *  parcod,
    TInt4 ** ibuffs,
    float ** zbuffs,
    TInt4 *  ierror );

#endif  /* M3MBUF_H_INCLUDED */

void ODSGetGrdTriNefMap (
    char *   fname,
    TInt4 *  ftype,
    TInt4 *  indloc,
    TInt4 *  indx,
    TInt4 *  nocell,
    TInt4 *  igisty,
    TInt4 *  ierror );

TVoid ODSGetGrdMorNefCom (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror);

TVoid ODSGetGrdPhidias (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror);

void ODS_MORSYS_NEFIS_commat_buffer(
                      char *   fname,
                      TInt4 *  parcod,
                      TInt4    itype,
                      TInt4 ** ibuffs,
                      float ** zbuffs,
                      TInt4 *  ierror );

void ODS_PHIDIAS_commat_buffer(
                      char *   fname,
                      TInt4 *  parcod,
                      TInt4    itype,
                      TInt4 ** ibuffs,
                      float ** zbuffs,
                      TInt4 *  ierror );


