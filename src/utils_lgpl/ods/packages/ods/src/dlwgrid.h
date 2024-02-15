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
 *  See c2for.txt for details
 */

#ifndef C2F_H_dlwgrid_INCLUDED
#   define C2F_H_dlwgrid_INCLUDED

#include "portable.h"

#ifndef FORTRAN_TRUE
#define FORTRAN_TRUE  1
#define FORTRAN_FALSE 0
#endif

/* Define the external names for the linker
*/
#define ODS_DELWAQ_UNF_LGA    ods_delwaq_unf_lga
#define ODS_DELWAQ_UNF_CCO    ods_delwaq_unf_cco

#if defined(USE_WINNT) || defined(WINNT)
#undef  ODS_DELWAQ_UNF_LGA
#undef  ODS_DELWAQ_UNF_CCO
#endif

#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#undef  ODS_DELWAQ_UNF_LGA
#undef  ODS_DELWAQ_UNF_CCO
#define ODS_DELWAQ_UNF_LGA    ods_delwaq_unf_lga_
#define ODS_DELWAQ_UNF_CCO    ods_delwaq_unf_cco_
/* Necessary for g77
#define ODS_DELWAQ_UNF_LGA    ods_delwaq_unf_lga__
#define ODS_DELWAQ_UNF_CCO    ods_delwaq_unf_cco__
*/
#endif

extern TVoid ODS_DELWAQ_UNF_lgrid(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4    * indloc,
    TInt4    * indx,
    TInt4    * nocell,
    TInt4    * igisty,
    TInt4    * ierror
    );


extern TVoid FOR_CALL ODS_DELWAQ_UNF_LGA(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TInt4    * indloc,
    TInt4    * indx,
    TInt4    * nocell,
    TInt4    * igisty,
    TInt4    * ierror
#if ! defined(IN_BETWEEN)
    ,TInt4   lfname
#endif
    );


extern TVoid ODS_DELWAQ_UNF_telmac(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      ipcode,
    TReal8   * time,
    TInt4    * indloc,
    TReal4     valmis,
    TInt4      maxdim,
    TReal4   * data,
    TInt4    * ierror
    );


extern TVoid FOR_CALL ODS_DELWAQ_UNF_CCO(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TInt4    * ipcode,
    TReal8   * time,
    TInt4    * indloc,
    TReal4   * valmis,
    TInt4    * maxdim,
    TReal4   * data,
    TInt4    * ierror
#if ! defined(IN_BETWEEN)
    ,TInt4   lfname
#endif
    );


#endif  /* C2F_H_dlwgrid_INCLUDED */
