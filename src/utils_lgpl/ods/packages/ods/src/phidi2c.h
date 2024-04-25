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

#ifndef C2F_H_phidi2c_INCLUDED
#   define C2F_H_phidi2c_INCLUDED

#include "portable.h"

#ifndef FORTRAN_TRUE
#define FORTRAN_TRUE  1
#define FORTRAN_FALSE 0
#endif

/* Define the external names for the linker
*/
#define PHI_DIM               phi_dim
#define PHSPDIM               phspdim
#define PHI_PAR               phi_par
#define PHSPPAR               phsppar
#define PHSPLOC               phsploc
#define PHI_TME               phi_tme
#define PHSPTME               phsptme
#define PHI_MAT               phi_mat
#define PHSPMAT               phspmat
#define PHI_LOC               phi_loc

#if defined(USE_WINNT) || defined(WINNT)
#undef  PHI_DIM
#undef  PHSPDIM
#undef  PHI_PAR
#undef  PHSPPAR
#undef  PHSPLOC
#undef  PHI_TME
#undef  PHSPTME
#undef  PHI_MAT
#undef  PHSPMAT
#undef  PHI_LOC
#endif

#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#undef  PHI_DIM
#undef  PHSPDIM
#undef  PHI_PAR
#undef  PHSPPAR
#undef  PHSPLOC
#undef  PHI_TME
#undef  PHSPTME
#undef  PHI_MAT
#undef  PHSPMAT
#undef  PHI_LOC
#define PHSPDIM               phspdim_
#define PHSPPAR               phsppar_
#define PHSPLOC               phsploc_
#define PHSPTME               phsptme_
#define PHSPMAT               phspmat_
#define PHI_DIM               phi_dim_
#define PHI_PAR               phi_par_
#define PHI_TME               phi_tme_
#define PHI_MAT               phi_mat_
#define PHI_LOC               phi_loc_

/* Necessary for g77
#define PHI_DIM               phi_dim__
#define PHI_PAR               phi_par__
#define PHI_TME               phi_tme__
#define PHI_MAT               phi_mat__
#define PHI_LOC               phi_loc__
*/
#endif


extern TVoid ODSGetDimPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    dimtyp,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHI_DIM(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    dimtyp,
#if defined(IN_BETWEEN)
    TInt4      l_dimtyp,
#endif
    TInt4    * pardep,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      l_dimtyp,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetDimPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    dimtyp,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHSPDIM(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    dimtyp,
#if defined(IN_BETWEEN)
    TInt4      l_dimtyp,
#endif
    TInt4    * pardep,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      l_dimtyp,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetParPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    pardef,
    TInt4      lpardef,
    TInt4      npardef,
    TInt4      maxdef,
    TInt4      timdep,
    TInt4      locdep,
    TInt4      maxlst,
    TInt4      lang,
    TString    parlst,
    TInt4      lparlst,
    TInt4      nparlst,
    TString    paruni,
    TInt4      lparuni,
    TInt4      nparuni,
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHI_PAR(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    pardef,
#if defined(IN_BETWEEN)
    TInt4      lpardef,
#endif
    TInt4    * maxdef,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TInt4    * lang,
    TString    parlst,
#if defined(IN_BETWEEN)
    TInt4      lparlst,
#endif
    TString    paruni,
#if defined(IN_BETWEEN)
    TInt4      lparuni,
#endif
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      lpardef,
    TInt4      lparlst,
    TInt4      lparuni,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetParPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    pardef,
    TInt4      lpardef,
    TInt4      npardef,
    TInt4      maxdef,
    TInt4      timdep,
    TInt4      locdep,
    TInt4      maxlst,
    TInt4      lang,
    TString    parlst,
    TInt4      lparlst,
    TInt4      nparlst,
    TString    paruni,
    TInt4      lparuni,
    TInt4      nparuni,
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHSPPAR(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    pardef,
#if defined(IN_BETWEEN)
    TInt4      lpardef,
#endif
    TInt4    * maxdef,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TInt4    * lang,
    TString    parlst,
#if defined(IN_BETWEEN)
    TInt4      lparlst,
#endif
    TString    paruni,
#if defined(IN_BETWEEN)
    TInt4      lparuni,
#endif
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      lpardef,
    TInt4      lparlst,
    TInt4      lparuni,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetLocPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    locdef,
    TInt4      llocdef,
    TInt4      nlocdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      timdep,
    TString    loclst,
    TInt4      lloclst,
    TInt4      nloclst,
    TInt4    * loctyp,
    TInt4    * locnr,
    TInt4      maxlst,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHSPLOC(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    locdef,
#if defined(IN_BETWEEN)
    TInt4      llocdef,
#endif
    TInt4    * maxdef,
    TInt4    * pardep,
    TInt4    * timdep,
    TString    loclst,
#if defined(IN_BETWEEN)
    TInt4      lloclst,
#endif
    TInt4    * loctyp,
    TInt4    * locnr,
    TInt4    * maxlst,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      llocdef,
    TInt4      lloclst,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetTmePhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TReal8   * timdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      locdep,
    TInt4      maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHI_TME(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TReal8   * timdef,
    TInt4    * maxdef,
    TInt4    * pardep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetTmePhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TReal8   * timdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      locdep,
    TInt4      maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL PHSPTME(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TReal8   * timdef,
    TInt4    * maxdef,
    TInt4    * pardep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4      l_option
    );


extern TVoid ODSGetMatPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4     misval,
    TInt4      i3gl,
    TInt4      maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
    TInt4    * ibuffs,
    TReal4   * rbuffs
    );


extern TVoid FOR_CALL PHI_MAT(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TInt4    * parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4   * misval,
    TInt4    * i3gl,
    TInt4    * maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
#if defined(IN_BETWEEN)
    TInt4      l_option,
#endif
    TInt4    * ibuffs,
    TReal4   * rbuffs
#if ! defined(IN_BETWEEN)
    ,TInt4      lfname
    ,TInt4      l_option
#endif
    );


extern TVoid ODSGetMatPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4     misval,
    TInt4      i3gl,
    TInt4      maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
    TInt4    * ibuffs,
    TReal4   * rbuffs
    );


extern TVoid FOR_CALL PHSPMAT(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TInt4    * parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4   * misval,
    TInt4    * i3gl,
    TInt4    * maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
#if defined(IN_BETWEEN)
    TInt4      l_option,
#endif
    TInt4    * ibuffs,
    TReal4   * rbuffs
#if ! defined(IN_BETWEEN)
    ,TInt4     lfname
    ,TInt4     l_option
#endif
    );


extern TVoid ODSGetLocPhiHis(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    locdef,
    TInt4      llocdef,
    TInt4      nlocdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      maxlst,
    TString    loclst,
    TInt4      lloclst,
    TInt4      nloclst,
    TInt4    * loctyp,
    TInt4    * nrlst,
    TInt4    * locnr,
    TInt4    * ierror,
    TString    zbuffs,
    TInt4      lzbuffs,
    TInt4      nzbuffs,
    TString    option
    );


extern TVoid FOR_CALL PHI_LOC(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    locdef,
#if defined(IN_BETWEEN)
    TInt4      llocdef,
#endif
    TInt4    * maxdef,
    TInt4    * pardep,
    TInt4    * timdep,
    TInt4    * maxlst,
    TString    loclst,
#if defined(IN_BETWEEN)
    TInt4      lloclst,
#endif
    TInt4    * loctyp,
    TInt4    * nrlst,
    TInt4    * locnr,
    TInt4    * ierror,
    TString    zbuffs,
#if defined(IN_BETWEEN)
    TInt4      lzbuffs,
#endif
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      llocdef,
    TInt4      lloclst,
    TInt4      lzbuffs,
#endif
    TInt4      l_option
    );


#endif  /* C2F_H_phidi2c_INCLUDED */
