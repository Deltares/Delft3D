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

#ifndef C2F_H_trinint_INCLUDED
#   define C2F_H_trinint_INCLUDED

#include "portable.h"

#ifndef FORTRAN_TRUE
#define FORTRAN_TRUE  1
#define FORTRAN_FALSE 0
#endif

/* Define the external names for the linker
*/
#define HISDIM                hisdim
#define HISTME                histme
#define HISPAR                hispar
#define HISLOC                hisloc
#define HISMAT                hismat
#define ODS_TRI_NEF_MAP_DIM   ods_tri_nef_map_dim
#define ODS_TRI_NEF_MAP_TME   ods_tri_nef_map_tme
#define ODS_TRI_NEF_MAP_PAR   ods_tri_nef_map_par
#define ODS_TRI_NEF_MAP_LOC   ods_tri_nef_map_loc
#define ODS_TRI_NEF_MAP_MAT   ods_tri_nef_map_mat

#if defined(USE_WINNT) || defined(WINNT)
#undef  HISDIM
#undef  HISTME
#undef  HISPAR
#undef  HISLOC
#undef  HISMAT
#undef  ODS_TRI_NEF_MAP_DIM
#undef  ODS_TRI_NEF_MAP_TME
#undef  ODS_TRI_NEF_MAP_PAR
#undef  ODS_TRI_NEF_MAP_LOC
#undef  ODS_TRI_NEF_MAP_MAT
#endif

#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#undef  HISDIM
#undef  HISTME
#undef  HISPAR
#undef  HISLOC
#undef  HISMAT
#undef  ODS_TRI_NEF_MAP_DIM
#undef  ODS_TRI_NEF_MAP_TME
#undef  ODS_TRI_NEF_MAP_PAR
#undef  ODS_TRI_NEF_MAP_LOC
#undef  ODS_TRI_NEF_MAP_MAT
#define HISDIM                hisdim_
#define HISTME                histme_
#define HISPAR                hispar_
#define HISLOC                hisloc_
#define HISMAT                hismat_
#define ODS_TRI_NEF_MAP_DIM   ods_tri_nef_map_dim_
#define ODS_TRI_NEF_MAP_TME   ods_tri_nef_map_tme_
#define ODS_TRI_NEF_MAP_PAR   ods_tri_nef_map_par_
#define ODS_TRI_NEF_MAP_LOC   ods_tri_nef_map_loc_
#define ODS_TRI_NEF_MAP_MAT   ods_tri_nef_map_mat_

/* Necessary for g77

#define ODS_TRI_NEF_MAP_DIM   ods_tri_nef_map_dim__
#define ODS_TRI_NEF_MAP_TME   ods_tri_nef_map_tme__
#define ODS_TRI_NEF_MAP_PAR   ods_tri_nef_map_par__
#define ODS_TRI_NEF_MAP_LOC   ods_tri_nef_map_loc__
#define ODS_TRI_NEF_MAP_MAT   ods_tri_nef_map_mat__
*/
#endif

extern TVoid ODS_TRISULA_NEFIS_hisdim(
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


extern TVoid FOR_CALL HISDIM(
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


extern TVoid ODS_TRISULA_NEFIS_histme(
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


extern TVoid FOR_CALL HISTME(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4   lfname,
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
#if !defined(IN_BETWEEN)
    TInt4     lfname,
#endif
    TInt4     l_option
    );


extern TVoid ODS_TRISULA_NEFIS_hispar(
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


extern TVoid FOR_CALL HISPAR(
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


extern TVoid ODS_TRISULA_NEFIS_hisloc(
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


extern TVoid FOR_CALL HISLOC(
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


extern TVoid ODS_TRISULA_NEFIS_hismat(
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
    TReal4   * zbuffs
    );


extern TVoid FOR_CALL HISMAT(
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
    TReal4   * zbuffs
#if ! defined(IN_BETWEEN)
   ,TInt4      lfname,
    TInt4      l_option
#endif
    );


extern TVoid ODS_TRISULA_NEFIS_MAPDIM(
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


extern TVoid FOR_CALL ODS_TRI_NEF_MAP_DIM(
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


extern TVoid ODS_TRISULA_NEFIS_MAPTME(
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


extern TVoid FOR_CALL ODS_TRI_NEF_MAP_TME(
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


extern TVoid ODS_TRISULA_NEFIS_MAPPAR(
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


extern TVoid FOR_CALL ODS_TRI_NEF_MAP_PAR(
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


extern TVoid ODS_TRISULA_NEFIS_MAPLOC(
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


extern TVoid FOR_CALL ODS_TRI_NEF_MAP_LOC(
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


extern TVoid ODS_TRISULA_NEFIS_MAPMAT(
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


extern TVoid FOR_CALL ODS_TRI_NEF_MAP_MAT(
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
   ,TInt4      lfname,
    TInt4      l_option
#endif
    );


#endif  /* C2F_H_trinint_INCLUDED */
