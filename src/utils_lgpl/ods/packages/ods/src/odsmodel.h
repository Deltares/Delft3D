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
 *  odsmodel.h  -  ODS low level function prototypes
 *
 *  Eric Verschuur
 */
/*
 *  $Author: Markus $
 *  $Date: 15-05-03 13:17 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/odsmodel.h,v $
 */

/* filetype for tekascii2d toegevoegd */

/* make sure all types are known here */
#ifndef PORTABLE_H_INCLUDED
#include "portable.h"
#endif

#include "nefis.h"
#ifndef _NFTPS_INCLUDED
#include "btps.h"
#endif

/* include headerfiles containing function prototypes here */

#ifndef NOPROT

/* Provide macros for SUN-like systems: append an underscore if necessary
*/
#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#define phi_dim phi_dim_
#define phi_tme phi_tme_
#define phi_mat phi_mat_
#define phi_par phi_par_
#define phi_loc phi_loc_
#define phspdim phspdim_
#define phsppar phsppar_
#define phsploc phsploc_
#define phsptme phsptme_
#define phspmat phspmat_
#define hisdim  hisdim_
#define hispar  hispar_
#define hisloc  hisloc_
#define hismat  hismat_
#define histme  histme_
#define ods_tri_nef_map_dim ods_tri_nef_map_dim_
#define ods_tri_nef_map_tme ods_tri_nef_map_tme_
#define ods_tri_nef_map_par ods_tri_nef_map_par_
#define ods_tri_nef_map_loc ods_tri_nef_map_loc_
#define ods_tri_nef_map_mat ods_tri_nef_map_mat_
#define ods_mor_nef_com_dim ods_mor_nef_com_dim_
#define ods_mor_nef_com_tme ods_mor_nef_com_tme_
#define ods_mor_nef_com_loc ods_mor_nef_com_loc_
#define ods_mor_nef_com_par ods_mor_nef_com_par_
#define ods_mor_nef_com_mat ods_mor_nef_com_mat_
#define ods_delwaq_unf_cco  ods_delwaq_unf_cco_
#define ods_delwaq_unf_lga  ods_delwaq_unf_lga_
#endif

#ifdef PC
#  include "dbasewse.h"
#endif

#include "itrans.h"
#include "gregor.h"
#include "julian.h"
#include "dlwbin.h"
#include "dlwnef.h"
#include "dlwgrid.h"
#include "shyfem.h"
#include "waspro.h"
#include "mappix.h"
#include "trinint.h"
#include "m3hbuf.h"
#include "tekasc.h"
#include "tekasc2d.h"
#include "ods2nef.h"
#include "opnclose.h"
#include "readline.h"
#include "pharos.h"
#include "jspost.h"
#include "morcom.h"
#include "phidi2c.h"
#include "ods_bna.h"
#include "morbagr.h"
#include "samples.h"
#include "tridro.h"
#include "geotext.h"
#include "ods_gnf.h"

#endif

