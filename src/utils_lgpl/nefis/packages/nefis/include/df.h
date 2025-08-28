//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
// $Id$
// $HeadURL$
#ifndef __DF__
#define __DF__

#include "nef-tag.h"

extern  BInt4 nefis_errcnt;
extern  BInt4 nefis_errno;
extern  BChar error_text[LENGTH_ERROR_MESSAGE+1];

union element
{
  BChar  st [SIZE_ELM_BUF];
  BUInt4 in [SIZE_ELM_BUF/SIZE_BINT4];
  BUInt8 ptr[SIZE_ELM_BUF/SIZE_BINT8];
} elm_buf;

union cel
{
  BChar   * st ;
  BUInt4  * in ;
  BUInt8  * ptr;
};

union group
{
  BChar  st [SIZE_GRP_BUF];
  BUInt4 in [SIZE_GRP_BUF/SIZE_BINT4];
  BUInt8 ptr[SIZE_GRP_BUF/SIZE_BINT8];
} grp_buf;

union data
{
  BChar  st [ SIZE_DAT_BUF+(1+256)*SIZE_BINT8];
  BUInt4 in [(SIZE_DAT_BUF+(1+256)*SIZE_BINT8)/SIZE_BINT4];
  BUInt8 ptr[(SIZE_DAT_BUF+(1+256)*SIZE_BINT8)/SIZE_BINT8];
} dat_buf;

extern BInt4 nefis_errcnt;
extern BInt4 nefis_errno;

#if DO_DEBUG
extern BInt4 DBG_nefis_tree ( BInt4  , BText  );
#endif
extern BInt4 HS_check_ecg   ( BInt4  , BInt4   , BText   , BUInt8  , BUInt8 *,
                              BInt4  , BUInt8 *, BUInt4 *, BUInt4 *);
extern BInt4 HS_get_cont_cel( BInt4   , BUInt8 , BUInt8 *, BText  , BText *,
                              BUInt4 *, BUInt8 *);
extern BInt4 HS_get_cont_elm( BInt4   , BUInt8  , BUInt8 *, BText   , BText   ,
                              BText   , BText   , BText   , BUInt4 *, BUInt4 *,
                              BUInt4 *, BUInt8 *);
extern BInt4 HS_get_cont_grp( BInt4   , BUInt8  , BUInt8 *, BText  , BText  ,
                              BUInt4 *, BUInt4 *, BUInt4 *);
extern BInt4 convert_ieee   ( voidp *, charp *, BUInt8  , BInt4  , BText  ,
                              BInt4  );
extern BUInt8 GP_write_file ( BInt4, BText  , BUInt8  , BUInt8  );
extern BUInt8 GP_write_file_pointers( BInt4 , BText   , BUInt8  , BUInt8  , BInt4);

#endif
