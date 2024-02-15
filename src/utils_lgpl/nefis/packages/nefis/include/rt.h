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
// $Id$
// $HeadURL$
#ifndef __RT__
#define __RT__

#include "nef-tag.h"

extern BUInt8 *** retrieve_var;

#if DO_DEBUG
extern BInt4 DBG_nefis_tree ( BInt4  , BText  );
#endif

extern BInt4  convert_ieee        ( voidp *, charp *, BUInt8 , BInt4  , BText  ,
                                    BInt4  );
extern BInt4  GP_inquire_cel      ( BInt4  , BText   , BUInt4 *, BText *, BUInt8 *);
extern BInt4  GP_inquire_dat      ( BInt4  , BUInt8 *, BText  , BText  );
extern BInt4  GP_inquire_elm      ( BInt4  , BText   , BText   , BText   , BText,
                                    BText  , BUInt4 *, BUInt4 *, BUInt4 *, BUInt8 *);
extern BInt4  GP_inquire_grp_def  ( BInt4   , BText   , BText   ,
                                    BUInt4 *, BUInt4 *, BUInt4 *);
extern BUInt8 GP_read_file        ( BInt4  , BText  , BUInt8  , BUInt8  );
extern BUInt8 GP_read_file_pointers ( BInt4  , BText  , BUInt8  , BUInt8  , BUInt4);
extern BInt4  GP_variable_pointer ( BInt4  , BUInt8 *, BUInt4 , BUInt8 *);
extern BUInt8 GP_write_file       ( BInt4  , BText  , BUInt8  , BUInt8  );

extern BInt4 nefis_errcnt;
extern BInt4 nefis_errno;
extern BChar error_text[LENGTH_ERROR_MESSAGE+1];

typedef struct RETRIEVE {
  BInt4  hash_key            ;
  BChar  grp_name[MAX_NAME+1];
  BChar  elm_name[MAX_NAME+1];
  BUInt4 grp_num_dim         ;
  BUInt4 grp_dimens[MAX_DIM] ;
  BUInt4 grp_order [MAX_DIM] ;
  BUInt8 grp_pointer        ;
  BInt4  elm_dimens[MAX_DIM] ;
  BUInt4 elm_offset         ;
  BInt4  elm_num_dim         ;
  BInt4  elm_num_dimens      ;
  BChar  elm_type[MAX_TYPE+1];
  BInt4  elm_single_bytes    ;
  BUInt8 cel_num_bytes       ;
  BUInt8 write_bytes         ;
  struct RETRIEVE * left    ;
  struct RETRIEVE * right   ;
} retrieve;
retrieve * array_retrieve_ptrs[MAX_NEFIS_FILES];

/*
union dat_var {
  BChar   st [8*257];
  BUInt8  ptr[  257];
} pointer_buf;
*/
#endif
