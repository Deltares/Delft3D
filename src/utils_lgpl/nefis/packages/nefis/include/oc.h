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
#ifndef __OC__
#define __OC__

struct nefis_tag
{
  BInt4   array_retrieve_ptrs;
  BInt4   file_retrieve      ;
  BInt4   exist              ;
  BInt4   dat_fds            ;
  BInt4   def_fds            ;
  BInt4   daf_fds            ;
  BInt4   dat_neutral        ;
  BInt4   def_neutral        ;
  BInt4   daf_neutral        ;
  BInt4   one_file           ;
  File_Version file_version  ;
  union
  {
    BChar  str[SIZE_BINT8*(2+LHSHDT)];
    BUInt8 fds[           (2+LHSHDT)];
  } dat;
  union
  {
    BChar  str[SIZE_BINT8*(2+LHSHDF)];
    BUInt8 fds[           (2+LHSHDF)];
  } def;
  union
  {
    BChar  str[SIZE_BINT8*(2+LHSHDAF)];
    BUInt8 fds[           (2+LHSHDAF)];
  } daf;
  BChar   dat_name [FILENAME_MAX];
  BChar   def_name [FILENAME_MAX];
  BChar   daf_name [FILENAME_MAX];
} nefis[MAX_NEFIS_FILES];


extern BInt4 nefis_errcnt;
extern BInt4 nefis_errno;
extern BChar error_text[LENGTH_ERROR_MESSAGE+1];

extern BInt4  convert_ieee          ( voidp *, charp *, BUInt8  , BInt4  , BText  , BInt4  );
extern BInt4  GP_flush_hash         ( BInt4  , BInt4  );
extern BUInt8 GP_read_file          ( BInt4  , BText  , BUInt8  , BUInt8 );
extern BUInt8 GP_read_file_pointers ( BInt4  , BText  , BUInt8  , BUInt8 , BInt4 );
extern BUInt8 GP_write_file         ( BInt4  , BText  , BUInt8  , BUInt8 );
extern BUInt8 GP_write_file_pointers( BInt4  , BText  , BUInt8  , BUInt8 , BInt4);
#if DO_DEBUG
       BInt4 DBG_nefis_tree        ( BInt4  , BText  );
#endif
       BInt4 open_nefis_file       ( BText  , BChar  , BInt4 *, BInt4 *);
       BInt4 detect_version        ( BText  , File_Version * );
       BInt4 endianness            ( void   );
       BInt4 setConversionNewFile       ( BChar  , BText  , BUInt4);
       BInt4 setConversionExistingFile  ( BInt4, BChar  , BText  , BUInt4);

BUInt8 *** retrieve_var;
extern struct retrieve * array_retrieve_ptrs[MAX_NEFIS_FILES];
extern struct retrieve * free_retrieve_ptrs( struct retrieve * );
#endif
