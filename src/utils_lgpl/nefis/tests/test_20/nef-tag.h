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

#ifndef __NEFIS_TAG__
#define __NEFIS_TAG__

#include "btps.h"
#include "nef-def.h"

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
#endif
