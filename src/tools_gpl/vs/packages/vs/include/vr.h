//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

#ifndef _VR_INCLUDED
# define _VR_INCLUDED
  extern BVoid           VR_release_all_variables  ( BVoid );
  extern BVoid           VR_release_variable       ( const BText );
  extern BInt4           VR_read_var_from_file     ( const BText, const BText,
                                                     const BText, BInt4 [][3],
                                                     BInt4 [][3] );
  extern BVoid           PR_variables_info_print   ( BVoid );
  extern BVoid           PR_print_variable_info    ( FILE * ,
                                                     struct St_var * ) ;
  extern BInt4           VR_chck_existnce_of_variables ( BText [] );
  extern struct St_var * VR_get_pointer_to_variable( const BText );
  extern struct St_var * VR__alloc_memory_for_variable ( BVoid );
  extern BVoid *         VR__alloc_memory_for_data ( BInt4 );

  extern BVoid           VR_add_variable_to_memo   ( struct St_var * );

  extern BInt4           VR_same_structure         ( struct St_var *,
                                                     struct St_var * ) ;

  extern NfDtp           VR_get_data_type          ( struct St_var * ) ;
#endif
