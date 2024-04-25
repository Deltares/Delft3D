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

#ifndef _SM_INCLUDED
#   define _SM_INCLUDED
    extern BVoid     SM_remove_file_info      ( BVoid ) ;
    extern VsGrpData SM_get_group_pointer     ( BVoid ) ;
    extern VsDefData SM_get_definition_pointer( BVoid ) ;
    extern VsCelData SM_get_cell_pointer      ( BVoid ) ;
    extern VsElmData SM_get_element_pointer   ( BVoid ) ;
    extern BVoid     SM_read_nefis_meta_data  ( BInt4 ) ;

    extern BVoid  GR_remove_groups_from_chain ( VsGrpData );
    extern BVoid  DF_remove_definition_branche( VsDefData );
    extern VsDefData DF_add_definition_to_tree( VsDefData, const BText );
    extern BVoid  CL_remove_cell_branche      ( VsCelData );
    extern BVoid  EL_remove_element_branche   ( struct St_elm * );

    extern BVoid            PR_groups_print         ( FILE * ) ;
    extern BVoid            PR_print_group_info     ( FILE *,
                                                      struct St_grp * );
#endif
