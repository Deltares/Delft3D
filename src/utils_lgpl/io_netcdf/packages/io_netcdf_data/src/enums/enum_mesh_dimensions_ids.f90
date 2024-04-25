!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module m_enum_mesh_dimensions_ids

   implicit none

!mesh dimensions
   enum, bind(C)
      enumerator::mdim_start = 1
      enumerator::mdim_node               !< Dimension ID for nodes.
      enumerator::mdim_edge               !< Dimension ID for edges.
      enumerator::mdim_face               !< Dimension ID for faces.
      enumerator::mdim_1dbranches         !< Dimension ID for 1d network branches
      enumerator::mdim_1dnodes            !< Dimension ID for 1d network nodes
      enumerator::mdim_1dgeopoints        !< Dimension ID for 1d network geometry points
      enumerator::mdim_maxfacenodes       !< Dimension ID for max nr of nodes per face.
      enumerator::mdim_two                !< Dimension ID for two
      enumerator::mdim_layer              !< Dimension ID for layer centers.
      enumerator::mdim_interface          !< Dimension ID for layer interfaces.
      enumerator::mdim_idstring           !< Dimension ID for the string id
      enumerator::mdim_longnamestring     !< Dimension ID for the string longnames
      enumerator::mdim_1dedgenodes        !< Dimension ID for 1d sourcetargets arrays
      enumerator::mdim_node_original      !< Dimension ID for nodes (before merging).
      enumerator::mdim_end
   end enum

end module m_enum_mesh_dimensions_ids
