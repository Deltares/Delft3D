!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

module m_enum_mesh_variables_ids

   implicit none

!mesh variables
   enum, bind(C)
      enumerator::mid_start = 1
      !1d variables
      enumerator::mid_1dtopo                     !< The network used by this topology
      enumerator::mid_1dnodebranch               !< Variable ID for 1d branch indexes of each mesh point
      enumerator::mid_1dnodeoffset               !< Coordinate variable ID for mesh point offsets on branches
      enumerator::mid_1dedgebranch               !< Variable ID for 1d branch indexes of each mesh edge
      enumerator::mid_1dedgeoffset               !< Coordinate variable ID for mesh edge offsets on branches
      !2d variables
      enumerator::mid_meshtopo                    !< Top-level variable ID for mesh topology, collects all related variable names via attributes.
      enumerator::mid_edgenodes                   !< Variable ID for edge-to-node mapping table.
      enumerator::mid_facenodes                   !< Variable ID for face-to-node mapping table.
      enumerator::mid_edgefaces                   !< Variable ID for edge-to-face mapping table (optional, can be -1).
      enumerator::mid_faceedges                   !< Variable ID for face-to-edge mapping table (optional, can be -1).
      enumerator::mid_facelinks                   !< Variable ID for face-to-face mapping table (optional, can be -1).
      !mesh ids variables
      enumerator::mid_node_ids                    !< Variable ID for node ids (optional, can be -1).
      enumerator::mid_edge_ids                    !< Variable ID for edge ids (optional, can be -1).
      enumerator::mid_face_ids                    !< Variable ID for face ids (optional, can be -1).
      enumerator::mid_node_longnames              !< Variable ID for node longnames (optional, can be -1).
      enumerator::mid_edge_longnames              !< Variable ID for edge longnames (optional, can be -1).
      enumerator::mid_face_longnames              !< Variable ID for face longnames (optional, can be -1).
      !Coordinate variables
      enumerator::mid_nodex                       !< Coordinate variable ID for node x-coordinate.
      enumerator::mid_nodey                       !< Coordinate variable ID for node y-coordinate.
      enumerator::mid_nodez                       !< Coordinate variable ID for node z-coordinate.
      enumerator::mid_nodelon                     !< Coordinate variable ID for node longitude coordinate.
      enumerator::mid_nodelat                     !< Coordinate variable ID for node latitude coordinate.
      enumerator::mid_edgex                       !< Coordinate variable ID for edge x-coordinate.
      enumerator::mid_edgey                       !< Coordinate variable ID for edge y-coordinate.
      enumerator::mid_edgexbnd                    !< Coordinate variable ID for edge boundaries' x-coordinate.
      enumerator::mid_edgeybnd                    !< Coordinate variable ID for edge boundaries' y-coordinate.
      enumerator::mid_edgelon                     !< Coordinate variable ID for edge longitude coordinate.
      enumerator::mid_edgelat                     !< Coordinate variable ID for edge latitude coordinate.
      enumerator::mid_edgelonbnd                  !< Coordinate variable ID for edge boundaries' longitude coordinate.
      enumerator::mid_edgelatbnd                  !< Coordinate variable ID for edge boundaries' latitude coordinate.
      enumerator::mid_facex                       !< Coordinate variable ID for face x-coordinate.
      enumerator::mid_facey                       !< Coordinate variable ID for face y-coordinate.
      enumerator::mid_facexbnd                    !< Coordinate variable ID for face boundaries' x-coordinate.
      enumerator::mid_faceybnd                    !< Coordinate variable ID for face boundaries' y-coordinate.
      enumerator::mid_facelon                     !< Coordinate variable ID for face longitude coordinate.
      enumerator::mid_facelat                     !< Coordinate variable ID for face latitude coordinate.
      enumerator::mid_facelonbnd                  !< Coordinate variable ID for face boundaries' longitude coordinate.
      enumerator::mid_facelatbnd                  !< Coordinate variable ID for face boundaries' latitude coordinate.
      enumerator::mid_layerzs                     !< Coordinate variable ID for fixed z/sigma layer center vertical coordinate
      enumerator::mid_layerz                      !< Coordinate variable ID for fixed z layer center vertical coordinate
      enumerator::mid_layersigma                  !< Coordinate variable ID for fixed sigma layer center vertical coordinate
      enumerator::mid_interfacezs                 !< Coordinate variable ID for fixed z/sigma layer interface vertical coordinate
      enumerator::mid_interfacez                  !< Coordinate variable ID for fixed z layer interface vertical coordinate
      enumerator::mid_interfacesigma              !< Coordinate variable ID for fixed sigma layer interface vertical coordinate
      enumerator::mid_sigmazdepth                 !< Coordinate variable ID for transition depth from sigma above to z below
      enumerator::mid_node_ids_original           !< Variable storing the original ids
      enumerator::mid_node_mapping_original       !< Variable storing the ids - current nodes mapping
      enumerator::mid_end
   end enum

end module m_enum_mesh_variables_ids
