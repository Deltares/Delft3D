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

module m_ug_mesh

   use m_enum_mesh_variables_ids
   use m_enum_mesh_dimensions_ids

   implicit none

!mesh type, it will expand with the commented componentes to accomodate composite meshes
   type t_ug_mesh
      integer::dimids(mdim_end) = -1
      integer::varids(mid_end) = -1
      !t_ug_mesh,allocatable::meshes(:)
      !t_composite:: compositeType
      !integer,allocatable::contacts_idx(:)
   end type t_ug_mesh

end module m_ug_mesh
