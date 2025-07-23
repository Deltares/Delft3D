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

module m_enum_contact_variables_ids

   implicit none

!contact variables
   enum, bind(C)
      enumerator::cid_start = 1
      enumerator::cid_contacttopo       !< Top-level variable ID for contact topology
      enumerator::cid_contactids        !< Variable ID for contacts ids
      enumerator::cid_contactlongnames  !< Variable ID for contacts longnames
      enumerator::cid_contacttype       !< Variable ID for contact types
      enumerator::cid_compositemesh     !< Top-level variable ID for composite mesh
      enumerator::cid_meshes
      enumerator::cid_mesh_contact
      enumerator::cid_end
   end enum

end module m_enum_contact_variables_ids
