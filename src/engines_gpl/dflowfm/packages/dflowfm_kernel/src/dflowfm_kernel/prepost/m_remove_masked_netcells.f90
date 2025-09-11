!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_remove_masked_netcells

   implicit none

   private

   public :: remove_masked_netcells

   interface

      !> remove "dry"masked netcells (cellmask==1) from netcell administration
      !> typically used in combination with a drypoints file (samples or polygons)
      !> \see polygon_to_cellmask
      !> note: we do not want to alter the netnodes and netlinks and will therefore not change kn and nod%lin
      !> during the removal process, the netcells are renumbered, so that the remaining cells are numbered 1..numpnew
      !> this renumbering should also be applied to all quantities defined on netcells, such as bl, ba, xz, yz
      !> the bl array may not yet be loaded and therefore an optional argument is provided to specify whether bl should be updated
      module subroutine remove_masked_netcells(update_bl)
         implicit none
         logical, optional, intent(in) :: update_bl !< flag to specify whether bl should be updated
      end subroutine remove_masked_netcells

   end interface

end module m_remove_masked_netcells
