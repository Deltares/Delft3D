!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

!  increase netcell admin. to include boundary nodes (safety)
module m_add_boundarynetcells

implicit none

private

public :: add_boundarynetcells

contains

   subroutine add_boundarynetcells()

      use network_data
      use m_flowgeom
      use gridoperations

      implicit none

      integer nump_store
      integer k

      !   store nump
      nump_store = nump

      !   need to store nump1d2d netcells in "increasenetcells", not only nump
      nump = nump1d2d
      call increasenetcells(Ndx, 1.0, .true.)
      do k = nump1d2d + 1, Ndx
         netcell(k)%n = 0 ! safety
      end do

      !   restore nump
      nump = nump_store

      return
   end subroutine add_boundarynetcells

end module m_add_boundarynetcells
