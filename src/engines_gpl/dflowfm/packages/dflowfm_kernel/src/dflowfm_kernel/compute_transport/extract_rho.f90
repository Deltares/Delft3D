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

module m_extract_rho

   implicit none

   private

   public :: extract_rho

contains

   subroutine extract_rho()
      use m_transport
      use m_flow
      use m_sediment
      use m_transport
      use timers

      implicit none

      integer :: k

      integer(4) :: ithndl = 0

      if (timon) call timstrt("extract_rho", ithndl)

      do k = 1, Ndkx
         rho(k) = constituents(1, k)
         constituents(1, k) = sa1(k)
      end do

      if (timon) call timstop(ithndl)
      return
   end subroutine extract_rho

end module m_extract_rho
