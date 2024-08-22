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

subroutine decaytracers()
   use m_transport
   use m_flowgeom
   use m_flow
   use m_flowtimes
   use timers

   implicit none

   double precision :: decaytime
   integer :: i, k

   integer(4) :: ithndl = 0
   
   if (timon) call timstrt("decaytracers", ithndl)

   do i = ITRA1, ITRAN
      decaytime = decaytimetracers(i - itra1 + 1)
      if (decaytime > 0d0) then
         do k = 1, ndkx
            constituents(i, k) = constituents(i, k) / (1d0 + dts / decaytime)
         end do
      end if
   end do

   if (timon) call timstop(ithndl)
end subroutine decaytracers
