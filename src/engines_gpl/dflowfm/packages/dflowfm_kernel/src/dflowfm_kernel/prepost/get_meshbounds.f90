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

!> get mesh bounding box coordinates (useful for spherical, periodic coordinates)
!>   2D part of the mesh only
subroutine get_meshbounds(xboundmin, xboundmax)
   use precision, only: dp
   use network_data
   implicit none

   real(kind=dp), intent(out) :: xboundmin, xboundmax !< mesh bounding box x-coordinates

   real(kind=dp) :: x1, x2

   integer :: L, k1, k2

   xboundmin = huge(1d0)
   xboundmax = -huge(1d0)
   do L = 1, numL
      if (kn(3, L) == 2) then
         k1 = kn(1, L)
         k2 = kn(2, L)
         if (k1 > 0 .and. k2 > 0) then ! safety
            x1 = xk(kn(1, L))
            x2 = xk(kn(2, L))
            xboundmin = min(min(x1, x2), xboundmin)
            xboundmax = max(max(x1, x2), xboundmax)
         end if
      end if
   end do

   return
end subroutine get_meshbounds
