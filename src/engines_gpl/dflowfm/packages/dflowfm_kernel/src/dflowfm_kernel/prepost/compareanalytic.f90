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

module m_compareanalytic

implicit none

private

public :: compareanalytic

contains

subroutine compareanalytic(s, x, mmax)

   use m_flowgeom
   use m_flow
   use m_set_col
   use m_inview

   integer :: mmax
   double precision :: s(0:mmax), x(0:mmax)
   double precision :: alf, dif, si
   integer :: n, i, ii

   call statisticsnewstep()

   call setcol(221)
   do n = 1, ndx

      if (.not. inview(xz(n), yz(n))) cycle

      i = 0
      do ii = 1, mmax - 1
         if (x(ii) <= xz(n) .and. xz(n) < x(ii + 1)) then
            i = ii
            exit
         end if
      end do
      !i = (xz(n) + 0.5*dxw) / dxw
      if (i > 2 .and. i < mmax - 1) then
         alf = (xz(n) - x(i)) / (x(i + 1) - x(i))
         if (alf < 0d0 .or. alf > 1d0) then
            si = 0
         else
            si = (1 - alf) * s(i) + alf * s(i + 1)
            dif = abs(s1(n) - si)
            call statisticsonemorepoint(dif)
            !   call ptabs(xz(n), bl(n) + 100d0*dif)
         end if
      end if
   end do
   call statisticsfinalise()

end subroutine compareanalytic

end module m_compareanalytic
