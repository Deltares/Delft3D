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

module m_comp_middle_latitude

   implicit none

contains

   !> divide segment 1-3 (between latitudes y1 and y3) by y2,
   !> such that aspect ratios of the segments 1-2 and 2-3 are equal:
   !>   |y2-y1| / cos((y1+y2)/2) = |y3-y2| / cos((y2+y3)/2)
   subroutine comp_middle_latitude(y1_, y3_, y2, ierr)
      use precision, only: dp
      use m_sferic
      implicit none

      real(kind=dp), intent(in) :: y1_
      real(kind=dp), intent(in) :: y3_
      real(kind=dp), intent(out) :: y2
      integer, intent(out) :: ierr

      real(kind=dp) :: y1, y3
      real(kind=dp) :: y2min, y2max
      real(kind=dp) :: A, dAdy2

      integer :: iter

      real(kind=dp), parameter :: dtol = 1d-8
      real(kind=dp), parameter :: deps = 1d-16
      integer, parameter :: MAXITER = 1000

      y2 = 0.5d0 * (y1_ + y3_)

      if (jsferic == 0 .or. y1_ == y3_ .or. jamidlat == 0) then
         ierr = 0
         return
      end if

      y1 = dg2rd * y1_
      y3 = dg2rd * y3_

      y2max = 0.5d0 * pi - deps
      y2min = -y2max

      ierr = 1

      y2 = 0.5d0 * (y1 + y3)
      do iter = 1, MAXITER
         A = abs(y3 - y2) * cos(0.5d0 * (y1 + y2)) - abs(y2 - y1) * cos(0.5d0 * (y2 + y3))

         if (abs(A) < dtol) then
            ierr = 0
            exit
         end if

         dAdy2 = -sign(1d0, y3 - y2) * cos(0.5d0 * (y1 + y2)) - 0.5d0 * abs(y3 - y2) * sin(0.5d0 * (y1 + y2)) - &
                 sign(1d0, y3 - y2) * cos(0.5d0 * (y2 + y3)) + 0.5d0 * abs(y2 - y1) * sin(0.5d0 * (y2 + y3))
         y2 = y2 - A / dAdy2
         y2 = min(max(y2, y2min), y2max)
      end do

      if (ierr /= 0) then
!      error
         y2 = 0.5d0 * (y1_ + y3_)
      else
         y2 = y2 / dg2rd
      end if

      return
   end subroutine comp_middle_latitude

end module m_comp_middle_latitude
