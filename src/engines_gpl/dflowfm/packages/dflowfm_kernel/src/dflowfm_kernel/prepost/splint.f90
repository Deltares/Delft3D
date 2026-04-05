!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR a PARTICULAR PURPOSE.  See the
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

module m_splint
   implicit none
   private
   public :: splint

contains

   subroutine splint(ya, y2a, n, x, y)
      use precision, only: dp

      integer, intent(in) :: n !< number of control points
      real(kind=dp), dimension(n), intent(in) :: ya !< control point values
      real(kind=dp), dimension(n), intent(in) :: y2a !< control point second order derivatives
      real(kind=dp), intent(in) :: x !< spline coordinate
      real(kind=dp), intent(out) :: y !< interpolated value at prescribed spline coordinate

      ! Adjusted for use with xa is only 0,1,2...n-1
      ! Search can be broken because the definition of xa is 0,1,

      real(kind=dp), parameter :: EPS = 0.00001_dp
      real(kind=dp), parameter :: SPLFAC = 1.0_dp
      real(kind=dp) :: a, b

      integer :: intx
      integer :: klo, khi

      intx = int(x)
      if (x - intx < EPS) then
         y = ya(intx + 1)
      else
         klo = intx + 1
         khi = klo + 1
         a = ((khi - 1) - x)
         b = (x - (klo - 1))
         y = a * ya(klo) + b * ya(khi) + SPLFAC * ((a**3 - a) * y2a(klo) + (b**3 - b) * y2a(khi)) / 6.0_dp
      end if
   end subroutine splint
end module m_splint
