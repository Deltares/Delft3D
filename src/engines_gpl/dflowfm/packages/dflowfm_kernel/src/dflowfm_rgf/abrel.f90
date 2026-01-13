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

module m_abrel
   implicit none
   private
   public :: abrel

contains

   subroutine abrel(x1, y1, b1r, nfac)
      use precision, only: dp

      integer, intent(in) :: nfac
      real(kind=dp), intent(in) :: x1(nfac + 1)
      real(kind=dp), intent(in) :: y1(nfac + 1)
      real(kind=dp), intent(out) :: b1r(nfac + 1)

      integer :: j
      real(kind=dp) :: b1

      b1 = 0
      do j = 2, nfac + 1
         b1 = b1 + sqrt((x1(j) - x1(j - 1))**2 + (y1(j) - y1(j - 1))**2)
         b1r(j) = b1
      end do

      do j = 2, nfac + 1
         b1r(j) = b1r(j) / b1r(nfac + 1)
      end do
   end subroutine abrel
end module m_abrel
