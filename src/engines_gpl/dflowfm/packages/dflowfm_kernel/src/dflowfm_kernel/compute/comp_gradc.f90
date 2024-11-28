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

!> computer cell-centered gradient from cell-centered data
!>   note: since we use wcx1 etc., it is assumed that normal components at the closed boundary are zero
module m_comp_gradc

implicit none

private

public :: comp_gradC

contains

   subroutine comp_gradC(val, gradx, grady)
      use m_flowgeom, only: ln, Lnx, Ndx, Dxi, wcx1, wcy1, wcx2, wcy2
      implicit none

      double precision, dimension(Ndx), intent(in) :: val !< cell-centered data
      double precision, dimension(Ndx), intent(out) :: gradx !< x-component of cell-centered gradient vector
      double precision, dimension(Ndx), intent(out) :: grady !< y-component of cell-centered gradient vector

      double precision :: DvalDn

      integer :: k1, k2, L

      gradx = 0d0
      grady = 0d0

      do L = 1, Lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         DvalDn = (val(k2) - val(k1)) * Dxi(L) ! outward positive for k1

         gradx(k1) = gradx(k1) + wcx1(L) * DvalDn
         grady(k1) = grady(k1) + wcy1(L) * DvalDn
         gradx(k2) = gradx(k2) + wcx2(L) * DvalDn
         grady(k2) = grady(k2) + wcy2(L) * DvalDn
      end do

      return
   end subroutine comp_gradC

end module m_comp_gradc
