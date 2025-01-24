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
module m_wgs842bessel
   implicit none
contains
!> convert from WGS84 to Bessel
   subroutine wgs842bessel(phiwgs, lamwgs, phibes, lambes)
      use precision, only: dp

      real(kind=dp), intent(in) :: phiwgs, lamwgs
      real(kind=dp), intent(out) :: phibes, lambes

      real(kind=dp), dimension(2), parameter :: A1 = (/1.00011715371927d+00, -3.29086810736171d-06/)
      real(kind=dp), dimension(2), parameter :: A2 = (/1.25032982802497d-06, 1.00014669151113d+00/)
      real(kind=dp), dimension(2), parameter :: b = (/-5.12951110000526d-03, -1.83260002653070d-04/)

      phibes = A1(1) * phiwgs + A2(1) * lamwgs + b(1)
      lambes = A1(2) * phiwgs + A2(2) * lamwgs + b(2)

   end subroutine wgs842bessel
end module m_wgs842bessel
