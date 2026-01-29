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

module m_eqdint
   implicit none
   private
   public :: eqdint

contains

   subroutine eqdint(yh2, imax, tj, y2)
      use precision, only: dp

      integer, intent(in) :: imax
      real(kind=dp), intent(in) :: yh2(imax)
      real(kind=dp), intent(in) :: tj
      real(kind=dp), intent(out) :: y2

      integer :: j1, j2
      real(kind=dp) :: t1, t2

      j1 = int(tj) + 1
      j2 = j1 + 1
      t1 = tj - int(tj)
      t2 = 1 - t1
      y2 = t2 * yh2(j1) + t1 * yh2(j2)
   end subroutine eqdint
end module m_eqdint
